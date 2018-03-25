{-# LANGUAGE DeriveGeneric #-}

module Semabadge.Main
  ( defaultMain
  ) where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson (decode, encode)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified GHC.Generics as Generics
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Semabadge.Version as Version
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read
import qualified Text.XML.Light as Xml

defaultMain :: IO ()
defaultMain = do
  config <- getConfig
  manager <- Client.newManager Client.tlsManagerSettings
  let perform request = Client.httpLbs request manager
  Warp.runSettings
    (settings (configHost config) (configPort config))
    (application perform)

getConfig :: IO Config
getConfig = do
  args <- Environment.getArgs
  let (updates, unexpecteds, unknowns, errors) =
        Console.getOpt' Console.Permute options args
  Monad.unless (null errors) (printErrorsAndExit errors)
  printUnknowns unknowns
  printUnexpecteds unexpecteds
  case buildConfig updates of
    Left problem -> do
      IO.hPutStrLn IO.stderr ("ERROR: invalid config: " ++ problem)
      Exit.exitFailure
    Right config -> do
      Monad.when (configShowHelp config) printHelpAndExit
      Monad.when (configShowVersion config) printVersionAndExit
      pure config

data Config = Config
  { configHost :: Warp.HostPreference
  , configPort :: Warp.Port
  , configShowHelp :: Bool
  , configShowVersion :: Bool
  } deriving (Eq, Show)

options :: [Option]
options = [helpOption, hostOption, portOption, versionOption]

type Option = Console.OptDescr (Config -> Either String Config)

helpOption :: Option
helpOption =
  Console.Option
    ['h', '?']
    ["help"]
    (Console.NoArg (\config -> Right config {configShowHelp = True}))
    "show the help and exit"

hostOption :: Option
hostOption =
  Console.Option
    []
    ["host"]
    (Console.ReqArg
       (\rawHost config ->
          Right config {configHost = String.fromString rawHost})
       "HOST")
    "host to bind"

portOption :: Option
portOption =
  Console.Option
    []
    ["port"]
    (Console.ReqArg
       (\rawPort config ->
          case Read.readEither rawPort of
            Left message ->
              Left
                (concat ["invalid port: ", show rawPort, " (", message, ")"])
            Right port -> Right config {configPort = port})
       "PORT")
    "port number to bind"

versionOption :: Option
versionOption =
  Console.Option
    []
    ["version"]
    (Console.NoArg (\config -> Right config {configShowVersion = True}))
    "show the version number and exit"

printErrorsAndExit :: [String] -> IO ()
printErrorsAndExit errors = do
  mapM_ printError errors
  Exit.exitFailure

printError :: String -> IO ()
printError error_ = IO.hPutStr IO.stderr ("ERROR: " ++ error_)

printUnknowns :: [String] -> IO ()
printUnknowns unknowns = mapM_ printUnknown unknowns

printUnknown :: String -> IO ()
printUnknown unknown =
  IO.hPutStrLn IO.stderr (concat ["WARNING: unknown option `", unknown, "'"])

printUnexpecteds :: [String] -> IO ()
printUnexpecteds unexpecteds = mapM_ printUnexpected unexpecteds

printUnexpected :: String -> IO ()
printUnexpected unexpected =
  IO.hPutStrLn
    IO.stderr
    (concat ["WARNING: unexpected argument `", unexpected, "'"])

buildConfig :: [Config -> Either String Config] -> Either String Config
buildConfig updates =
  Monad.foldM (\config update -> update config) defaultConfig updates

defaultConfig :: Config
defaultConfig =
  Config
    { configHost = String.fromString "127.0.0.1"
    , configPort = 8080
    , configShowHelp = False
    , configShowVersion = False
    }

printHelpAndExit :: IO ()
printHelpAndExit = do
  IO.hPutStr
    IO.stderr
    (Console.usageInfo
       (unwords ["semabadge", "version", Version.versionString])
       options)
  Exit.exitFailure

printVersionAndExit :: IO ()
printVersionAndExit = do
  IO.hPutStrLn IO.stderr Version.versionString
  Exit.exitFailure

type Perform m
   = Client.Request -> m (Client.Response LazyByteString.ByteString)

settings :: Warp.HostPreference -> Warp.Port -> Warp.Settings
settings host port =
  Warp.setBeforeMainLoop (beforeMainLoop host port) .
  Warp.setHost host .
  Warp.setLogger logger .
  Warp.setOnExceptionResponse onExceptionResponse .
  Warp.setPort port . Warp.setServerName serverName $
  Warp.defaultSettings

beforeMainLoop :: Warp.HostPreference -> Warp.Port -> IO ()
beforeMainLoop host port =
  putStrLn (unwords ["Listening on", show host, "port", show port, "..."])

logger :: Wai.Request -> Http.Status -> Maybe Integer -> IO ()
logger request status _ = putStrLn (requestLog request status)

requestLog :: Wai.Request -> Http.Status -> String
requestLog request status =
  concat
    [ requestMethod request
    , " "
    , fromUtf8 (Wai.rawPathInfo request)
    , fromUtf8 (Wai.rawQueryString request)
    , " "
    , show (Http.statusCode status)
    ]

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ = jsonResponse Http.internalServerError500 [] Aeson.Null

serverName :: ByteString.ByteString
serverName = toUtf8 ("semabadge-" ++ Version.versionString)

application :: Perform IO -> Wai.Application
application perform request respond = do
  response <-
    case (requestMethod request, requestPath request) of
      ("GET", ["health-check"]) -> getHealthCheckHandler
      ("GET", ["projects", project, "servers", server]) ->
        getBadgeHandler perform request project server
      _ -> notFoundHandler
  respond response

getHealthCheckHandler :: Applicative io => io Wai.Response
getHealthCheckHandler = pure (jsonResponse Http.ok200 [] True)

notFoundHandler :: Applicative io => io Wai.Response
notFoundHandler = pure (jsonResponse Http.notFound404 [] Aeson.Null)

getBadgeHandler ::
     Monad m => Perform m -> Wai.Request -> String -> String -> m Wai.Response
getBadgeHandler perform request project server =
  case requestParam "token" request of
    Nothing -> pure (jsonResponse Http.badRequest400 [] Aeson.Null)
    Just token -> do
      result <-
        getServerStatus perform (Project project) (Server server) (Token token)
      case result of
        Nothing ->
          pure (jsonResponse Http.internalServerError500 [] Aeson.Null)
        Just serverStatus ->
          pure
            (Wai.responseLBS
               Http.ok200
               [(Http.hContentType, toUtf8 "image/svg+xml")]
               (badgeFor serverStatus))

getServerStatus ::
     Monad m
  => Perform m
  -> Project
  -> Server
  -> Token
  -> m (Maybe ServerStatus)
getServerStatus perform project server token = do
  request <-
    case Client.parseRequest (semaphoreUrl project server token) of
      Left message -> fail (show message)
      Right request -> pure request
  response <- perform request
  pure (Aeson.decode (Client.responseBody response))

semaphoreUrl :: Project -> Server -> Token -> String
semaphoreUrl project server token =
  concat
    [ "https://semaphoreci.com/api/v1/projects/"
    , unwrapProject project
    , "/servers/"
    , unwrapServer server
    , "/status?auth_token="
    , unwrapToken token
    ]

newtype Project =
  Project String
  deriving (Eq, Show)

unwrapProject :: Project -> String
unwrapProject (Project project) = project

newtype Server =
  Server String
  deriving (Eq, Show)

unwrapServer :: Server -> String
unwrapServer (Server server) = server

newtype Token =
  Token String
  deriving (Eq, Show)

unwrapToken :: Token -> String
unwrapToken (Token token) = token

data Result
  = ResultFailed
  | ResultPassed
  | ResultPending
  deriving (Eq, Show)

instance Aeson.FromJSON Result where
  parseJSON =
    Aeson.withText
      "Result"
      (\text ->
         case Text.unpack text of
           "failed" -> pure ResultFailed
           "passed" -> pure ResultPassed
           "pending" -> pure ResultPending
           _ -> mempty)

badgeFor :: ServerStatus -> LazyByteString.ByteString
badgeFor serverStatus =
  let color = badgeColor serverStatus
      leftLabel = badgeLeftLabel serverStatus
      rightLabel = badgeRightLabel serverStatus
   in LazyByteString.fromStrict
        (toUtf8
           (Xml.ppContent
              (xmlElem
                 "svg"
                 [ ("xmlns", "http://www.w3.org/2000/svg")
                 , ("width", "98")
                 , ("height", "20")
                 ]
                 [ xmlElem
                     "linearGradient"
                     [("id", "smooth"), ("x2", "0"), ("y2", "100%")]
                     [ xmlElem
                         "stop"
                         [ ("offset", "0")
                         , ("stop-color", "#bbb")
                         , ("stop-opacity", ".1")
                         ]
                         []
                     , xmlElem
                         "stop"
                         [("offset", "1"), ("stop-opacity", ".1")]
                         []
                     ]
                 , xmlElem
                     "mask"
                     [("id", "round")]
                     [ xmlElem
                         "rect"
                         [ ("width", "98")
                         , ("height", "20")
                         , ("rx", "3")
                         , ("fill", "#fff")
                         ]
                         []
                     ]
                 , xmlElem
                     "g"
                     [("mask", "url(#round)")]
                     [ xmlElem
                         "rect"
                         [("width", "48"), ("height", "20"), ("fill", "#555")]
                         []
                     , xmlElem
                         "rect"
                         [ ("x", "48")
                         , ("width", "50")
                         , ("height", "20")
                         , ("fill", color)
                         ]
                         []
                     , xmlElem
                         "rect"
                         [ ("width", "98")
                         , ("height", "20")
                         , ("fill", "url(#smooth)")
                         ]
                         []
                     ]
                 , xmlElem
                     "g"
                     [ ("fill", "#fff")
                     , ("text-anchor", "middle")
                     , ( "font-family"
                       , "'DejaVu Sans', 'Verdana', 'Geneva', sans-serif")
                     , ("font-size", "11")
                     ]
                     [ xmlElem
                         "text"
                         [ ("x", "25")
                         , ("y", "15")
                         , ("fill", "#010101")
                         , ("fill-opacity", ".3")
                         ]
                         [xmlText leftLabel]
                     , xmlElem
                         "text"
                         [("x", "25"), ("y", "14")]
                         [xmlText leftLabel]
                     , xmlElem
                         "text"
                         [ ("x", "72")
                         , ("y", "15")
                         , ("fill", "#010101")
                         , ("fill-opacity", ".3")
                         ]
                         [xmlText rightLabel]
                     , xmlElem
                         "text"
                         [("x", "72"), ("y", "14")]
                         [xmlText rightLabel]
                     ]
                 ])))

xmlElem :: String -> [(String, String)] -> [Xml.Content] -> Xml.Content
xmlElem name attributes children =
  Xml.Elem
    (Xml.Element
       (xmlName name)
       (map (uncurry xmlAttr) attributes)
       children
       Nothing)

xmlAttr :: String -> String -> Xml.Attr
xmlAttr key value = Xml.Attr (xmlName key) value

xmlName :: String -> Xml.QName
xmlName string = Xml.QName string Nothing Nothing

xmlText :: String -> Xml.Content
xmlText text = Xml.Text (Xml.CData Xml.CDataText text Nothing)

badgeColor :: ServerStatus -> String
badgeColor serverStatus =
  case serverStatusResult serverStatus of
    ResultFailed -> "#e05d44"
    ResultPassed -> "#4c1"
    ResultPending -> "#9f9f9f"

badgeLeftLabel :: ServerStatus -> String
badgeLeftLabel serverStatus = Text.unpack (serverStatusServerName serverStatus)

badgeRightLabel :: ServerStatus -> String
badgeRightLabel serverStatus =
  case serverStatusResult serverStatus of
    ResultFailed -> "failed"
    ResultPassed -> "passed"
    ResultPending -> "pending"

data ServerStatus = ServerStatus
  { serverStatusResult :: Result
  , serverStatusServerName :: Text.Text
  } deriving (Eq, Generics.Generic, Show)

instance Aeson.FromJSON ServerStatus where
  parseJSON =
    Aeson.genericParseJSON
      Aeson.defaultOptions
        { Aeson.fieldLabelModifier =
            Aeson.camelTo2 '_' . unsafeDropPrefix "serverStatus"
        }

unsafeDropPrefix :: (Eq a, Show a) => [a] -> [a] -> [a]
unsafeDropPrefix prefix list =
  Maybe.fromMaybe
    (error (unwords [show prefix, "is not a prefix of", show list]))
    (dropPrefix prefix list)

dropPrefix :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefix prefix list =
  case prefix of
    [] -> Just list
    ph:pt ->
      case list of
        [] -> Nothing
        lh:lt ->
          if lh == ph
            then dropPrefix pt lt
            else Nothing

toUtf8 :: String -> ByteString.ByteString
toUtf8 string = Text.encodeUtf8 (Text.pack string)

fromUtf8 :: ByteString.ByteString -> String
fromUtf8 byteString = Text.unpack (Text.decodeUtf8 byteString)

jsonResponse ::
     Aeson.ToJSON json
  => Http.Status
  -> Http.ResponseHeaders
  -> json
  -> Wai.Response
jsonResponse status headers body =
  Wai.responseLBS
    status
    ((Http.hContentType, toUtf8 "application/json") : headers)
    (Aeson.encode body)

requestMethod :: Wai.Request -> String
requestMethod request = fromUtf8 (Wai.requestMethod request)

requestPath :: Wai.Request -> [String]
requestPath request = map Text.unpack (Wai.pathInfo request)

requestParam :: String -> Wai.Request -> Maybe String
requestParam key request =
  case lookup (toUtf8 key) (Wai.queryString request) of
    Nothing -> Nothing
    Just Nothing -> Nothing
    Just (Just value) -> Just (fromUtf8 value)
