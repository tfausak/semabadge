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
import qualified Graphics.Badge.Barrier as Barrier
import qualified Lens.Family as Lens
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

defaultMain :: IO ()
defaultMain = do
  config <- getConfig
  manager <- Client.newManager Client.tlsManagerSettings
  let perform request = Client.httpLbs request manager
  Warp.runSettings
    (settings (configHost config) (configPort config))
    (application (configToken config) perform)

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
  , configToken :: Token
  } deriving (Eq, Show)

options :: [Option]
options = [helpOption, hostOption, portOption, tokenOption, versionOption]

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

tokenOption :: Option
tokenOption =
  Console.Option
    []
    ["token"]
    (Console.ReqArg
       (\token config -> Right config {configToken = Token token})
       "TOKEN")
    "Semaphore authentication token"

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
    , configToken = Token "no-token-set"
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

type Perform io
   = Client.Request -> io (Client.Response LazyByteString.ByteString)

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

application :: Token -> Perform IO -> Wai.Application
application token perform request respond = do
  response <-
    case (requestMethod request, requestPath request) of
      ("GET", ["health-check"]) -> getHealthCheckHandler
      ("GET", ["projects", project, "branches", branch]) ->
        getBranchBadgeHandler
          token
          perform
          request
          (Project project)
          (Branch branch)
      ("GET", ["projects", project, "servers", server]) ->
        getServerBadgeHandler
          token
          perform
          request
          (Project project)
          (Server server)
      _ -> notFoundHandler
  respond response

getHealthCheckHandler :: Applicative io => io Wai.Response
getHealthCheckHandler = pure (jsonResponse Http.ok200 [] True)

notFoundHandler :: Applicative io => io Wai.Response
notFoundHandler = pure (jsonResponse Http.notFound404 [] Aeson.Null)

getBranchBadgeHandler ::
     Monad io
  => Token
  -> Perform io
  -> Wai.Request
  -> Project
  -> Branch
  -> io Wai.Response
getBranchBadgeHandler token perform request project branch = do
  result <- getBranchStatus perform project branch token
  case result of
    Nothing -> pure (jsonResponse Http.internalServerError500 [] Aeson.Null)
    Just branchStatus -> do
      let maybeLabel = requestParam "label" request
      pure (svgResponse Http.ok200 [] (badgeForBranch branchStatus maybeLabel))

getServerBadgeHandler ::
     Monad io
  => Token
  -> Perform io
  -> Wai.Request
  -> Project
  -> Server
  -> io Wai.Response
getServerBadgeHandler token perform request project server = do
  result <- getServerStatus perform project server token
  case result of
    Nothing -> pure (jsonResponse Http.internalServerError500 [] Aeson.Null)
    Just serverStatus -> do
      let maybeLabel = requestParam "label" request
      pure (svgResponse Http.ok200 [] (badgeForServer serverStatus maybeLabel))

getBranchStatus ::
     Monad io
  => Perform io
  -> Project
  -> Branch
  -> Token
  -> io (Maybe BranchStatus)
getBranchStatus perform (Project project) (Branch branch) token =
  getSemaphore
    perform
    token
    (concat ["/projects/", project, "/", branch, "/status"])

getServerStatus ::
     Monad io
  => Perform io
  -> Project
  -> Server
  -> Token
  -> io (Maybe ServerStatus)
getServerStatus perform (Project project) (Server server) token =
  getSemaphore
    perform
    token
    (concat ["/projects/", project, "/servers/", server, "/status"])

getSemaphore ::
     (Aeson.FromJSON json, Monad io)
  => Perform io
  -> Token
  -> String
  -> io (Maybe json)
getSemaphore perform token path = do
  request <-
    case Client.parseRequest (semaphoreUrl token path) of
      Left message -> fail (show message)
      Right request -> pure request
  response <- perform request
  pure (Aeson.decode (Client.responseBody response))

semaphoreUrl :: Token -> String -> String
semaphoreUrl (Token token) path =
  concat ["https://semaphoreci.com/api/v1", path, "?auth_token=", token]

newtype Project =
  Project String
  deriving (Eq, Show)

newtype Server =
  Server String
  deriving (Eq, Show)

newtype Branch =
  Branch String
  deriving (Eq, Show)

newtype Token =
  Token String
  deriving (Eq, Show)

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

badgeForServer :: ServerStatus -> Maybe String -> LazyByteString.ByteString
badgeForServer serverStatus maybeLabel =
  Barrier.renderBadge
    (Lens.set
       Barrier.right
       (badgeColor (serverStatusResult serverStatus))
       Barrier.flat)
    (maybe (serverStatusServerName serverStatus) Text.pack maybeLabel)
    (badgeRightLabel (serverStatusResult serverStatus))

badgeForBranch :: BranchStatus -> Maybe String -> LazyByteString.ByteString
badgeForBranch branchStatus maybeLabel =
  Barrier.renderBadge
    (Lens.set
       Barrier.right
       (badgeColor (branchStatusResult branchStatus))
       Barrier.flat)
    (maybe (branchStatusBranchName branchStatus) Text.pack maybeLabel)
    (badgeRightLabel (branchStatusResult branchStatus))

badgeColor :: Result -> Barrier.Color
badgeColor result =
  case result of
    ResultFailed -> Barrier.red
    ResultPassed -> Barrier.brightgreen
    ResultPending -> Barrier.gray

badgeRightLabel :: Result -> Text.Text
badgeRightLabel result =
  Text.pack
    (case result of
       ResultFailed -> "failed"
       ResultPassed -> "passed"
       ResultPending -> "pending")

data ServerStatus = ServerStatus
  { serverStatusResult :: Result
  , serverStatusServerName :: Text.Text
  } deriving (Eq, Generics.Generic, Show)

instance Aeson.FromJSON ServerStatus where
  parseJSON = Aeson.genericParseJSON (optionsFor "serverStatus")

data BranchStatus = BranchStatus
  { branchStatusResult :: Result
  , branchStatusBranchName :: Text.Text
  } deriving (Eq, Generics.Generic, Show)

instance Aeson.FromJSON BranchStatus where
  parseJSON = Aeson.genericParseJSON (optionsFor "branchStatus")

optionsFor :: String -> Aeson.Options
optionsFor prefix =
  Aeson.defaultOptions
    {Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . unsafeDropPrefix prefix}

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
jsonResponse status headers json =
  Wai.responseLBS
    status
    ((Http.hContentType, toUtf8 "application/json") : headers)
    (Aeson.encode json)

svgResponse ::
     Http.Status
  -> Http.ResponseHeaders
  -> LazyByteString.ByteString
  -> Wai.Response
svgResponse status headers svg =
  Wai.responseLBS
    status
    ((Http.hContentType, toUtf8 "image/svg+xml") : headers)
    svg

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
