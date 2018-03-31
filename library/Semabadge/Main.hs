module Semabadge.Main
  ( defaultMain
  ) where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson (decode, encode)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Semabadge.Badge as Badge
import qualified Semabadge.Type.Branch as Branch
import qualified Semabadge.Type.BranchStatus as BranchStatus
import qualified Semabadge.Type.Config as Config
import qualified Semabadge.Type.Project as Project
import qualified Semabadge.Type.Server as Server
import qualified Semabadge.Type.ServerStatus as ServerStatus
import qualified Semabadge.Type.Token as Token
import qualified Semabadge.Unicode as Unicode
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
    (settings (Config.configHost config) (Config.configPort config))
    (application (Config.configToken config) perform)

getConfig :: IO Config.Config
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
      Monad.when (Config.configShowHelp config) printHelpAndExit
      Monad.when (Config.configShowVersion config) printVersionAndExit
      pure config

options :: [Option]
options = [helpOption, hostOption, portOption, tokenOption, versionOption]

type Option = Console.OptDescr (Config.Config -> Either String Config.Config)

helpOption :: Option
helpOption =
  Console.Option
    ['h', '?']
    ["help"]
    (Console.NoArg (\config -> Right config {Config.configShowHelp = True}))
    "show the help and exit"

hostOption :: Option
hostOption =
  Console.Option
    []
    ["host"]
    (Console.ReqArg
       (\rawHost config ->
          Right config {Config.configHost = String.fromString rawHost})
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
            Right port -> Right config {Config.configPort = port})
       "PORT")
    "port number to bind"

tokenOption :: Option
tokenOption =
  Console.Option
    []
    ["token"]
    (Console.ReqArg
       (\token config ->
          Right config {Config.configToken = Just (Token.makeToken token)})
       "TOKEN")
    "Semaphore authentication token"

versionOption :: Option
versionOption =
  Console.Option
    []
    ["version"]
    (Console.NoArg (\config -> Right config {Config.configShowVersion = True}))
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

buildConfig ::
     [Config.Config -> Either String Config.Config]
  -> Either String Config.Config
buildConfig updates =
  Monad.foldM (\config update -> update config) Config.defaultConfig updates

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
    , Unicode.fromUtf8 (Wai.rawPathInfo request)
    , Unicode.fromUtf8 (Wai.rawQueryString request)
    , " "
    , show (Http.statusCode status)
    ]

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ = jsonResponse Http.internalServerError500 [] Aeson.Null

serverName :: ByteString.ByteString
serverName = Unicode.toUtf8 ("semabadge-" ++ Version.versionString)

application :: Maybe Token.Token -> Perform IO -> Wai.Application
application token perform request respond = do
  response <-
    case (requestMethod request, requestPath request) of
      ("GET", ["health-check"]) -> getHealthCheckHandler
      ("GET", ["projects", project, "branches", branch]) ->
        getBranchBadgeHandler
          token
          perform
          request
          (Project.makeProject project)
          (Branch.makeBranch branch)
      ("GET", ["projects", project, "servers", server]) ->
        getServerBadgeHandler
          token
          perform
          request
          (Project.makeProject project)
          (Server.makeServer server)
      _ -> notFoundHandler
  respond response

getHealthCheckHandler :: Applicative io => io Wai.Response
getHealthCheckHandler = pure (jsonResponse Http.ok200 [] True)

notFoundHandler :: Applicative io => io Wai.Response
notFoundHandler = pure (jsonResponse Http.notFound404 [] Aeson.Null)

getBranchBadgeHandler ::
     Monad io
  => Maybe Token.Token
  -> Perform io
  -> Wai.Request
  -> Project.Project
  -> Branch.Branch
  -> io Wai.Response
getBranchBadgeHandler token perform request project branch = do
  result <- getBranchStatus perform project branch token
  case result of
    Nothing -> pure (jsonResponse Http.internalServerError500 [] Aeson.Null)
    Just branchStatus -> do
      let maybeLabel = requestParam "label" request
      pure
        (svgResponse
           Http.ok200
           []
           (Badge.badgeForBranch branchStatus maybeLabel))

getServerBadgeHandler ::
     Monad io
  => Maybe Token.Token
  -> Perform io
  -> Wai.Request
  -> Project.Project
  -> Server.Server
  -> io Wai.Response
getServerBadgeHandler token perform request project server = do
  result <- getServerStatus perform project server token
  case result of
    Nothing -> pure (jsonResponse Http.internalServerError500 [] Aeson.Null)
    Just serverStatus -> do
      let maybeLabel = requestParam "label" request
      pure
        (svgResponse
           Http.ok200
           []
           (Badge.badgeForServer serverStatus maybeLabel))

getBranchStatus ::
     Monad io
  => Perform io
  -> Project.Project
  -> Branch.Branch
  -> Maybe Token.Token
  -> io (Maybe BranchStatus.BranchStatus)
getBranchStatus perform project branch token =
  getSemaphore
    perform
    token
    (concat
       [ "/projects/"
       , Project.unwrapProject project
       , "/"
       , Branch.unwrapBranch branch
       , "/status"
       ])

getServerStatus ::
     Monad io
  => Perform io
  -> Project.Project
  -> Server.Server
  -> Maybe Token.Token
  -> io (Maybe ServerStatus.ServerStatus)
getServerStatus perform project server token =
  getSemaphore
    perform
    token
    (concat
       [ "/projects/"
       , Project.unwrapProject project
       , "/servers/"
       , Server.unwrapServer server
       , "/status"
       ])

getSemaphore ::
     (Aeson.FromJSON json, Monad io)
  => Perform io
  -> Maybe Token.Token
  -> String
  -> io (Maybe json)
getSemaphore perform token path = do
  request <-
    case Client.parseRequest (semaphoreUrl token path) of
      Left message -> fail (show message)
      Right request -> pure request
  response <- perform request
  pure (Aeson.decode (Client.responseBody response))

semaphoreUrl :: Maybe Token.Token -> String -> String
semaphoreUrl maybeToken path =
  concat
    [ "https://semaphoreci.com/api/v1"
    , path
    , case maybeToken of
        Nothing -> ""
        Just token -> "?auth_token=" ++ Token.unwrapToken token
    ]

jsonResponse ::
     Aeson.ToJSON json
  => Http.Status
  -> Http.ResponseHeaders
  -> json
  -> Wai.Response
jsonResponse status headers json =
  Wai.responseLBS
    status
    ((Http.hContentType, jsonMime) : headers)
    (Aeson.encode json)

jsonMime :: ByteString.ByteString
jsonMime = Unicode.toUtf8 "application/json"

svgResponse ::
     Http.Status
  -> Http.ResponseHeaders
  -> LazyByteString.ByteString
  -> Wai.Response
svgResponse status headers svg =
  Wai.responseLBS status ((Http.hContentType, svgMime) : headers) svg

svgMime :: ByteString.ByteString
svgMime = Unicode.toUtf8 "image/svg+xml"

requestMethod :: Wai.Request -> String
requestMethod request = Unicode.fromUtf8 (Wai.requestMethod request)

requestPath :: Wai.Request -> [String]
requestPath request = map Text.unpack (Wai.pathInfo request)

requestParam :: String -> Wai.Request -> Maybe String
requestParam key request =
  case lookup (Unicode.toUtf8 key) (Wai.queryString request) of
    Nothing -> Nothing
    Just Nothing -> Nothing
    Just (Just value) -> Just (Unicode.fromUtf8 value)
