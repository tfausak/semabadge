module Semabadge.Main
  ( defaultMain
  ) where

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson (encode)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Semabadge.Badge as Badge
import qualified Semabadge.Command as Command
import qualified Semabadge.Semaphore as Semaphore
import qualified Semabadge.Type.Branch as Branch
import qualified Semabadge.Type.Config as Config
import qualified Semabadge.Type.Project as Project
import qualified Semabadge.Type.Server as Server
import qualified Semabadge.Type.Token as Token
import qualified Semabadge.Unicode as Unicode
import qualified Semabadge.Version as Version
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

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
  case Command.getConfigWith args of
    Left problem -> do
      IO.hPutStrLn IO.stderr problem
      Exit.exitFailure
    Right (config, warnings) -> do
      mapM_ (IO.hPutStrLn IO.stderr) warnings
      pure config

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

application :: Maybe Token.Token -> Semaphore.Perform IO -> Wai.Application
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
  -> Semaphore.Perform io
  -> Wai.Request
  -> Project.Project
  -> Branch.Branch
  -> io Wai.Response
getBranchBadgeHandler token perform request project branch = do
  result <- Semaphore.getBranchStatus perform project branch token
  case result of
    Left _ -> pure (jsonResponse Http.internalServerError500 [] Aeson.Null)
    Right branchStatus -> do
      let maybeLabel = requestParam "label" request
      pure
        (svgResponse
           Http.ok200
           []
           (Badge.badgeForBranch branchStatus maybeLabel))

getServerBadgeHandler ::
     Monad io
  => Maybe Token.Token
  -> Semaphore.Perform io
  -> Wai.Request
  -> Project.Project
  -> Server.Server
  -> io Wai.Response
getServerBadgeHandler token perform request project server = do
  result <- Semaphore.getServerStatus perform project server token
  case result of
    Left _ -> pure (jsonResponse Http.internalServerError500 [] Aeson.Null)
    Right serverStatus -> do
      let maybeLabel = requestParam "label" request
      pure
        (svgResponse
           Http.ok200
           []
           (Badge.badgeForServer serverStatus maybeLabel))

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
