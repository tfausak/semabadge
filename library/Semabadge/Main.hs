module Semabadge.Main
  ( defaultMain
  ) where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Semabadge.Command as Command
import qualified Semabadge.Handler as Handler
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
  Warp.setOnExceptionResponse Handler.exceptionHandler .
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

serverName :: ByteString.ByteString
serverName = Unicode.toUtf8 ("semabadge-" ++ Version.versionString)

application :: Maybe Token.Token -> Semaphore.Perform IO -> Wai.Application
application token perform request respond = do
  response <-
    case (requestMethod request, requestPath request) of
      ("GET", ["health-check"]) -> Handler.getHealthCheckHandler
      ("GET", ["projects", project, "branches", branch]) ->
        Handler.getBranchBadgeHandler
          token
          perform
          request
          (Project.makeProject project)
          (Branch.makeBranch branch)
      ("GET", ["projects", project, "servers", server]) ->
        Handler.getServerBadgeHandler
          token
          perform
          request
          (Project.makeProject project)
          (Server.makeServer server)
      _ -> Handler.defaultHandler
  respond response

requestMethod :: Wai.Request -> String
requestMethod request = Unicode.fromUtf8 (Wai.requestMethod request)

requestPath :: Wai.Request -> [String]
requestPath request = map Text.unpack (Wai.pathInfo request)
