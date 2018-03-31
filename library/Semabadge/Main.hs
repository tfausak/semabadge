module Semabadge.Main
  ( defaultMain
  ) where

import qualified Data.ByteString as ByteString
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Semabadge.Application as Application
import qualified Semabadge.Command as Command
import qualified Semabadge.Handler as Handler
import qualified Semabadge.Type.Config as Config
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
    (Application.application (Config.configToken config) perform)

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
    [ Unicode.fromUtf8 (Wai.requestMethod request)
    , " "
    , Unicode.fromUtf8 (Wai.rawPathInfo request)
    , Unicode.fromUtf8 (Wai.rawQueryString request)
    , " "
    , show (Http.statusCode status)
    ]

serverName :: ByteString.ByteString
serverName = Unicode.toUtf8 ("semabadge-" ++ Version.versionString)
