module Semabadge.Main
  ( defaultMain
  ) where

import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.Wai.Handler.Warp as Warp
import qualified Semabadge.Application as Application
import qualified Semabadge.Command as Command
import qualified Semabadge.Server as Server
import qualified Semabadge.Type.Config as Config
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO

defaultMain :: IO ()
defaultMain = do
  config <- getConfig
  manager <- Client.newManager Client.tlsManagerSettings
  let perform request = Client.httpLbs request manager
  Warp.runSettings
    (Server.settings (Config.configHost config) (Config.configPort config))
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
