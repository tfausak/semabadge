module Semabadge.Server
  ( settings
  ) where

import qualified Data.ByteString as ByteString
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Semabadge.Handler as Handler
import qualified Semabadge.Unicode as Unicode
import qualified Semabadge.Version as Version

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
