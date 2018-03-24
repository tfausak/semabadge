module Semabadge.Main
  ( defaultMain
  ) where

import qualified Control.Exception as Exception
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as Bytes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Middleware
import qualified Semabadge.Version as Version
import qualified System.IO as IO
import qualified Web.Scotty as Scotty

defaultMain :: IO ()
defaultMain = Scotty.scottyOpts options application

options :: Scotty.Options
options = Scotty.Options {Scotty.settings = settings, Scotty.verbose = 1}

settings :: Warp.Settings
settings =
  Warp.setOnExceptionResponse onExceptionResponse .
  Warp.setPort port . Warp.setServerName serverName $
  Warp.defaultSettings

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ =
  Wai.responseLBS
    Http.internalServerError500
    [(Http.hContentType, toUtf8 "application/json; charset=utf-8")]
    (Aeson.encode Aeson.Null)

port :: Warp.Port
port = 8080

serverName :: Bytes.ByteString
serverName = toUtf8 ("semabadge-" ++ Version.versionString)

toUtf8 :: String -> Bytes.ByteString
toUtf8 = Text.encodeUtf8 . Text.pack

application :: Scotty.ScottyM ()
application = do
  Scotty.middleware Middleware.logStdout
  Scotty.defaultHandler defaultHandler
  Scotty.get (Scotty.literal "/health-check") getHealthCheckHandler
  Scotty.notFound notFoundHandler

getHealthCheckHandler :: Scotty.ActionM ()
getHealthCheckHandler = do
  Scotty.status Http.ok200
  Scotty.json Aeson.Null

notFoundHandler :: Scotty.ActionM ()
notFoundHandler = do
  Scotty.status Http.notFound404
  Scotty.json Aeson.Null

defaultHandler :: LazyText.Text -> Scotty.ActionM ()
defaultHandler message = do
  MonadIO.liftIO (LazyText.hPutStrLn IO.stderr message)
  Scotty.status Http.internalServerError500
  Scotty.json Aeson.Null
