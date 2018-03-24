{-# LANGUAGE DeriveGeneric #-}

module Semabadge.Main
  ( defaultMain
  ) where

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
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

defaultMain :: IO ()
defaultMain = Warp.runSettings settings application

settings :: Warp.Settings
settings =
  Warp.setBeforeMainLoop beforeMainLoop .
  Warp.setLogger logger .
  Warp.setOnExceptionResponse onExceptionResponse .
  Warp.setPort port . Warp.setServerName serverName $
  Warp.defaultSettings

beforeMainLoop :: IO ()
beforeMainLoop = putStrLn "Starting server ..."

logger :: Wai.Request -> Http.Status -> Maybe Integer -> IO ()
logger request status _ =
  putStrLn
    (concat
       [ requestMethod request
       , " "
       , fromUtf8 (Wai.rawPathInfo request)
       , fromUtf8 (Wai.rawQueryString request)
       , " "
       , show (Http.statusCode status)
       ])

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ = jsonResponse Http.internalServerError500 [] Aeson.Null

port :: Warp.Port
port = 8080

serverName :: ByteString.ByteString
serverName = toUtf8 ("semabadge-" ++ Version.versionString)

application :: Wai.Application
application request respond = do
  response <-
    case (requestMethod request, requestPath request) of
      ("GET", ["health-check"]) -> getHealthCheckHandler
      ("GET", ["projects", project, "servers", server]) ->
        getServerHandler request project server
      _ -> notFoundHandler
  respond response

getHealthCheckHandler :: Applicative io => io Wai.Response
getHealthCheckHandler = pure (jsonResponse Http.ok200 [] True)

notFoundHandler :: Applicative io => io Wai.Response
notFoundHandler = pure (jsonResponse Http.notFound404 [] Aeson.Null)

getServerHandler :: Wai.Request -> String -> String -> IO Wai.Response
getServerHandler request project server =
  case lookup (toUtf8 "token") (Wai.queryString request) of
    Nothing -> pure (jsonResponse Http.badRequest400 [] Aeson.Null)
    Just Nothing -> pure (jsonResponse Http.badRequest400 [] Aeson.Null)
    Just (Just rawToken) -> do
      let token = fromUtf8 rawToken
          url =
            concat
              [ "https://semaphoreci.com/api/v1/projects/"
              , project
              , "/servers/"
              , server
              , "/status?auth_token="
              , token
              ]
      req <- Client.parseRequest url
      man <- Client.newTlsManager
      res <- Client.httpLbs req man
      case Aeson.eitherDecode (Client.responseBody res) of
        Left message ->
          pure (jsonResponse Http.internalServerError500 [] message)
        Right serverStatus ->
          pure
            (Wai.responseLBS
               Http.ok200
               [(Http.hContentType, toUtf8 "image/svg+xml")]
               (resultBadge (serverStatusResult serverStatus)))

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

resultBadge :: Result -> LazyByteString.ByteString
resultBadge result =
  Barrier.renderBadge
    (Lens.set Barrier.right (resultColor result) Barrier.flat)
    (Text.pack "deploy")
    (Text.pack (resultLabel result))

resultColor :: Result -> Barrier.Color
resultColor result =
  case result of
    ResultFailed -> Barrier.red
    ResultPassed -> Barrier.green
    ResultPending -> Barrier.gray

resultLabel :: Result -> String
resultLabel result =
  case result of
    ResultFailed -> "failed"
    ResultPassed -> "passed"
    ResultPending -> "pending"

newtype ServerStatus = ServerStatus
  { serverStatusResult :: Result
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
