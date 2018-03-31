module Semabadge.Semaphore
  ( Perform
  , getSemaphore
  , semaphoreUrl
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Network.HTTP.Client as Client
import qualified Semabadge.Type.Token as Token

type Perform io
   = Client.Request -> io (Client.Response LazyByteString.ByteString)

getSemaphore ::
     (Aeson.FromJSON json, Monad io)
  => Perform io
  -> Maybe Token.Token
  -> String
  -> io (Either String json)
getSemaphore perform token path = do
  request <-
    case Client.parseRequest (semaphoreUrl token path) of
      Left message -> fail (show message)
      Right request -> pure request
  response <- perform request
  pure (Aeson.eitherDecode (Client.responseBody response))

semaphoreUrl :: Maybe Token.Token -> String -> String
semaphoreUrl maybeToken path =
  concat
    [ "https://semaphoreci.com/api/v1"
    , path
    , case maybeToken of
        Nothing -> ""
        Just token -> "?auth_token=" ++ Token.unwrapToken token
    ]
