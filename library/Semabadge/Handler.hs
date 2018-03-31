module Semabadge.Handler
  ( exceptionHandler
  , defaultHandler
  , getHealthCheckHandler
  , getBranchBadgeHandler
  , getServerBadgeHandler
  ) where

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Semabadge.Badge as Badge
import qualified Semabadge.Semaphore as Semaphore
import qualified Semabadge.Type.Branch as Branch
import qualified Semabadge.Type.Project as Project
import qualified Semabadge.Type.Server as Server
import qualified Semabadge.Type.Token as Token
import qualified Semabadge.Unicode as Unicode

exceptionHandler :: Exception.SomeException -> Wai.Response
exceptionHandler _ = jsonResponse Http.internalServerError500 [] Aeson.Null

defaultHandler :: Applicative io => io Wai.Response
defaultHandler = pure (jsonResponse Http.notFound404 [] Aeson.Null)

getHealthCheckHandler :: Applicative io => io Wai.Response
getHealthCheckHandler = pure (jsonResponse Http.ok200 [] Aeson.Null)

getBranchBadgeHandler ::
     Monad io
  => Maybe Token.Token
  -> Semaphore.Perform io
  -> (String -> io ())
  -> Wai.Request
  -> Project.Project
  -> Branch.Branch
  -> io Wai.Response
getBranchBadgeHandler token perform warn request project branch = do
  result <- Semaphore.getBranchStatus perform project branch token
  case result of
    Left problem -> do
      warn problem
      pure (jsonResponse Http.internalServerError500 [] Aeson.Null)
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
  -> (String -> io ())
  -> Wai.Request
  -> Project.Project
  -> Server.Server
  -> io Wai.Response
getServerBadgeHandler token perform warn request project server = do
  result <- Semaphore.getServerStatus perform project server token
  case result of
    Left problem -> do
      warn problem
      pure (jsonResponse Http.internalServerError500 [] Aeson.Null)
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

requestParam :: String -> Wai.Request -> Maybe String
requestParam key request =
  case lookup (Unicode.toUtf8 key) (Wai.queryString request) of
    Nothing -> Nothing
    Just Nothing -> Nothing
    Just (Just value) -> Just (Unicode.fromUtf8 value)
