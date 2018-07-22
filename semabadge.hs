{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Graphics.Badge.Barrier as Barrier
import qualified Lens.Micro as Lens
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Http
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Middleware
import qualified Network.Wai.Middleware.RequestLogger as Middleware
import qualified System.Envy as Envy
import qualified System.IO as IO
import qualified Web.Scotty as Scotty

main :: IO ()
main = do
  config <- getConfig
  Scotty.scotty (configPort config) $ do
    Scotty.middleware Middleware.logStdout
    Scotty.middleware $ Middleware.gzip Middleware.def
    Scotty.defaultHandler defaultHandler
    Scotty.get "/ping" getPingHandler
    Scotty.get "/projects/:project/branches/:branch" $ getBranchHandler config
    Scotty.get "/projects/:project/servers/:server" $ getServerHandler config
    Scotty.notFound notFoundHandler

getConfig :: IO Config
getConfig = do
  result <- Envy.decodeEnv
  case result of
    Left problem -> fail problem
    Right config -> pure config

defaultHandler :: LazyText.Text -> Scotty.ActionM ()
defaultHandler problem = do
  MonadIO.liftIO $ LazyText.hPutStrLn IO.stderr problem
  Scotty.status Http.internalServerError500
  Scotty.json Aeson.Null

getPingHandler :: Scotty.ActionM ()
getPingHandler = Scotty.json Aeson.Null

getBranchHandler :: Config -> Scotty.ActionM ()
getBranchHandler config = do
  project <- Scotty.param "project"
  branch <- Scotty.param "branch"
  let path = concat ["/projects/", project, "/", branch, "/status"]
  result <- Scotty.liftAndCatchIO $ semaphore (configToken config) path
  Scotty.setHeader "Content-Type" "image/svg+xml"
  Scotty.raw $ badgeFor (branchName result) (branchStatus result)

getServerHandler :: Config -> Scotty.ActionM ()
getServerHandler config = do
  project <- Scotty.param "project"
  server <- Scotty.param "server"
  let path = concat ["/projects/", project, "/servers/", server, "/status"]
  result <- Scotty.liftAndCatchIO $ semaphore (configToken config) path
  Scotty.setHeader "Content-Type" "image/svg+xml"
  Scotty.raw $ badgeFor (serverName result) (serverStatus result)

notFoundHandler :: Scotty.ActionM ()
notFoundHandler = do
  Scotty.status Http.notFound404
  Scotty.json Aeson.Null

semaphore :: Aeson.FromJSON json => Token -> String -> IO json
semaphore token path = do
  let
    url = concat
      [ "https://semaphoreci.com/api/v1"
      , path
      , "?auth_token="
      , unwrapToken token
      ]
  request <- Client.parseRequest url
  manager <- Tls.getGlobalManager
  response <- Client.httpLbs request manager
  case Aeson.eitherDecode $ Client.responseBody response of
    Left problem -> fail problem
    Right json -> pure json

badgeFor :: Name -> Status -> LazyByteString.ByteString
badgeFor name status = Barrier.renderBadge
  (Lens.set Barrier.right (colorFor status) Barrier.flat)
  (unwrapName name)
  (unwrapStatus status)

colorFor :: Status -> Barrier.Color
colorFor status = case unwrapStatus status of
  "failed" -> Barrier.red
  "passed" -> Barrier.brightgreen
  "pending" -> Barrier.yellow
  _ -> Barrier.orange

data Config = Config
  { configPort :: Warp.Port
  , configToken :: Token
  } deriving (Eq, Show)

instance Envy.FromEnv Config where
  fromEnv = Config <$> Envy.envMaybe "PORT" Envy..!= 8080 <*> Envy.env "TOKEN"

data Branch = Branch
  { branchName :: Name
  , branchStatus :: Status
  } deriving (Eq, Show)

instance Aeson.FromJSON Branch where
  parseJSON =
    Aeson.withObject "Branch" $ \object ->
      Branch <$> object Aeson..: "branch_name" <*> object Aeson..: "result"

data Server = Server
  { serverName :: Name
  , serverStatus :: Status
  } deriving (Eq, Show)

instance Aeson.FromJSON Server where
  parseJSON =
    Aeson.withObject "Server" $ \object ->
      Server <$> object Aeson..: "server_name" <*> object Aeson..: "result"

newtype Token = Token
  { unwrapToken :: String
  } deriving (Eq, Show)

instance Envy.Var Token where
  fromVar = Just . Token
  toVar = unwrapToken

newtype Name = Name
  { unwrapName :: Text.Text
  } deriving (Eq, Show)

instance Aeson.FromJSON Name where
  parseJSON = fmap Name . Aeson.parseJSON

newtype Status = Status
  { unwrapStatus :: Text.Text
  } deriving (Eq, Show)

instance Aeson.FromJSON Status where
  parseJSON = fmap Status . Aeson.parseJSON
