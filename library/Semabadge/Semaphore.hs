module Semabadge.Semaphore
  ( Perform
  , getBranchStatus
  , getSemaphore
  , getServerStatus
  , semaphoreUrl
  ) where

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Network.HTTP.Client as Client
import qualified Semabadge.Type.Branch as Branch
import qualified Semabadge.Type.BranchStatus as BranchStatus
import qualified Semabadge.Type.Project as Project
import qualified Semabadge.Type.Server as Server
import qualified Semabadge.Type.ServerStatus as ServerStatus
import qualified Semabadge.Type.Token as Token

type Perform io
   = Client.Request -> io (Client.Response LazyByteString.ByteString)

getBranchStatus ::
     Monad io
  => Perform io
  -> Project.Project
  -> Branch.Branch
  -> Maybe Token.Token
  -> io (Either String BranchStatus.BranchStatus)
getBranchStatus perform project branch token =
  getSemaphore
    perform
    token
    (concat
       [ "/projects/"
       , Project.unwrapProject project
       , "/"
       , Branch.unwrapBranch branch
       , "/status"
       ])

getSemaphore ::
     (Aeson.FromJSON json, Monad io)
  => Perform io
  -> Maybe Token.Token
  -> String
  -> io (Either String json)
getSemaphore perform token path =
  case Client.parseRequest (semaphoreUrl token path) of
    Left message -> pure (Left (Exception.displayException message))
    Right request -> do
      response <- perform request
      pure (Aeson.eitherDecode (Client.responseBody response))

getServerStatus ::
     Monad io
  => Perform io
  -> Project.Project
  -> Server.Server
  -> Maybe Token.Token
  -> io (Either String ServerStatus.ServerStatus)
getServerStatus perform project server token =
  getSemaphore
    perform
    token
    (concat
       [ "/projects/"
       , Project.unwrapProject project
       , "/servers/"
       , Server.unwrapServer server
       , "/status"
       ])

semaphoreUrl :: Maybe Token.Token -> String -> String
semaphoreUrl maybeToken path =
  concat
    [ "https://semaphoreci.com/api/v1"
    , path
    , case maybeToken of
        Nothing -> ""
        Just token -> "?auth_token=" ++ Token.unwrapToken token
    ]
