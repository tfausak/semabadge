module Semabadge.Application
  ( application
  ) where

import qualified Data.Text as Text
import qualified Network.Wai as Wai
import qualified Semabadge.Handler as Handler
import qualified Semabadge.Semaphore as Semaphore
import qualified Semabadge.Type.Branch as Branch
import qualified Semabadge.Type.Project as Project
import qualified Semabadge.Type.Server as Server
import qualified Semabadge.Type.Token as Token
import qualified Semabadge.Unicode as Unicode

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
