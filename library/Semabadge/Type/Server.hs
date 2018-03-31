module Semabadge.Type.Server
  ( Server
  , makeServer
  , unwrapServer
  ) where

newtype Server =
  Server String
  deriving (Eq, Show)

makeServer :: String -> Server
makeServer server = Server server

unwrapServer :: Server -> String
unwrapServer (Server server) = server
