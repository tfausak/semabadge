module Semabadge.Type.Config
  ( Config(..)
  , defaultConfig
  ) where

import qualified Data.String as String
import qualified Network.Wai.Handler.Warp as Warp
import qualified Semabadge.Type.Token as Token

data Config = Config
  { configHost :: Warp.HostPreference
  , configPort :: Warp.Port
  , configShowHelp :: Bool
  , configShowVersion :: Bool
  , configToken :: Token.Token
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config
    { configHost = String.fromString "127.0.0.1"
    , configPort = 8080
    , configShowHelp = False
    , configShowVersion = False
    , configToken = Token.makeToken "no-token-set"
    }
