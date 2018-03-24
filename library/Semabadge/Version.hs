module Semabadge.Version
  ( version
  , versionString
  ) where

import qualified Data.Version as Version
import qualified Paths_semabadge as This

version :: Version.Version
version = This.version

versionString :: String
versionString = Version.showVersion version
