module Semabadge.Main
  ( defaultMain
  ) where

import qualified Semabadge.Version as Version

defaultMain :: IO ()
defaultMain = putStrLn ("semabadge-" ++ Version.versionString)
