module Main
  ( main
  ) where

import qualified Distribution.Simple as Cabal

main :: IO ()
main = Cabal.defaultMain
