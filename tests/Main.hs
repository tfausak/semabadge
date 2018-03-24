module Main
  ( main
  ) where

import Test.Hspec

import qualified Data.Version as Version
import qualified Semabadge

{-# ANN module "Hlint: ignore Redundant do" #-}

main :: IO ()
main =
  hspec . parallel . describe "Semabadge" $ do
    describe "Version" $ do
      describe "version" $ do
        it "has some numbers" $ do
          Version.versionBranch Semabadge.version `shouldNotSatisfy` null
        it "has no tags" $ do
          let Version.Version _ tags = Semabadge.version
          tags `shouldSatisfy` null
      describe "versionString" $ do
        it "is not null" $ do Semabadge.versionString `shouldNotSatisfy` null
