module Main
  ( main
  ) where

import Test.Hspec

import qualified Data.ByteString as ByteString
import qualified Data.Version as Version
import qualified Semabadge

{-# ANN module "Hlint: ignore Redundant do" #-}

main :: IO ()
main =
  hspec . parallel . describe "Semabadge" $ do
    describe "Unicode" $ do
      describe "fromUtf8" $ do
        it "decodes UTF-8" $ do
          Semabadge.fromUtf8 (ByteString.pack [0x68, 0x69]) `shouldBe` "hi"
        it "replaces invalid bytes" $ do
          Semabadge.fromUtf8 (ByteString.pack [0xc0]) `shouldBe` "\xfffd"
      describe "toUtf8" $ do
        it "encodes UTF-8" $ do
          Semabadge.toUtf8 "hi" `shouldBe` ByteString.pack [0x68, 0x69]
    describe "Version" $ do
      describe "version" $ do
        it "has some numbers" $ do
          Version.versionBranch Semabadge.version `shouldNotSatisfy` null
        it "has no tags" $ do
          let Version.Version _ tags = Semabadge.version
          tags `shouldSatisfy` null
      describe "versionString" $ do
        it "is not null" $ do Semabadge.versionString `shouldNotSatisfy` null
