module Main
  ( main
  ) where

import Test.Hspec

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Version as Version
import qualified Semabadge

{-# ANN module "Hlint: ignore Redundant do" #-}

main :: IO ()
main =
  hspec . parallel . describe "Semabadge" $ do
    describe "Json" $ do
      describe "optionsFor" $ do
        it "strips the field prefix" $ do
          let options = Semabadge.optionsFor "p"
          Aeson.fieldLabelModifier options "pf" `shouldBe` "f"
        it "converts the field to camel case" $ do
          let options = Semabadge.optionsFor ""
          Aeson.fieldLabelModifier options "fA" `shouldBe` "f_a"
    describe "Lens" $ do
      describe "set" $ do
        it "sets the value" $ do
          let _1 :: Functor f => (l -> f l') -> (l, r) -> f (l', r)
              _1 f (l, r) = fmap (\l' -> (l', r)) (f l)
          Semabadge.set _1 "true" (True, ()) `shouldBe` ("true", ())
    describe "List" $ do
      describe "dropPrefix" $ do
        it "drops the prefix" $ do
          Semabadge.dropPrefix "sp" "spam" `shouldBe` Just "am"
        it "returns nothing when the prefix doesn't match" $ do
          Semabadge.dropPrefix "h" "spam" `shouldBe` Nothing
      describe "unsafeDropPrefix" $ do
        it "drops the prefix" $ do
          Semabadge.unsafeDropPrefix "sp" "spam" `shouldBe` "am"
        it "throws an exception when the prefix doesn't match" $ do
          let result = Semabadge.unsafeDropPrefix "h" "spam"
          Exception.evaluate result `shouldThrow` anyErrorCall
    describe "Type" $ do
      describe "Result" $ do
        let parseResult :: String -> Either String Semabadge.Result
            parseResult string =
              Aeson.eitherDecode
                (LazyByteString.fromStrict (Semabadge.toUtf8 string))
        it "parses a failed result" $ do
          parseResult "\"failed\"" `shouldBe` Right Semabadge.ResultFailed
        it "parses a passed result" $ do
          parseResult "\"passed\"" `shouldBe` Right Semabadge.ResultPassed
        it "parses a pending result" $ do
          parseResult "\"pending\"" `shouldBe` Right Semabadge.ResultPending
        it "fails to parse an unknown result" $ do
          parseResult "\"unknown\"" `shouldBe`
            Left "Error in $: expected Result, encountered String"
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
