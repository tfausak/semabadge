module Main
  ( main
  ) where

import Test.Hspec

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
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
      let parseJson :: Aeson.FromJSON json => String -> Either String json
          parseJson string =
            Aeson.eitherDecode
              (LazyByteString.fromStrict (Semabadge.toUtf8 string))
      describe "Branch" $ do
        it "can be round tripped" $ do
          Semabadge.unwrapBranch (Semabadge.makeBranch "it") `shouldBe` "it"
      describe "BranchStatus" $ do
        let parseBranchStatus =
              parseJson :: String -> Either String Semabadge.BranchStatus
        it "parses a branch status" $ do
          parseBranchStatus
            "{ \"result\": \"passed\", \"branch_name\": \"master\" }" `shouldBe`
            Right
              Semabadge.BranchStatus
                { Semabadge.branchStatusResult = Semabadge.ResultPassed
                , Semabadge.branchStatusBranchName = Text.pack "master"
                }
        it "fails to parse an invalid branch status" $ do
          parseBranchStatus "null" `shouldBe`
            Left "Error in $: expected record (:*:), encountered Null"
      describe "Result" $ do
        let parseResult = parseJson :: String -> Either String Semabadge.Result
        it "parses a failed result" $ do
          parseResult "\"failed\"" `shouldBe` Right Semabadge.ResultFailed
        it "parses a passed result" $ do
          parseResult "\"passed\"" `shouldBe` Right Semabadge.ResultPassed
        it "parses a pending result" $ do
          parseResult "\"pending\"" `shouldBe` Right Semabadge.ResultPending
        it "fails to parse an invalid result" $ do
          parseResult "null" `shouldBe`
            Left "Error in $: expected Result, encountered Null"
      describe "Server" $ do
        it "can be round tripped" $ do
          Semabadge.unwrapServer (Semabadge.makeServer "it") `shouldBe` "it"
      describe "ServerStatus" $ do
        let parseServerStatus =
              parseJson :: String -> Either String Semabadge.ServerStatus
        it "parses a server status" $ do
          parseServerStatus
            "{ \"result\": \"passed\", \"server_name\": \"production\" }" `shouldBe`
            Right
              Semabadge.ServerStatus
                { Semabadge.serverStatusResult = Semabadge.ResultPassed
                , Semabadge.serverStatusServerName = Text.pack "production"
                }
        it "fails to parse an invalid server status" $ do
          parseServerStatus "null" `shouldBe`
            Left "Error in $: expected record (:*:), encountered Null"
      describe "Token" $ do
        it "can be round tripped" $ do
          Semabadge.unwrapToken (Semabadge.makeToken "it") `shouldBe` "it"
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
