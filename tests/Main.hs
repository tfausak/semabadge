module Main
  ( main
  ) where

import Test.Hspec

import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Either as Either
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified Graphics.Badge.Barrier as Barrier
import qualified Network.HTTP.Client.Internal as Client
import qualified Network.HTTP.Types as Http
import qualified Semabadge

{-# ANN module "Hlint: ignore Redundant do" #-}

main :: IO ()
main =
  hspec . parallel . describe "Semabadge" $ do
    let branchStatus =
          Semabadge.BranchStatus
            { Semabadge.branchStatusResult = Semabadge.ResultPassed
            , Semabadge.branchStatusBranchName = Text.pack "master"
            }
        branchStatusJson =
          "{ \"result\": \"passed\", \"branch_name\": \"master\" }"
        serverStatus =
          Semabadge.ServerStatus
            { Semabadge.serverStatusResult = Semabadge.ResultPassed
            , Semabadge.serverStatusServerName = Text.pack "production"
            }
        serverStatusJson =
          "{ \"result\": \"passed\", \"server_name\": \"production\" }"
    describe "Application" . describe "application" $ it "needs tests" pending
    describe "Badge" $ do
      describe "badgeColor" $ do
        it "is red for failed results" $ do
          Semabadge.badgeColor Semabadge.ResultFailed `shouldBe` Barrier.red
        it "is green for passed results" $ do
          Semabadge.badgeColor Semabadge.ResultPassed `shouldBe`
            Barrier.brightgreen
        it "is gray for pending results" $ do
          Semabadge.badgeColor Semabadge.ResultPending `shouldBe` Barrier.gray
      describe "badgeForBranch" $ do
        it "is not empty" $ do
          Semabadge.badgeForBranch branchStatus Nothing `shouldNotSatisfy`
            LazyByteString.null
      describe "badgeForServer" $ do
        it "is not empty" $ do
          Semabadge.badgeForServer serverStatus Nothing `shouldNotSatisfy`
            LazyByteString.null
      describe "badgeRightLabel" $ do
        it "works for failed results" $ do
          Semabadge.badgeRightLabel Semabadge.ResultFailed `shouldBe`
            Text.pack "failed"
        it "works for passed results" $ do
          Semabadge.badgeRightLabel Semabadge.ResultPassed `shouldBe`
            Text.pack "passed"
        it "works for pending results" $ do
          Semabadge.badgeRightLabel Semabadge.ResultPending `shouldBe`
            Text.pack "pending"
    describe "Command" $ do
      describe "getConfigWith" $ do
        it "returns the default with no arguments" $ do
          Semabadge.getConfigWith [] `shouldBe`
            Right (Semabadge.defaultConfig, [])
        it "errors with invalid arguments" $ do
          Semabadge.getConfigWith ["--help=invalid"] `shouldSatisfy`
            Either.isLeft
        it "warns about unexpected arguments" $ do
          Semabadge.getConfigWith ["unexpected"] `shouldBe`
            Right
              ( Semabadge.defaultConfig
              , ["WARNING: unexpected argument `unexpected'"])
        it "warns about unknown options" $ do
          Semabadge.getConfigWith ["--unknown"] `shouldBe`
            Right
              ( Semabadge.defaultConfig
              , ["WARNING: unknown option `--unknown'"])
        it "shows the help" $ do
          Semabadge.getConfigWith ["--help"] `shouldSatisfy` Either.isLeft
        it "shows the version" $ do
          Semabadge.getConfigWith ["--version"] `shouldSatisfy` Either.isLeft
        it "sets an option" $ do
          Semabadge.getConfigWith ["--port=80"] `shouldBe`
            Right (Semabadge.defaultConfig {Semabadge.configPort = 80}, [])
    describe "Handler" $ do
      describe "exceptionHandler" $ it "needs tests" pending
      describe "defaultHandler" $ it "needs tests" pending
      describe "getHealthCheckHandler" $ it "needs tests" pending
      describe "getBranchBadgeHandler" $ it "needs tests" pending
      describe "getServerBadgeHandler" $ it "needs tests" pending
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
    describe "Semaphore" $ do
      let responseWith body =
            Client.Response
              { Client.responseStatus = Http.ok200
              , Client.responseVersion = Http.http11
              , Client.responseHeaders = []
              , Client.responseBody =
                  LazyByteString.fromStrict (Semabadge.toUtf8 body)
              , Client.responseCookieJar = Client.createCookieJar []
              , Client.responseClose' = Client.ResponseClose (pure ())
              }
      describe "getBranchStatus" $ do
        it "gets the branch status" $ do
          Semabadge.getBranchStatus
            (\_ -> pure (responseWith branchStatusJson))
            (Semabadge.makeProject "semabadge")
            (Semabadge.makeBranch "master")
            Nothing `shouldReturn`
            Right branchStatus
      describe "getSemaphore" $ do
        let getJson path body =
              Semabadge.getSemaphore
                (\_ -> pure (responseWith body))
                Nothing
                path :: IO (Either String Aeson.Value)
        it "fails if the URL is invalid" $ do
          getJson "%" "" `shouldReturn`
            Left
              "InvalidUrlException \"https://semaphoreci.com/api/v1%\" \"Invalid URL\""
        it "fails if the response is invalid JSON" $ do
          getJson "" "invalid" `shouldReturn`
            Left "Error in $: Failed reading: not a valid json value"
        it "succeeds" $ do getJson "" "null" `shouldReturn` Right Aeson.Null
      describe "getServerStatus" $ do
        it "gets the server status" $ do
          Semabadge.getServerStatus
            (\_ -> pure (responseWith serverStatusJson))
            (Semabadge.makeProject "semabadge")
            (Semabadge.makeServer "production")
            Nothing `shouldReturn`
            Right serverStatus
      describe "semaphoreUrl" $ do
        it "builds a URL without a token" $ do
          Semabadge.semaphoreUrl Nothing "/path" `shouldBe`
            "https://semaphoreci.com/api/v1/path"
        it "builds a URL with a token" $ do
          Semabadge.semaphoreUrl (Just (Semabadge.makeToken "token")) "/path" `shouldBe`
            "https://semaphoreci.com/api/v1/path?auth_token=token"
    describe "Server" . describe "settings" $ it "needs tests" pending
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
          parseBranchStatus branchStatusJson `shouldBe` Right branchStatus
        it "fails to parse an invalid branch status" $ do
          parseBranchStatus "null" `shouldBe`
            Left "Error in $: expected record (:*:), encountered Null"
      describe "Config" $ do
        describe "defaultConfig" $ do
          it "has reasonable defaults" $ do
            let config = Semabadge.defaultConfig
            Semabadge.configHost config `shouldBe`
              String.fromString "127.0.0.1"
            Semabadge.configPort config `shouldBe` 8080
            Semabadge.configShowHelp config `shouldBe` False
            Semabadge.configShowVersion config `shouldBe` False
            Semabadge.configToken config `shouldBe` Nothing
      describe "Project" $ do
        it "can be round tripped" $ do
          Semabadge.unwrapProject (Semabadge.makeProject "it") `shouldBe` "it"
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
          parseServerStatus serverStatusJson `shouldBe` Right serverStatus
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
