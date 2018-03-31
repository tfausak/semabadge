module Semabadge.Command
  ( getConfigWith
  ) where

import qualified Control.Monad as Monad
import qualified Data.String as String
import qualified Semabadge.Type.Config as Config
import qualified Semabadge.Type.Token as Token
import qualified Semabadge.Version as Version
import qualified System.Console.GetOpt as Console
import qualified Text.Read as Read

getConfigWith :: [String] -> Either String (Config.Config, [String])
getConfigWith args = do
  let (updates, unexpecteds, unknowns, errors) =
        Console.getOpt' Console.Permute options args
  Monad.unless (null errors) (Left (concatMap formatError errors))
  case buildConfig updates of
    Left problem -> Left problem
    Right config -> do
      Monad.when (Config.configShowHelp config) (Left helpText)
      Monad.when (Config.configShowVersion config) (Left Version.versionString)
      Right
        ( config
        , map formatUnexpected unexpecteds ++ map formatUnknown unknowns)

formatError :: String -> String
formatError error_ = "ERROR: " ++ error_

helpText :: String
helpText =
  Console.usageInfo
    (unwords ["semabadge", "version", Version.versionString])
    options

formatUnexpected :: String -> String
formatUnexpected unexpected =
  concat ["WARNING: unexpected argument `", unexpected, "'"]

formatUnknown :: String -> String
formatUnknown unknown = concat ["WARNING: unknown option `", unknown, "'"]

options :: [Option]
options = [helpOption, hostOption, portOption, tokenOption, versionOption]

type Option = Console.OptDescr (Config.Config -> Either String Config.Config)

helpOption :: Option
helpOption =
  Console.Option
    ['h', '?']
    ["help"]
    (Console.NoArg (\config -> Right config {Config.configShowHelp = True}))
    "show the help and exit"

hostOption :: Option
hostOption =
  Console.Option
    []
    ["host"]
    (Console.ReqArg
       (\rawHost config ->
          Right config {Config.configHost = String.fromString rawHost})
       "HOST")
    "host to bind"

portOption :: Option
portOption =
  Console.Option
    []
    ["port"]
    (Console.ReqArg
       (\rawPort config ->
          case Read.readEither rawPort of
            Left message ->
              Left
                (concat ["invalid port: ", show rawPort, " (", message, ")"])
            Right port -> Right config {Config.configPort = port})
       "PORT")
    "port number to bind"

tokenOption :: Option
tokenOption =
  Console.Option
    []
    ["token"]
    (Console.ReqArg
       (\token config ->
          Right config {Config.configToken = Just (Token.makeToken token)})
       "TOKEN")
    "Semaphore authentication token"

versionOption :: Option
versionOption =
  Console.Option
    []
    ["version"]
    (Console.NoArg (\config -> Right config {Config.configShowVersion = True}))
    "show the version number and exit"

buildConfig ::
     [Config.Config -> Either String Config.Config]
  -> Either String Config.Config
buildConfig updates =
  Monad.foldM (\config update -> update config) Config.defaultConfig updates
