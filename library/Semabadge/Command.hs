module Semabadge.Command
  ( getConfig
  ) where

import qualified Control.Monad as Monad
import qualified Data.String as String
import qualified Semabadge.Type.Config as Config
import qualified Semabadge.Type.Token as Token
import qualified Semabadge.Version as Version
import qualified System.Console.GetOpt as Console
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.IO as IO
import qualified Text.Read as Read

getConfig :: IO Config.Config
getConfig = do
  args <- Environment.getArgs
  let (updates, unexpecteds, unknowns, errors) =
        Console.getOpt' Console.Permute options args
  Monad.unless (null errors) (printErrorsAndExit errors)
  mapM_ printUnknown unknowns
  mapM_ printUnexpected unexpecteds
  case buildConfig updates of
    Left problem -> do
      IO.hPutStrLn IO.stderr ("ERROR: invalid config: " ++ problem)
      Exit.exitFailure
    Right config -> do
      Monad.when (Config.configShowHelp config) printHelpAndExit
      Monad.when (Config.configShowVersion config) printVersionAndExit
      pure config

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

printErrorsAndExit :: [String] -> IO ()
printErrorsAndExit errors = do
  mapM_ printError errors
  Exit.exitFailure

printError :: String -> IO ()
printError error_ = IO.hPutStr IO.stderr ("ERROR: " ++ error_)

printUnknown :: String -> IO ()
printUnknown unknown =
  IO.hPutStrLn IO.stderr (concat ["WARNING: unknown option `", unknown, "'"])

printUnexpected :: String -> IO ()
printUnexpected unexpected =
  IO.hPutStrLn
    IO.stderr
    (concat ["WARNING: unexpected argument `", unexpected, "'"])

buildConfig ::
     [Config.Config -> Either String Config.Config]
  -> Either String Config.Config
buildConfig updates =
  Monad.foldM (\config update -> update config) Config.defaultConfig updates

printHelpAndExit :: IO ()
printHelpAndExit = do
  IO.hPutStr
    IO.stderr
    (Console.usageInfo
       (unwords ["semabadge", "version", Version.versionString])
       options)
  Exit.exitFailure

printVersionAndExit :: IO ()
printVersionAndExit = do
  IO.hPutStrLn IO.stderr Version.versionString
  Exit.exitFailure
