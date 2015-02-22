module Cli
  ( run
  ) where

import           CommandLineOption   (commandLineOption)
import qualified ModuleName
import           Option              (buildOption)
import           Types

import qualified Paths_package_name  (version)

import           Control.Applicative
import           Data.Version        (showVersion)
import           Options.Applicative

run :: [String] -> IO ()
run args = parseArgs args >>= ModuleName.run

parseArgs :: [String] -> IO Option
parseArgs args = handleParseResult (execParserPure (prefs idm) opts args) >>= buildOption
  where
    opts = info (helper <*> (version <*> commandLineOption))
      ( fullDesc
     <> progDesc "Hello."
     <> header "package-name - a template for your cli application" )
    version = infoOption (showVersion Paths_package_name.version)
      (  short 'v'
      <> long "version"
      <> help "Print version information" )
