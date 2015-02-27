module Cli
  ( run
  ) where

import           CommandLineOption   (commandLineOption)
import qualified Chi
import           Option              (buildOption)
import           Types

import qualified Paths_chi  (version)

import           Control.Applicative
import           Data.Version        (showVersion)
import           Options.Applicative

run :: [String] -> IO ()
run args = parseArgs args >>= Chi.run

parseArgs :: [String] -> IO Option
parseArgs args = handleParseResult (execParserPure (prefs idm) opts args) >>= buildOption
  where
    opts = info (helper <*> (version <*> commandLineOption))
      ( fullDesc
     <> progDesc "Hello."
     <> header "cji - a template for your cli application" )
    version = infoOption (showVersion Paths_chi.version)
      (  short 'v'
      <> long "version"
      <> help "Print version information" )
