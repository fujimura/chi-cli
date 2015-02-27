{-# LANGUAGE OverloadedStrings #-}

module ChiSpec ( main, spec ) where

import qualified Cli
import           SpecHelper

import           Test.Hspec
import           System.Directory    (doesDirectoryExist, doesFileExist,
                                      getCurrentDirectory)
import           System.Exit         (ExitCode (..))
import           System.FilePath     (joinPath, (</>))
import           System.IO           (stdout)
import           System.IO.Silently  (capture, hSilence)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "should clone from cabal package with '-c'" pending
  it "should clone from git repository with '-r'" pending
  it "should replace 'packange-name' in file path with first argument" $ do
    root <- getCurrentDirectory
    inTestDirectory $ hSilence [stdout] $ do
      Cli.run ["foo-bar-baz", "-r", (root </> "test" </> "template")]

      doesFileExist "foo-bar-baz/foo-bar-baz.cabal" `shouldReturn` True

  it "should replace 'packange-name' in file content with first argument" $ do
    root <- getCurrentDirectory
    inTestDirectory $ hSilence [stdout] $ do
      Cli.run ["foo-bar-baz", "-r", (root </> "test" </> "template")]
      actual <- readFile "foo-bar-baz/foo-bar-baz.cabal"
      actual `shouldContain` "name: foo-bar-baz"

  it "should replace 'ModuleName' in file path to '-m' or '--module-name'" pending
  it "should replace 'ModuleName' in file content to '-m' or '--module-name'" pending
  it "should replace 'ModuleName' in file path to to capitalized package name, if not specified" pending
  it "should replace 'ModuleName' in file content to capitalized package name, if not specified" pending
  it "should replace '$author' in file content to '-a' or '--author'" pending
  it "should replace '$author' in file content to the user.name in git config, if not specified" pending
  it "should replace '$email' in file content to '-e' or '--email'" pending
  it "should replace '$email' in file content to the user.email in git config, if not specified" pending
  it "should replace '$year' in file content to current year" pending
  it "should run command specified with '--after-command'" pending

  it "should generate files under the directory named `package-name`" pending