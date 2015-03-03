{-# LANGUAGE OverloadedStrings #-}

module ChiSpec ( main, spec ) where

import qualified Cli
import           SpecHelper

import           Control.Monad
import           System.Directory   (doesDirectoryExist, doesFileExist,
                                     getCurrentDirectory)
import           System.Exit        (ExitCode (..))
import           System.FilePath    (joinPath, (</>))
import           System.IO          (stdout)
import           System.IO.Silently (capture, hSilence)
import           System.Process     (system)
import           Test.Hspec

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

  it "should replace 'ModuleName' in file path to '-m' or '--module-name'" $ do
    root <- getCurrentDirectory
    inTestDirectory $ hSilence [stdout] $ do
      Cli.run ["foo-bar-baz", "-m", "Something.Special", "-r", (root </> "test" </> "template")]
      doesFileExist "foo-bar-baz/src/Something/Special.hs" `shouldReturn` True

  it "should replace 'ModuleName' in file content to '-m' or '--module-name'" $ do
    root <- getCurrentDirectory
    inTestDirectory $ hSilence [stdout] $ do
      Cli.run ["foo-bar-baz", "-m", "Something.Special", "-r", (root </> "test" </> "template")]
      actual <- readFile "foo-bar-baz/src/Something/Special.hs"
      actual `shouldContain` "module Something.Special"

  it "should replace 'ModuleName' in file path to to capitalized package name, if not specified" $ do
    root <- getCurrentDirectory
    inTestDirectory $ hSilence [stdout] $ do
      Cli.run ["foo-bar-baz", "-r", (root </> "test" </> "template")]
      doesFileExist "foo-bar-baz/src/Foo/Bar/Baz.hs" `shouldReturn` True

  it "should replace 'ModuleName' in file content to capitalized package name, if not specified" $ do
    root <- getCurrentDirectory
    inTestDirectory $ hSilence [stdout] $ do
      Cli.run ["foo-bar-baz", "-r", (root </> "test" </> "template")]
      actual <- readFile "foo-bar-baz/src/Foo/Bar/Baz.hs"
      actual `shouldContain` "module Foo.Bar.Baz"

  it "should update 'author' of .cabal with the user.name in git config" $ do
    root <- getCurrentDirectory
    inTestDirectory $ withLocalGitConfig [("user.name", "\"John Doe\"")] $ hSilence [stdout] $ do
      Cli.run ["foo-bar-baz", "-r", (root </> "test" </> "template")]
      actual <- readFile "foo-bar-baz/foo-bar-baz.cabal"
      actual `shouldContain` "author: John Doe"

  it "should replace 'maintainer' in .cabal with the user.email in git config" $ do
    root <- getCurrentDirectory
    inTestDirectory $ withLocalGitConfig [("user.email", "\"john@doe.com\"")] $ hSilence [stdout] $ do
      Cli.run ["foo-bar-baz", "-r", (root </> "test" </> "template")]
      actual <- readFile "foo-bar-baz/foo-bar-baz.cabal"
      actual `shouldContain` "maintainer: john@doe.com"

  it "should replace '$author' in file content to the user.name in git config" $ do
    root <- getCurrentDirectory
    inTestDirectory $ withLocalGitConfig [("user.name", "\"John Doe\"")] $ hSilence [stdout] $ do
      Cli.run ["foo-bar-baz", "-r", (root </> "test" </> "template")]
      actual <- readFile "foo-bar-baz/LICENSE"
      actual `shouldContain` "Neither the name of John Doe nor the names of other"

  it "should create files under '--directory-name'" $ do
    root <- getCurrentDirectory
    inTestDirectory $ hSilence [] $ do
      Cli.run ["foo-bar-baz", "-d", "qux", "-r", (root </> "test" </> "template")]
      doesFileExist "qux/foo-bar-baz.cabal" `shouldReturn` True

  it "should run command specified with '--after-command'" $ do
    root <- getCurrentDirectory
    inTestDirectory $ hSilence [stdout] $ do
      Cli.run ["foo-bar-baz", "-r", (root </> "test" </> "template"), "--after-command", "cabal sandbox init"]
      doesFileExist "foo-bar-baz/cabal.sandbox.config" `shouldReturn` True
