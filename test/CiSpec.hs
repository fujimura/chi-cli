{-# LANGUAGE OverloadedStrings #-}

module CiSpec ( main, spec ) where

import qualified Cli
import           SpecHelper

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "should clone from cabal package" pending
  it "should replace 'packange-name' in file path to '-p' or '--package-name'" pending
  it "should replace 'packange-name' in file content  to '-p' or '--package-name'" pending
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
