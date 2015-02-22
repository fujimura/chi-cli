{-# LANGUAGE OverloadedStrings #-}

module ModuleNameSpec ( main, spec ) where

import qualified Cli
import           SpecHelper

import           System.IO.Silently (capture)
import           Test.Hspec

main :: IO ()
main = hspec spec

content :: String
content = unlines [ "hello"
                  , "this"
                  , "is"
                  , "test"
                  ]

spec :: Spec
spec =
    describe "run" $ do
      it "should write content to the file" $ inTestDirectory $ do
        writeFile "foo" content
        (output, _) <- capture $ Cli.run ["foo"]
        output `shouldBe` content

      it "should output debug info with DEBUG=1" $ withEnv "DEBUG" "1" $ inTestDirectory $ do
        writeFile "foo" content
        (output, _) <- capture $ Cli.run ["foo"]
        output `shouldBe` unlines (zipWith (\i -> ([i, ' '] ++)) ['1'..] (lines content))
