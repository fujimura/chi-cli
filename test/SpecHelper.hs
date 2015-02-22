{-# LANGUAGE ScopedTypeVariables #-}

module SpecHelper where

import           Control.Exception  (bracket, bracket_)
import           System.Directory   (createDirectoryIfMissing,
                                     getCurrentDirectory,
                                     removeDirectoryRecursive,
                                     setCurrentDirectory)
import           System.Environment (lookupEnv, setEnv, unsetEnv)

withEnv :: String -> String -> IO a -> IO a
withEnv k v action = do
    bracket setup teardown (const action)
  where
    setup :: IO (Maybe String)
    setup = do
      mv <- lookupEnv k
      setEnv k v
      return mv
    teardown :: Maybe String -> IO ()
    teardown (Just _v) = setEnv k _v >> return ()
    teardown Nothing   = unsetEnv k >> return ()

inTestDirectory :: IO () -> IO ()
inTestDirectory action = do
    pwd <- getCurrentDirectory
    let go    = createDirectoryIfMissing True testDirectory >> setCurrentDirectory testDirectory
        flush = removeDirectoryRecursive testDirectory
        back  = setCurrentDirectory pwd
    bracket_ go (back >> flush) action

testDirectory :: String
testDirectory = "test_project"

{-# ANN module "HLint: Redundant do" #-}
