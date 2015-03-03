{-# LANGUAGE ScopedTypeVariables #-}

module Git
    (
      clone
    , config
    , lsFiles
    , expandUrl
    ) where

import           Control.Applicative
import           System.Exit
import           System.Process      (readProcess, readProcessWithExitCode, callCommand)
import           Control.Exception

expandUrl :: String -> String
expandUrl ('g':'h':':':xs) = "git@github.com:" ++ xs ++ ".git"
expandUrl xs = xs

-- | Clone given repository to current directory
clone :: String -> IO ()
clone repoUrl = handle (\(e :: IOException) -> print e >> exitFailure) $ do
    callCommand $ "git clone --no-checkout --quiet " ++ repoUrl ++ " " ++ "./"
    callCommand "git checkout HEAD --quiet"

-- | Return file list by `git ls-files`
lsFiles :: IO [String]
lsFiles = lines <$> readProcess "git" ["ls-files"] []

-- | Return given config value
config :: String -> IO String
config name = removeNewline <$> readProcess "git" ["config", name] []

removeNewline :: String -> String
removeNewline = reverse . dropWhile (=='\n') . reverse
