module CommandLineOption
  ( CommandLineOption(..)
  , commandLineOption
  ) where

import           Options.Applicative

data CommandLineOption = CommandLineOption
                       { packageName :: String
                       , repo         :: Maybe String
               -- TODO        , cabalPackage :: Maybe String
                       }

commandLineOption :: Parser CommandLineOption
commandLineOption = CommandLineOption
   <$> argument str (help "Package name")
   <*> optional (strOption (short 'r' <> long "repository" <> help "Repository of template"))
--   <*> optional (strOption (short 'c' <> long "cabal-package" <> help "Name of cabal package"))
