module CommandLineOption
  ( CommandLineOption(..)
  , commandLineOption
  ) where

import           Options.Applicative

data CommandLineOption = CommandLineOption
                       { source         :: String
                       , showLineNumber :: Bool
                       }

commandLineOption :: Parser CommandLineOption
commandLineOption = CommandLineOption
   <$> argument str (help "Source")
   <*> switch (short 'l' <> long "show-line-number" <> help "Show line number")
