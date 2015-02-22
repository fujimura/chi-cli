module Option
  ( buildOption
  ) where

import           CommandLineOption   (CommandLineOption)
import qualified CommandLineOption
import           Types

buildOption :: CommandLineOption -> IO Option
buildOption copt = return Option { source = CommandLineOption.source copt
                                 , showLineNumber = CommandLineOption.showLineNumber copt
                                 , debug = False
                                 }
