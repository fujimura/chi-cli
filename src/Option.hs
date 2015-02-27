module Option
  ( buildOption
  ) where

import           CommandLineOption   (CommandLineOption)
import qualified CommandLineOption
import           Types

buildOption :: CommandLineOption -> IO Option
buildOption copt = undefined

--
--  let packageName = CommandLineOption.packageName copt
--  return Option { packageName = packageName
--                               , moduleName  = fromJu
--                               , showLineNumber = CommandLineOption.showLineNumber copt
--                               , debug = False
--                               }
