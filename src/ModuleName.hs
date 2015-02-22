module ModuleName
  (
    run
  ) where

import           Types

run :: Option -> IO ()
run option = do
    file <- readFile (source option)
    putStr file
    return ()
