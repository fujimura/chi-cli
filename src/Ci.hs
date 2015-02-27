module Ci
  (
    run
  ) where

import           Types

run :: Option -> IO ()
run option = return ()

fetch :: Source -> IO File
fetch = undefined

convertFiles :: Option -> [File] -> [Modified File]
convertFiles option files = map (convert option) files

convert :: Option -> File -> Modified File
convert option file = undefined

writeFiles :: [Modified File] -> IO ()
writeFiles files = mapM_ write' files
  where
    write' (Modified f _) = write f

write :: File -> IO ()
write = undefined
