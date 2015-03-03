module Chi
  (
    run
  ) where

import qualified Git
import           Types

import           Control.Exception   (bracket_)
import           System.Directory
import           System.IO.Temp      (withSystemTempDirectory)
import           Data.List.Split (splitOn)
import           Control.Monad
import           Data.List               ((\\), intersperse)
import           Data.Maybe              (fromMaybe)
import qualified Data.Text               as T (pack, unpack)
import           Data.Text.Encoding      (decodeUtf8)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           System.Directory        (createDirectoryIfMissing)
import           System.FilePath         (dropFileName, joinPath, normalise,
                                          splitPath)
import           System.Process          (system)

run :: Option -> IO ()
run option = do
    files <- fetch (source option)
    print files
    writeFiles (convertFiles option files)

fetch :: Source -> IO [File]
fetch (Repo repo) = do
    e <- doesDirectoryExist repo
    repo' <- if e
               -- It seems to be an file path
               then canonicalizePath repo
               -- Not looks like a file path
               else return repo
    inTemporaryDirectory "chi" $ do
        Git.clone repo'
        paths <- Git.lsFiles
        mapM fetchFile paths

-- |Run callback in a temporary directory.
inTemporaryDirectory :: String         -- ^ Base of temporary directory name
                     -> IO a -> IO a -- ^ Callback
inTemporaryDirectory name callback =
    withSystemTempDirectory name $ flip inDirectory callback


-- |Run callback in given directory.
inDirectory :: FilePath        -- ^ Filepath to run callback
            -> IO a -> IO a  -- ^ Callback
inDirectory path callback = do
    pwd <- getCurrentDirectory
    bracket_ (setCurrentDirectory path) (setCurrentDirectory pwd) callback

fetchFile :: FilePath -> IO File
fetchFile fp = do
    content <- readFile fp
    return (fp,content)

convertFiles :: Option -> [File] -> [Modified File]
convertFiles option files = map (convert option) files

convert :: Option -> File -> Modified File
convert option file@(path,contents) = Modified (rewritePath path, substitute contents) file
  where
    substitute :: String -> String
    substitute = id
    rewritePath :: FilePath -> FilePath
    rewritePath = replacePackageName . replaceModuleName
      where
        replacePackageName = replace "package-name" (packageName option)
        replaceModuleName  = replace "ModuleName" $ moduleNameToFilePath (moduleName option)

-- | Convert module name to path
--
-- >>> moduleNameToFilePath "Foo.Bar"
-- "Foo/Bar"
--
moduleNameToFilePath :: String -> FilePath
moduleNameToFilePath = joinPath . splitOn "."

writeFiles :: [Modified File] -> IO ()
writeFiles files = mapM_ write' files
  where
    write' (Modified f _) = write f


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b = foldl1 (++) . intersperse b . splitOn a

write :: File -> IO ()
write (path,contents) =
    createDirectoryIfMissing True (dropFileName path) >> writeFile path contents
