{-# LANGUAGE NamedFieldPuns  #-}

module Chi
  (
    run
  ) where

import qualified Git
import           Types

import           Control.Exception                           (bracket_)
import           Control.Monad
import           Data.List                                   (intersperse, (\\))
import           Data.List.Split                             (splitOn)
import           Data.Maybe                                  (fromMaybe)
import qualified Data.Text                                   as T (pack, unpack)
import           Data.Text.Encoding                          (decodeUtf8)
import           Data.Text.Lazy.Encoding                     (encodeUtf8)
import           Distribution.PackageDescription             (GenericPackageDescription (GenericPackageDescription))
import qualified Distribution.PackageDescription             as PackageDescription
import           Distribution.PackageDescription.Parse       (readPackageDescription)
import           Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import           Distribution.Simple.Utils                   (findPackageDesc)
import           Distribution.Verbosity                      as Verbosity (Verbosity, normal)
import           System.Directory
import           System.Directory                            (createDirectoryIfMissing)
import           System.FilePath                             (dropFileName,
                                                              joinPath,
                                                              normalise,
                                                              splitPath, (</>))
import           System.IO.Temp                              (withSystemTempDirectory)
import           System.Process                              (system)

run :: Option -> IO ()
run option = do
    files <- fetch (source option)
    writeFiles (convertFiles option files)
    updateCabalFile option
    runAfterCommands option

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
convert Option {packageName, moduleName, directoryName, author, email, year} file@(path,contents) =
    Modified (rewritePath path, substitute contents) file
  where
    substitute :: String -> String
    substitute = foldl1 (.) $ map (uncurry replace) [ ("package-name", packageName)
                                                    , ("ModuleName", moduleName)
                                                    , ("$author", author)
                                                    , ("$email", email)
                                                    , ("$year", year)
                                                    ]
    rewritePath :: FilePath -> FilePath
    rewritePath = addDirectoryName . replacePackageName . replaceModuleName
      where
        addDirectoryName   = (directoryName </>)
        replacePackageName = replace "package-name" (packageName)
        replaceModuleName  = replace "ModuleName" $ moduleNameToFilePath (moduleName)

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

updateCabalFile :: Option -> IO ()
updateCabalFile Option {directoryName, author, email} = do
  path <- findPackageDesc directoryName
  gPkgDesc@GenericPackageDescription { PackageDescription.packageDescription = pd } <-
    readPackageDescription normal path

  let pd' = pd { PackageDescription.author = author
               , PackageDescription.maintainer = email
               }

  writeGenericPackageDescription path gPkgDesc { PackageDescription.packageDescription = pd' }

runAfterCommands :: Option -> IO ()
runAfterCommands Option {directoryName, afterCommands} =
    void $ inDirectory directoryName (forM_ afterCommands (void . system))
