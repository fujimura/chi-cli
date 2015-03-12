{-# LANGUAGE NamedFieldPuns #-}

module Chi
  (
    run
  ) where

import qualified Git
import           Types

import           Control.Applicative
import           Control.Arrow                               (first)
import           Control.Exception                           (bracket_)
import           Control.Monad
import           Data.List                                   (intersperse)
import           Data.List.Split                             (splitOn)
import           Data.Version                                (Version (Version))
import           Distribution.Package                        (PackageIdentifier (PackageIdentifier), PackageName (PackageName))
import           Distribution.PackageDescription             (GenericPackageDescription (GenericPackageDescription))
import qualified Distribution.PackageDescription             as PackageDescription
import           Distribution.PackageDescription.Parse       (readPackageDescription)
import           Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import           Distribution.Simple.Utils                   (findPackageDesc)
import qualified Distribution.Verbosity                      as Verbosity
import           System.Directory
import           System.FilePath                             (dropFileName,
                                                              joinPath,
                                                              splitPath, (</>))
import           System.IO.Temp                              (withSystemTempDirectory)
import           System.Process                              (callCommand,
                                                              system)

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
-- TODO Handle IO exceptions
fetch (CabalPackage p) = inTemporaryDirectory "chi" $ do
    callCommand $ "cabal get " ++ p
    d:_ <- getDirectoryContents "."
    inDirectory d $ do
      paths <- getDirectoryContentsRecursively "."
      map (first dropFirstDirectory) <$> mapM fetchFile paths

dropFirstDirectory :: FilePath -> FilePath
dropFirstDirectory path = go (splitPath path)
  where
    go []     = []
    go (_:[]) = []
    go (x:_:ys) = joinPath (x:ys)

getDirectoryContentsRecursively :: FilePath -> IO [FilePath]
getDirectoryContentsRecursively path = go [path]
  where
    go :: [FilePath] -> IO [FilePath]
    go [] = return []
    go (x:xs) = do
       isDir <- doesDirectoryExist x
       if isDir then do
                  xs' <- filter (/= "..") <$> filter (/= ".") <$> getDirectoryContents x
                  go (xs ++ map (x </>) xs')
                else
                  (x:) <$> go xs

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
                                                    , ("package_name", underscorize packageName)
                                                    , ("ModuleName", moduleName)
                                                    , ("$author", author)
                                                    , ("$email", email)
                                                    , ("$year", year)
                                                    ]
    rewritePath :: FilePath -> FilePath
    rewritePath = addDirectoryName . replacePackageName . replaceModuleName
      where
        addDirectoryName   = (directoryName </>)
        replacePackageName = replace "package-name" packageName
        replaceModuleName  = replace "ModuleName" $ moduleNameToFilePath moduleName

-- | Underscorize hyphenized string
--
-- >>> underscorize "foo-bar"
-- "foo_bar"
--
underscorize :: String -> String
underscorize = replace "-" "_"

-- | Convert module name to path
--
-- >>> moduleNameToFilePath "Foo.Bar"
-- "Foo/Bar"
--
moduleNameToFilePath :: String -> FilePath
moduleNameToFilePath = joinPath . splitOn "."

writeFiles :: [Modified File] -> IO ()
writeFiles = mapM_ write'
  where
    write' (Modified f _) = write f

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace a b = foldl1 (++) . intersperse b . splitOn a

write :: File -> IO ()
write (path,contents) =
    createDirectoryIfMissing True (dropFileName path) >> writeFile path contents

updateCabalFile :: Option -> IO ()
updateCabalFile Option {packageName, directoryName, author, email} = do
  path <- findPackageDesc directoryName
  gPkgDesc@GenericPackageDescription { PackageDescription.packageDescription = pd } <-
    readPackageDescription Verbosity.normal path

  let pid = PackageIdentifier (PackageName packageName) (Version [0,0,1,0] [])
      pd' = pd { PackageDescription.package    = pid
               , PackageDescription.author     = author
               , PackageDescription.maintainer = email
               }

  removeFile path
  writeGenericPackageDescription (directoryName </> packageName ++ ".cabal" ) gPkgDesc { PackageDescription.packageDescription = pd' }

runAfterCommands :: Option -> IO ()
runAfterCommands Option {directoryName, afterCommands} =
    void $ inDirectory directoryName (forM_ afterCommands (void . system))
