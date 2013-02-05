module Paths_uir (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/schovi/Library/Haskell/ghc-7.4.1/lib/uir-0.1/bin"
libdir     = "/Users/schovi/Library/Haskell/ghc-7.4.1/lib/uir-0.1/lib"
datadir    = "/Users/schovi/Library/Haskell/ghc-7.4.1/lib/uir-0.1/share"
libexecdir = "/Users/schovi/Library/Haskell/ghc-7.4.1/lib/uir-0.1/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "uir_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "uir_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "uir_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "uir_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
