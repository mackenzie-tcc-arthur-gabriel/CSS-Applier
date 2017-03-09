module Paths_taggyparsinha (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/G4BB3R/.cabal/bin"
libdir     = "/Users/G4BB3R/.cabal/lib/x86_64-osx-ghc-7.10.3/taggyparsinha-0.1.0.0-4DugKrqyQnKHRioR3si1vF"
datadir    = "/Users/G4BB3R/.cabal/share/x86_64-osx-ghc-7.10.3/taggyparsinha-0.1.0.0"
libexecdir = "/Users/G4BB3R/.cabal/libexec"
sysconfdir = "/Users/G4BB3R/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "taggyparsinha_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "taggyparsinha_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "taggyparsinha_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "taggyparsinha_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "taggyparsinha_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
