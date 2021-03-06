module Paths_tcc (
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

bindir     = "/Users/G4BB3R/haskell/tcc/.stack-work/install/x86_64-osx/lts-6.30/7.10.3/bin"
libdir     = "/Users/G4BB3R/haskell/tcc/.stack-work/install/x86_64-osx/lts-6.30/7.10.3/lib/x86_64-osx-ghc-7.10.3/tcc-0.1.0.0-0DYtMuJwyzE3HdbpxHyrlT"
datadir    = "/Users/G4BB3R/haskell/tcc/.stack-work/install/x86_64-osx/lts-6.30/7.10.3/share/x86_64-osx-ghc-7.10.3/tcc-0.1.0.0"
libexecdir = "/Users/G4BB3R/haskell/tcc/.stack-work/install/x86_64-osx/lts-6.30/7.10.3/libexec"
sysconfdir = "/Users/G4BB3R/haskell/tcc/.stack-work/install/x86_64-osx/lts-6.30/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tcc_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tcc_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "tcc_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tcc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tcc_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
