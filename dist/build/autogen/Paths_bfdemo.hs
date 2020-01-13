module Paths_bfdemo (
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

bindir     = "/home/mikael/.cabal/bin"
libdir     = "/home/mikael/.cabal/lib/x86_64-linux-ghc-7.10.3/bfdemo-0.1.0.0-84LkKjaNOlNKxcJ0RWKCiJ"
datadir    = "/home/mikael/.cabal/share/x86_64-linux-ghc-7.10.3/bfdemo-0.1.0.0"
libexecdir = "/home/mikael/.cabal/libexec"
sysconfdir = "/home/mikael/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bfdemo_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bfdemo_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bfdemo_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bfdemo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bfdemo_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
