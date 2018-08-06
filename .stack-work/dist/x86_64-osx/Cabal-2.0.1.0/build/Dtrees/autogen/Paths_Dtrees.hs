{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Dtrees (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jonval/WARNING/singularity/Dtrees/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/bin"
libdir     = "/Users/jonval/WARNING/singularity/Dtrees/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/lib/x86_64-osx-ghc-8.2.2/Dtrees-0.1.0.0-B8FXe762Ca42AZHqpEoo1c-Dtrees"
dynlibdir  = "/Users/jonval/WARNING/singularity/Dtrees/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/jonval/WARNING/singularity/Dtrees/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/share/x86_64-osx-ghc-8.2.2/Dtrees-0.1.0.0"
libexecdir = "/Users/jonval/WARNING/singularity/Dtrees/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/libexec/x86_64-osx-ghc-8.2.2/Dtrees-0.1.0.0"
sysconfdir = "/Users/jonval/WARNING/singularity/Dtrees/.stack-work/install/x86_64-osx/lts-10.3/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Dtrees_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Dtrees_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Dtrees_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Dtrees_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Dtrees_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Dtrees_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
