{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_worker (
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

bindir     = "/Users/Vadim/Documents/fp/project/.stack-work/install/x86_64-osx/lts-12.26/8.4.4/bin"
libdir     = "/Users/Vadim/Documents/fp/project/.stack-work/install/x86_64-osx/lts-12.26/8.4.4/lib/x86_64-osx-ghc-8.4.4/worker-0.1.0.0-5XkmKKhWOeY2JN2zOnsFeR-worker"
dynlibdir  = "/Users/Vadim/Documents/fp/project/.stack-work/install/x86_64-osx/lts-12.26/8.4.4/lib/x86_64-osx-ghc-8.4.4"
datadir    = "/Users/Vadim/Documents/fp/project/.stack-work/install/x86_64-osx/lts-12.26/8.4.4/share/x86_64-osx-ghc-8.4.4/worker-0.1.0.0"
libexecdir = "/Users/Vadim/Documents/fp/project/.stack-work/install/x86_64-osx/lts-12.26/8.4.4/libexec/x86_64-osx-ghc-8.4.4/worker-0.1.0.0"
sysconfdir = "/Users/Vadim/Documents/fp/project/.stack-work/install/x86_64-osx/lts-12.26/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "worker_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "worker_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "worker_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "worker_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "worker_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "worker_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
