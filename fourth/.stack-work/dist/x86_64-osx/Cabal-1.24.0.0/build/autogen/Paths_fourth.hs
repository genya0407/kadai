{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_fourth (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/sangenya/dev/eec/fourth/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/bin"
libdir     = "/Users/sangenya/dev/eec/fourth/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/lib/x86_64-osx-ghc-8.0.1/fourth-0.1.0.0-5jTFd9yBlWTDU9nDbbWYxs"
datadir    = "/Users/sangenya/dev/eec/fourth/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/share/x86_64-osx-ghc-8.0.1/fourth-0.1.0.0"
libexecdir = "/Users/sangenya/dev/eec/fourth/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/libexec"
sysconfdir = "/Users/sangenya/dev/eec/fourth/.stack-work/install/x86_64-osx/lts-7.14/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "fourth_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "fourth_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "fourth_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "fourth_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "fourth_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
