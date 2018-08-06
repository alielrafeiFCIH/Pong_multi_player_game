{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_pro (
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

bindir     = "/home/alielrafei/pro/.cabal-sandbox/bin"
libdir     = "/home/alielrafei/pro/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2/pro-0.1.0.0"
dynlibdir  = "/home/alielrafei/pro/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/alielrafei/pro/.cabal-sandbox/share/x86_64-linux-ghc-8.0.2/pro-0.1.0.0"
libexecdir = "/home/alielrafei/pro/.cabal-sandbox/libexec"
sysconfdir = "/home/alielrafei/pro/.cabal-sandbox/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "pro_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "pro_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "pro_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "pro_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pro_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pro_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
