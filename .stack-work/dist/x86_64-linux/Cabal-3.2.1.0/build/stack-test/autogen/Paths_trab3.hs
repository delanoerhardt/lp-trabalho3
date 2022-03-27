{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_trab3 (
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/gabriel/unb/lp/trab3/.stack-work/install/x86_64-linux/5ebff83da90e347bd742df885818c5d10b92f7deee98b9ab3928689a67df501a/8.10.7/bin"
libdir     = "/home/gabriel/unb/lp/trab3/.stack-work/install/x86_64-linux/5ebff83da90e347bd742df885818c5d10b92f7deee98b9ab3928689a67df501a/8.10.7/lib/x86_64-linux-ghc-8.10.7/trab3-0.0.0-GeIktXr9sk8GeyXLWe2zj9-stack-test"
dynlibdir  = "/home/gabriel/unb/lp/trab3/.stack-work/install/x86_64-linux/5ebff83da90e347bd742df885818c5d10b92f7deee98b9ab3928689a67df501a/8.10.7/lib/x86_64-linux-ghc-8.10.7"
datadir    = "/home/gabriel/unb/lp/trab3/.stack-work/install/x86_64-linux/5ebff83da90e347bd742df885818c5d10b92f7deee98b9ab3928689a67df501a/8.10.7/share/x86_64-linux-ghc-8.10.7/trab3-0.0.0"
libexecdir = "/home/gabriel/unb/lp/trab3/.stack-work/install/x86_64-linux/5ebff83da90e347bd742df885818c5d10b92f7deee98b9ab3928689a67df501a/8.10.7/libexec/x86_64-linux-ghc-8.10.7/trab3-0.0.0"
sysconfdir = "/home/gabriel/unb/lp/trab3/.stack-work/install/x86_64-linux/5ebff83da90e347bd742df885818c5d10b92f7deee98b9ab3928689a67df501a/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "trab3_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "trab3_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "trab3_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "trab3_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "trab3_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "trab3_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
