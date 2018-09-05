{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_exercises (
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

bindir     = "/Users/ale/Learning/haskellbook/chapter15/.stack-work/install/x86_64-osx/lts-12.9/8.4.3/bin"
libdir     = "/Users/ale/Learning/haskellbook/chapter15/.stack-work/install/x86_64-osx/lts-12.9/8.4.3/lib/x86_64-osx-ghc-8.4.3/exercises-0.1.0.0-EmpMtNQhl2X1NtdwJeuwcq-exercises"
dynlibdir  = "/Users/ale/Learning/haskellbook/chapter15/.stack-work/install/x86_64-osx/lts-12.9/8.4.3/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/ale/Learning/haskellbook/chapter15/.stack-work/install/x86_64-osx/lts-12.9/8.4.3/share/x86_64-osx-ghc-8.4.3/exercises-0.1.0.0"
libexecdir = "/Users/ale/Learning/haskellbook/chapter15/.stack-work/install/x86_64-osx/lts-12.9/8.4.3/libexec/x86_64-osx-ghc-8.4.3/exercises-0.1.0.0"
sysconfdir = "/Users/ale/Learning/haskellbook/chapter15/.stack-work/install/x86_64-osx/lts-12.9/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "exercises_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "exercises_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "exercises_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "exercises_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "exercises_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "exercises_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
