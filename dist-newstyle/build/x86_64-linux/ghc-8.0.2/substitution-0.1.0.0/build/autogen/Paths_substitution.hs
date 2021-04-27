{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_substitution (
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

bindir     = "/home/gdupont/.cabal/bin"
libdir     = "/home/gdupont/.cabal/lib/x86_64-linux-ghc-8.0.2/substitution-0.1.0.0-inplace"
dynlibdir  = "/home/gdupont/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/gdupont/.cabal/share/x86_64-linux-ghc-8.0.2/substitution-0.1.0.0"
libexecdir = "/home/gdupont/.cabal/libexec/x86_64-linux-ghc-8.0.2/substitution-0.1.0.0"
sysconfdir = "/home/gdupont/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "substitution_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "substitution_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "substitution_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "substitution_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "substitution_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "substitution_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
