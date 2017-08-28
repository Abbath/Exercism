{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_bracket_push (
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
version = Version [1,1,0,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/dan/exercism/haskell/bracket-push/.stack-work/install/x86_64-linux-nopie/lts-8.21/8.0.2/bin"
libdir     = "/home/dan/exercism/haskell/bracket-push/.stack-work/install/x86_64-linux-nopie/lts-8.21/8.0.2/lib/x86_64-linux-ghc-8.0.2/bracket-push-1.1.0.2-IQmMf6wuarA7UVW25giHL"
dynlibdir  = "/home/dan/exercism/haskell/bracket-push/.stack-work/install/x86_64-linux-nopie/lts-8.21/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/dan/exercism/haskell/bracket-push/.stack-work/install/x86_64-linux-nopie/lts-8.21/8.0.2/share/x86_64-linux-ghc-8.0.2/bracket-push-1.1.0.2"
libexecdir = "/home/dan/exercism/haskell/bracket-push/.stack-work/install/x86_64-linux-nopie/lts-8.21/8.0.2/libexec"
sysconfdir = "/home/dan/exercism/haskell/bracket-push/.stack-work/install/x86_64-linux-nopie/lts-8.21/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bracket_push_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bracket_push_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "bracket_push_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "bracket_push_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bracket_push_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bracket_push_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
