{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_spiral_matrix (
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
version = Version [1,0,0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/dan/exercism/haskell/spiral-matrix/.stack-work/install/x86_64-linux-nopie/lts-8.21/8.0.2/bin"
libdir     = "/home/dan/exercism/haskell/spiral-matrix/.stack-work/install/x86_64-linux-nopie/lts-8.21/8.0.2/lib/x86_64-linux-ghc-8.0.2/spiral-matrix-1.0.0.1-89OP2UjOz6fCKHalmcS2yD"
dynlibdir  = "/home/dan/exercism/haskell/spiral-matrix/.stack-work/install/x86_64-linux-nopie/lts-8.21/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/dan/exercism/haskell/spiral-matrix/.stack-work/install/x86_64-linux-nopie/lts-8.21/8.0.2/share/x86_64-linux-ghc-8.0.2/spiral-matrix-1.0.0.1"
libexecdir = "/home/dan/exercism/haskell/spiral-matrix/.stack-work/install/x86_64-linux-nopie/lts-8.21/8.0.2/libexec"
sysconfdir = "/home/dan/exercism/haskell/spiral-matrix/.stack-work/install/x86_64-linux-nopie/lts-8.21/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "spiral_matrix_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "spiral_matrix_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "spiral_matrix_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "spiral_matrix_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "spiral_matrix_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "spiral_matrix_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
