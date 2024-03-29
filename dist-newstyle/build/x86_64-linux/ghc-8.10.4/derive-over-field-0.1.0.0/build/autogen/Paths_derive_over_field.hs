{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_derive_over_field (
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

bindir     = "/home/josephsumabat/.cabal/bin"
libdir     = "/home/josephsumabat/.cabal/lib/x86_64-linux-ghc-8.10.4/derive-over-field-0.1.0.0-inplace"
dynlibdir  = "/home/josephsumabat/.cabal/lib/x86_64-linux-ghc-8.10.4"
datadir    = "/home/josephsumabat/.cabal/share/x86_64-linux-ghc-8.10.4/derive-over-field-0.1.0.0"
libexecdir = "/home/josephsumabat/.cabal/libexec/x86_64-linux-ghc-8.10.4/derive-over-field-0.1.0.0"
sysconfdir = "/home/josephsumabat/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "derive_over_field_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "derive_over_field_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "derive_over_field_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "derive_over_field_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "derive_over_field_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "derive_over_field_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
