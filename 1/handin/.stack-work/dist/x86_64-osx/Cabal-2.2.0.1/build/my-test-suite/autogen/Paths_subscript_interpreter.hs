{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_subscript_interpreter (
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

bindir     = "/Users/philiplassen/CS/nicolas-and-philip-go-to-school/1/handin/.stack-work/install/x86_64-osx/lts-12.6/8.4.3/bin"
libdir     = "/Users/philiplassen/CS/nicolas-and-philip-go-to-school/1/handin/.stack-work/install/x86_64-osx/lts-12.6/8.4.3/lib/x86_64-osx-ghc-8.4.3/subscript-interpreter-0.0.0-80MnS2fygAFHeIO7QX58gL-my-test-suite"
dynlibdir  = "/Users/philiplassen/CS/nicolas-and-philip-go-to-school/1/handin/.stack-work/install/x86_64-osx/lts-12.6/8.4.3/lib/x86_64-osx-ghc-8.4.3"
datadir    = "/Users/philiplassen/CS/nicolas-and-philip-go-to-school/1/handin/.stack-work/install/x86_64-osx/lts-12.6/8.4.3/share/x86_64-osx-ghc-8.4.3/subscript-interpreter-0.0.0"
libexecdir = "/Users/philiplassen/CS/nicolas-and-philip-go-to-school/1/handin/.stack-work/install/x86_64-osx/lts-12.6/8.4.3/libexec/x86_64-osx-ghc-8.4.3/subscript-interpreter-0.0.0"
sysconfdir = "/Users/philiplassen/CS/nicolas-and-philip-go-to-school/1/handin/.stack-work/install/x86_64-osx/lts-12.6/8.4.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "subscript_interpreter_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "subscript_interpreter_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "subscript_interpreter_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "subscript_interpreter_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "subscript_interpreter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "subscript_interpreter_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
