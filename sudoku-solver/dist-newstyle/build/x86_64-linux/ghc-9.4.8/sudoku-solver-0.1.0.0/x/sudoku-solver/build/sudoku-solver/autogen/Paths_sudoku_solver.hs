{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_sudoku_solver (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/varun/.cabal/bin"
libdir     = "/home/varun/.cabal/lib/x86_64-linux-ghc-9.4.8/sudoku-solver-0.1.0.0-inplace-sudoku-solver"
dynlibdir  = "/home/varun/.cabal/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/home/varun/.cabal/share/x86_64-linux-ghc-9.4.8/sudoku-solver-0.1.0.0"
libexecdir = "/home/varun/.cabal/libexec/x86_64-linux-ghc-9.4.8/sudoku-solver-0.1.0.0"
sysconfdir = "/home/varun/.cabal/etc"

getBinDir     = catchIO (getEnv "sudoku_solver_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "sudoku_solver_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "sudoku_solver_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "sudoku_solver_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "sudoku_solver_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "sudoku_solver_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
