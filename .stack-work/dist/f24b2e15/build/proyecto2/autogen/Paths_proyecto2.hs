{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_proyecto2 (
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
bindir     = "C:\\Users\\chedr\\OneDrive\\Desktop\\Materias de la U\\Lenguajes de programaci\243n\\Proyecto2LP\\.stack-work\\install\\1387d796\\bin"
libdir     = "C:\\Users\\chedr\\OneDrive\\Desktop\\Materias de la U\\Lenguajes de programaci\243n\\Proyecto2LP\\.stack-work\\install\\1387d796\\lib\\x86_64-windows-ghc-9.8.4\\proyecto2-0.1.0.0-DJyml4fqOrSBi9rGn24yor-proyecto2"
dynlibdir  = "C:\\Users\\chedr\\OneDrive\\Desktop\\Materias de la U\\Lenguajes de programaci\243n\\Proyecto2LP\\.stack-work\\install\\1387d796\\lib\\x86_64-windows-ghc-9.8.4"
datadir    = "C:\\Users\\chedr\\OneDrive\\Desktop\\Materias de la U\\Lenguajes de programaci\243n\\Proyecto2LP\\.stack-work\\install\\1387d796\\share\\x86_64-windows-ghc-9.8.4\\proyecto2-0.1.0.0"
libexecdir = "C:\\Users\\chedr\\OneDrive\\Desktop\\Materias de la U\\Lenguajes de programaci\243n\\Proyecto2LP\\.stack-work\\install\\1387d796\\libexec\\x86_64-windows-ghc-9.8.4\\proyecto2-0.1.0.0"
sysconfdir = "C:\\Users\\chedr\\OneDrive\\Desktop\\Materias de la U\\Lenguajes de programaci\243n\\Proyecto2LP\\.stack-work\\install\\1387d796\\etc"

getBinDir     = catchIO (getEnv "proyecto2_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "proyecto2_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "proyecto2_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "proyecto2_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "proyecto2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "proyecto2_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
