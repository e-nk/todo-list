{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_todo_list (
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
bindir     = "/home/enock/Development/haskell/ace/todo-list/.stack-work/install/x86_64-linux-tinfo6/a99b7a60b6c023e3e449b225ca3933fe11ed1618e0349746d94f0b9ca1decfd6/9.6.6/bin"
libdir     = "/home/enock/Development/haskell/ace/todo-list/.stack-work/install/x86_64-linux-tinfo6/a99b7a60b6c023e3e449b225ca3933fe11ed1618e0349746d94f0b9ca1decfd6/9.6.6/lib/x86_64-linux-ghc-9.6.6/todo-list-0.1.0.0-AqqfVtIk9jb1aELhGTjj9P-todo-list-exe"
dynlibdir  = "/home/enock/Development/haskell/ace/todo-list/.stack-work/install/x86_64-linux-tinfo6/a99b7a60b6c023e3e449b225ca3933fe11ed1618e0349746d94f0b9ca1decfd6/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/enock/Development/haskell/ace/todo-list/.stack-work/install/x86_64-linux-tinfo6/a99b7a60b6c023e3e449b225ca3933fe11ed1618e0349746d94f0b9ca1decfd6/9.6.6/share/x86_64-linux-ghc-9.6.6/todo-list-0.1.0.0"
libexecdir = "/home/enock/Development/haskell/ace/todo-list/.stack-work/install/x86_64-linux-tinfo6/a99b7a60b6c023e3e449b225ca3933fe11ed1618e0349746d94f0b9ca1decfd6/9.6.6/libexec/x86_64-linux-ghc-9.6.6/todo-list-0.1.0.0"
sysconfdir = "/home/enock/Development/haskell/ace/todo-list/.stack-work/install/x86_64-linux-tinfo6/a99b7a60b6c023e3e449b225ca3933fe11ed1618e0349746d94f0b9ca1decfd6/9.6.6/etc"

getBinDir     = catchIO (getEnv "todo_list_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "todo_list_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "todo_list_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "todo_list_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "todo_list_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "todo_list_sysconfdir") (\_ -> return sysconfdir)



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
