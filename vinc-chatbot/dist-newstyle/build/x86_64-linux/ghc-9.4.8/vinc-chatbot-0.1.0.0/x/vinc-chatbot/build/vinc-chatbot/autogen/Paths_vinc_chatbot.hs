{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_vinc_chatbot (
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
bindir     = "/home/fx/.cabal/bin"
libdir     = "/home/fx/.cabal/lib/x86_64-linux-ghc-9.4.8/vinc-chatbot-0.1.0.0-inplace-vinc-chatbot"
dynlibdir  = "/home/fx/.cabal/lib/x86_64-linux-ghc-9.4.8"
datadir    = "/home/fx/.cabal/share/x86_64-linux-ghc-9.4.8/vinc-chatbot-0.1.0.0"
libexecdir = "/home/fx/.cabal/libexec/x86_64-linux-ghc-9.4.8/vinc-chatbot-0.1.0.0"
sysconfdir = "/home/fx/.cabal/etc"

getBinDir     = catchIO (getEnv "vinc_chatbot_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "vinc_chatbot_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "vinc_chatbot_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "vinc_chatbot_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vinc_chatbot_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vinc_chatbot_sysconfdir") (\_ -> return sysconfdir)



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
