module Paths_rest_api (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/briansunter/Code/haskell/rest-api/.cabal-sandbox/bin"
libdir     = "/Users/briansunter/Code/haskell/rest-api/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.1/resta_8zoHmEKIztlHmnBgvpFY7G"
datadir    = "/Users/briansunter/Code/haskell/rest-api/.cabal-sandbox/share/x86_64-osx-ghc-7.10.1/rest-api-0.1.0.0"
libexecdir = "/Users/briansunter/Code/haskell/rest-api/.cabal-sandbox/libexec"
sysconfdir = "/Users/briansunter/Code/haskell/rest-api/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "rest_api_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "rest_api_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "rest_api_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "rest_api_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "rest_api_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
