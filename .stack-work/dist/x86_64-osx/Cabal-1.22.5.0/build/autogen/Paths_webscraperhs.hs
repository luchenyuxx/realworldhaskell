module Paths_webscraperhs (
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

bindir     = "/Users/chenyu/git/webscraperhs/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/bin"
libdir     = "/Users/chenyu/git/webscraperhs/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/lib/x86_64-osx-ghc-7.10.3/webscraperhs-0.1.0.0-BlcTnemthub4yXcgkhZqU2"
datadir    = "/Users/chenyu/git/webscraperhs/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/share/x86_64-osx-ghc-7.10.3/webscraperhs-0.1.0.0"
libexecdir = "/Users/chenyu/git/webscraperhs/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/libexec"
sysconfdir = "/Users/chenyu/git/webscraperhs/.stack-work/install/x86_64-osx/lts-6.16/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "webscraperhs_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "webscraperhs_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "webscraperhs_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "webscraperhs_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "webscraperhs_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
