module FindFile.ControlledVisit where

import Control.Monad
import System.IO
import System.Directory
import Data.Time.Clock (UTCTime(..))
import System.FilePath
import Control.Exception

data Info = Info {
  infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

traverseF :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseF order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
      then traverseF order (infoPath info)
      else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names )

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing)::IOException -> IO (Maybe a)) (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)
