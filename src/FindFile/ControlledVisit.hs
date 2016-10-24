module FindFile.ControlledVisit where

import System.Directory (Permissions(..), getDirectoryContents, searchable)
import Data.Time.Clock (UTCTime(..))
import System.FilePath

Data Info = Info {
  infoPath :: FilePath
  , infoPerms :: Maybe Permissions
  , infoSize :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  } deriving (Eq, Ord, Show)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
      then traverse order (infoPath info)
      else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names )

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
