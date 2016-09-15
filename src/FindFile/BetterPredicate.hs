module FindFile.BetterPredicate where

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension)
import FindFile.RecursiveContents (getRecursiveContents)
import Control.Exception (IOException, bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

type Predicate = FilePath  -- path to directory entry
               -> Permissions -- permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> UTCTime -- last modified
               -> Bool

type InfoP a = FilePath -> Permissions -> Maybe Integer -> UTCTime -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP = liftP (==)

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ((\_ -> return Nothing)::IOException -> IO (Maybe Integer)) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
          perms <- getPermissions name
          size <- getFileSize name
          modified <- getModificationTime name
          return (p name perms size modified)

