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

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

-- liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
-- liftp' q f k w x y z = f w x y z `q` constP k w x y z

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

constP :: a -> InfoP a
constP k _ _ _ _ = k

andP, orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP = liftP2 (||)

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP = liftP (==)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

(==?) :: Eq a => InfoP a -> a -> InfoP Bool
(==?) = equalP

(&&?) ::InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP

(>?) :: Ord a => InfoP a -> a -> InfoP Bool
(>?) = greaterP

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

