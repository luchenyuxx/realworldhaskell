module FindFile.BetterPredicate where

import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.Time (ClockTime(..))
import System.FilePath (takeExtension)
import FindFile.RecursiveContents (getRecursiveContents)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

type Predicate = FilePath  -- path to directory entry
               -> Permissions -- permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> ClockTime -- last modified
               -> Bool
