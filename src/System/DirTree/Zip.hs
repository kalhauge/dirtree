{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-|
Module      : System.DirTree.Zip
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

Enables reading and writeing zipfiles using dirtrees. It is
not a complete 1-1 mapping but for many usecases it gets the
job done.

It is based of the `zip-archive` library, which can be used
if more control is needed.
-}

module System.DirTree.Zip
  (
    entriesToFileMap
  , entriesFromFileMap

  -- * Helpers
  , entryToFileMap
  , entryFromFile

  , files
  , entries
    -- * Re-Exports
  , toArchive
  , fromArchive
  )
where

-- base
import Data.Foldable
import Data.Maybe
import Data.Bits
import System.Posix.Files (symbolicLinkMode, stdFileMode)

-- lens
import Control.Lens

-- zip-archive
import Codec.Archive.Zip

-- dirtree
import System.DirTree

-- bytestring
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC


-- | Convert a entry to a single filemap, fails if the entry path is empty.
entryToFileMap ::
  Entry
  -> Maybe (FileMap (DirTree Entry))
entryToFileMap e =
  case fileKeyFromPath $ eRelativePath e of
    k : rest -> Just $ singletonFileMap k (createDeepFile rest e)
    [] -> Nothing

-- | Convert entries to a 'FileMap' of 'RelativeDirTree'
entriesToFileMap ::
  [Entry]
  -> Maybe (FileMap (RelativeDirTree Link BL.ByteString))
entriesToFileMap =
  fmap (imap (\k -> imap $ parseEntry . (k:)) . fold)
  . traverse entryToFileMap
  where
    parseEntry key e =
      case symbolicLinkEntryTarget e of
        Just f -> Symlink . toLink key $ f
        Nothing -> Real $ fromEntry e

-- | Create a single entry from a file. This also handles symlinks, but changes
-- saves all files with the 'stdFileMode'.
entryFromFile :: Integer -> FileKey -> RelativeFile Link BL.ByteString -> Entry
entryFromFile i key = \case
  Real bs -> toEntry (fileKeyToPath key) i bs
  Symlink x ->
    toSymlinkEntry (fileKeyToPath key) $ case x of
      Internal trgt -> diffFileKey (init key) trgt
      External f -> f
  where
    toSymlinkEntry path t =
      let e = toEntry path i (BLC.pack t)
      in e { eExternalFileAttributes =
             eExternalFileAttributes e .|. shiftL (fromIntegral ( symbolicLinkMode .|. stdFileMode)) 16
           , eVersionMadeBy = 798 -- Random high number
           }

-- | Create a list of enties from a FileMap.
entriesFromFileMap ::
  Integer
  -> FileMap (RelativeDirTree Link BL.ByteString)
  -> [Entry]
entriesFromFileMap i =
  concat . imap (\k -> map (uncurry $ entryFromFile i . (k:)) . itoList)

-- | A simple lens into the entries of an archive.
entries :: Lens' Archive [Entry]
entries = lens zEntries (\a b -> a { zEntries = b })

-- | A list of entries can be seen as a FileMap of
entriesAsFileMap :: Integer -> Iso' [Entry] (FileMap (RelativeDirTree Link BL.ByteString))
entriesAsFileMap i = iso from' to' where
  from' = fromJust . entriesToFileMap
  to'   = entriesFromFileMap i

-- | A lens to get and set the files of an archive. Uses sparingly on
-- big archvies as it will convert forth and back.
files :: Lens' Archive (FileMap (RelativeDirTree Link BL.ByteString))
files = entries . entriesAsFileMap 0
