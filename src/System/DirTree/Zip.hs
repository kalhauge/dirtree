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
  ( entriesToDirForest
  , entriesFromDirForest

  -- * Helpers
  , entryToDirForest
  , entryFromFile
  , files
  , entries
    -- * Re-Exports
  , toArchive
  , fromArchive
  )
where


-- base
import           Data.Foldable
import           Data.Maybe
import           Data.Bits
import           System.Posix.Files             ( symbolicLinkMode
                                                , stdFileMode
                                                )

-- lens
import           Control.Lens

-- zip-archive
import           Codec.Archive.Zip

-- dirtree
import           System.DirTree

-- bytestring
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BLC


-- | Convert a entry to a single filemap, fails if the entry path is empty.
entryToDirForest :: Entry -> Maybe (DirForest Entry)
entryToDirForest e = flip createDeepForest (file e)
  <$> toForestFileKey (fileKeyFromPath (eRelativePath e))

-- | Convert entries to a 'FileMap' of 'RelativeDirTree'
entriesToDirForest :: [Entry] -> Maybe (RelativeDirForest Link BL.ByteString)
entriesToDirForest = fmap (imap parseEntry . fold) . traverse entryToDirForest
 where
  parseEntry key e = case symbolicLinkEntryTarget e of
    Just f  -> Symlink . toLink (fromForestFileKey key) $ f
    Nothing -> Real $ fromEntry e

-- | Create a single entry from a file. This also handles symlinks, but changes
-- saves all files with the 'stdFileMode'.
entryFromFile :: Integer -> FileKey -> RelativeFile Link BL.ByteString -> Entry
entryFromFile i key = \case
  Real    bs -> toEntry (fileKeyToPath key) i bs
  Symlink x  -> toSymlinkEntry (fileKeyToPath key) $ case x of
    Internal trgt -> diffFileKey (init key) trgt
    External f    -> f
 where
  toSymlinkEntry path t =
    let e           = toEntry path i (BLC.pack t)
        shiftlength = fromIntegral (symbolicLinkMode .|. stdFileMode)
    in  e
          { eExternalFileAttributes = eExternalFileAttributes e
                                        .|. shiftL shiftlength 16
          , eVersionMadeBy          = 798 -- Random high number
          }

-- | Create a list of enties from a FileMap.
entriesFromDirForest
  :: Integer -> RelativeDirForest Link BL.ByteString -> [Entry]
entriesFromDirForest i = toList . imap (entryFromFile i . fromForestFileKey)

-- | A simple lens into the entries of an archive.
entries :: Lens' Archive [Entry]
entries = lens zEntries (\a b -> a { zEntries = b })

-- | A list of entries can be seen as a FileMap of
entriesAsDirForest
  :: Integer -> Iso' [Entry] (RelativeDirForest Link BL.ByteString)
entriesAsDirForest i = iso from' to' where
  from' = fromJust . entriesToDirForest
  to'   = entriesFromDirForest i

-- | A lens to get and set the files of an archive. Uses sparingly on
-- big archvies as it will convert forth and back.
files :: Lens' Archive (RelativeDirForest Link BL.ByteString)
files = entries . entriesAsDirForest 0
