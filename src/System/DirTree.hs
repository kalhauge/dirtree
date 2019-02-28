{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-|
Module      : System.DirTree
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

A directory tree, with helper functions to do different cool stuff.

-}
module System.DirTree
 (

 -- * DirTree
   DirTree (..)

 , file
 , symlink
 , directory

 -- ** Folds
 -- These function are used for folding over the DirTree

 , mapDirTree
 , traverseDirTree
 , flatten

 , foldDirTree

 -- ** Utils

 , depthfirst
 , findFile
 , listFiles
 , forgetOrder


 -- ** IO operations
 -- These functions can be used to read a DirTree from the file system.
 , readDirTree
 , lazyReadDirTree

 , writeDirTree

 , readFiles

 , followLinks
 , followLinksLimit


 -- * DirTreeNode
 , DirTreeNode (..)
 , FileType
 , fileTypeOfNode

 -- ** Folds
 , mapDirTreeNode
 , foldDirTreeNode

 -- ** IO operations
 , getFileType
 , readPath
 , writePathWith

 -- * Helpers
 , DirTreeN
 , FileMap
 ) where

-- containers
import qualified Data.Map          as Map
import qualified Data.Set          as Set
import qualified Data.List         as List

-- deepseq
import           Control.DeepSeq

-- directory
import           System.Directory  hiding (findFile)

-- filepath
import           System.FilePath

-- void
import           Data.Void

-- mtl
import           Control.Monad.Reader


-- base
import           Data.Foldable
import           Data.Semigroup
import           GHC.Generics
import           System.IO.Unsafe

-- * DirTree

-- | A dir tree is a tree of nodes.
newtype DirTree s a = DirTree
  { dirTreeNode :: DirTreeN s a
  }
  deriving (Show, Eq, Ord, NFData, Generic)

-- | A `DirTreeN` is a `DirTreeNode` with a the directory as a recursive
-- DirTree.
type DirTreeN s a = DirTreeNode (FileMap (DirTree s a)) s a

type FileMap a = [(String, a)]

instance Semigroup (DirTree s a) where
  DirTree (Directory as) <> DirTree (Directory bs) =
    DirTree (Directory (reverse . nubWith (<>) . reverse $ as <> bs))
  _ <> a = a

instance Functor (DirTree s) where
  fmap = mapDirTree id

instance Foldable (DirTree s) where
  foldMap f = foldDirTree (foldDirTreeNode (foldMap snd) (const mempty) f)

instance Traversable (DirTree s) where
  traverse = traverseDirTree pure

-- ** Constructors

-- | Constructs a dirtree with only a file
file :: a -> DirTree s a
file = DirTree . File

-- | Constructs a dirtree with a symlink
symlink :: s -> DirTree s a
symlink = DirTree . Symlink

-- | Constructs a dirtree with a directory
directory :: [(String, DirTree s a)] -> DirTree s a
directory = DirTree . Directory

-- ** Helpers

-- | Maps over a dir tree
mapDirTree :: (s -> s') -> (a -> a') -> DirTree s a -> DirTree s' a'
mapDirTree fs fa =
    DirTree
    . mapDirTreeNode (map.fmap.mapDirTree fs $ fa) fs fa
    . dirTreeNode

-- | Traverse a DirTree
traverseDirTree ::
  Applicative m
  => (s -> m s') -> (a -> m a')
  -> DirTree s a -> m (DirTree s' a')
traverseDirTree fs fa =
  fmap DirTree
  . traverseDirTreeNode (traverse.traverse.traverseDirTree fs $ fa) fs fa
  . dirTreeNode

-- | Folds over a dirtree
foldDirTree :: (DirTreeNode (FileMap m) s a -> m) -> DirTree s a -> m
foldDirTree f =
  f . mapDirTreeNode (fmap . fmap $ foldDirTree f) id id . dirTreeNode

-- | Flatten a directory tree. This is usefull for following symlinks, or
-- expanding zip-files.
flatten ::
  (s -> DirTree s' a')
  -> (a -> DirTree s' a')
  -> DirTree s a
  -> DirTree s' a'
flatten s a =
  foldDirTree (foldDirTreeNode directory s a)

-- * Utils

-- | Recursively iterate over a folder.
depthfirst :: Monoid m => FilePath -> (FilePath -> DirTreeNode [String] v a -> m) -> DirTree v a -> m
depthfirst basefile fm =
  flip runReader basefile . foldDirTree
  (\file' -> do
      fp <- ask
      case file' of
        Directory files -> do
          let x = fm fp (Directory (map fst files))
          rest <- mapM (\(s, a) -> local (</> s) a) files
          return (x <> fold rest)
        File a  ->
          return $ fm fp (File a)
        Symlink v  ->
          return $ fm fp (Symlink v)
  )

findFile ::
  (FilePath -> DirTreeNode [String] v a -> Bool)
  -> DirTree v a
  -> Maybe (FilePath, DirTreeNode [String] v a)
findFile f =
  fmap getFirst . depthfirst "."
  (curry $ \case
      a | uncurry f a -> Just (First a)
        | otherwise -> Nothing
  )

listFiles :: DirTree v a -> [(FilePath, DirTreeNode [String] v a)]
listFiles =
  flip appEndo [] . depthfirst "." (curry $ Endo . (:))

forgetOrder :: DirTree v a -> DirTree v a
forgetOrder =
  DirTree
  . mapDirTreeNode
  (map (\(s, a) -> (s, forgetOrder a))
   . List.sortOn fst) id id
  . dirTreeNode

-- -- | Reads a DirTree
-- writeDirTreeWith ::
--   (FilePath -> a -> IO ())
--   -> FilePath
--   -> DirTree FilePath a
--   -> IO ()
-- writeDirTreeWith ffile fp =



-- ** IO Methods

-- | Reads a DirTree
readDirTree ::
  FilePath
  -> IO (DirTree FilePath FilePath)
readDirTree fp =
  force <$> lazyReadDirTree fp

-- | Lazy read a DirTree. This function uses `unsafeInterleaveIO` to
-- lazy interleave load a node. This means that it can be used to efficiently
-- search of a file.
lazyReadDirTree ::
  FilePath
  -> IO (DirTree FilePath FilePath)
lazyReadDirTree fp = unsafeInterleaveIO $ do
  node <- readPath fp
  foldDirTreeNode
    (fmap directory . mapM (\s -> (s,) <$> lazyReadDirTree (fp </> s)))
    (return . symlink . (
        \case
          a | isAbsolute a -> a
            | otherwise -> takeDirectory fp </> a
    ))
    (const . return $ file fp)
    node

-- | Follow the links to create the tree. This function might recurse forever.
followLinks :: DirTree FilePath FilePath -> IO (DirTree Void FilePath)
followLinks dt =
  force <$> lazyFollowLinks dt

-- | Like follow links but uses lazy io to only load the recursive folder when
-- needed.
lazyFollowLinks :: DirTree FilePath FilePath -> IO (DirTree Void FilePath)
lazyFollowLinks =
  unsafeInterleaveIO
  . fmap (flatten id file)
  . traverseDirTree
    (followLinks <=< readDirTree)
    pure

-- | Follow the links to create the tree. The first argument is max depth.
-- Give a negative number to possible recurse forever.
followLinksLimit :: Int -> DirTree FilePath FilePath -> IO (DirTree FilePath FilePath)
followLinksLimit 0 = pure
followLinksLimit n =
  fmap (flatten id file)
  . traverseDirTree
    (followLinksLimit (n - 1) <=< readDirTree)
    pure

-- | Read all the files in the `DirTree`. This is just a specialiezed
-- `traverse`
readFiles :: (FilePath -> IO a) -> DirTree v FilePath -> IO (DirTree v a)
readFiles = traverse
{-# INLINE readFiles #-}

-- | Write a `DirTree` to the folder
writeDirTree :: (FilePath -> a -> IO ()) -> FilePath -> DirTree FilePath a -> IO ()
writeDirTree writeFileF fp =
  depthfirst fp (\f -> \case
    Directory _ -> do
      createDirectory f
    Symlink target
      | isAbsolute target -> createFileLink target f
      | otherwise -> createFileLink (makeRelative fp target) f
    File a ->
      writeFileF f a
  )

-- * DirTreeNode

-- | A directory tree node. Everything is either a file, a symbolic link, or a
-- directory.
data DirTreeNode r s a
  = Directory r
  | Symlink s
  | File a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)


type FileType = DirTreeNode () () ()

-- ** Helpers

-- | A DirTreeNode is a weird kind of algebra.
flattenDirTreeNode :: DirTreeNode m m m -> m
flattenDirTreeNode = \case
  File m -> m
  Symlink m -> m
  Directory m -> m

-- | We can map over a DirTreeNode
mapDirTreeNode ::
  (r -> r') -> (s -> s') -> (a -> a')
  -> DirTreeNode r s a
  -> DirTreeNode r' s' a'
mapDirTreeNode fr fs fa = \case
  File a -> File $ fa a
  Symlink s -> Symlink $ fs s
  Directory r -> Directory $ fr r

-- | We can fold over a DirTreeNode by providing a function for each case.
foldDirTreeNode :: (r -> m) -> (s -> m) -> (a -> m) -> DirTreeNode r s a -> m
foldDirTreeNode fr fs fa =
  flattenDirTreeNode . mapDirTreeNode fr fs fa

-- | We can fold over a DirTreeNode by providing a function for each case.
traverseDirTreeNode ::
  Functor m
  =>
  (r -> m r') -> (s -> m s') -> (a -> m a')
  -> DirTreeNode r s a -> m (DirTreeNode r' s' a')
traverseDirTreeNode fr fs fa =
  flattenDirTreeNode . mapDirTreeNode
    (fmap Directory . fr)
    (fmap Symlink . fs)
    (fmap File . fa)


fileTypeOfNode :: DirTreeNode a b c -> FileType
fileTypeOfNode = mapDirTreeNode (const ()) (const ()) (const ())

-- ** IO Methods

-- | Check a filepath for Type, throws an IOException if path does not exist.
getFileType :: FilePath -> IO FileType
getFileType fp =
  pathIsSymbolicLink fp >>= \case
  True ->
    return $ Symlink ()
  False ->
    doesDirectoryExist fp >>= \case
    True ->
      return $ Directory ()
    False ->
      return $ File ()

-- | Reads the structure of the filepath
readPath ::
  FilePath
  -> IO (DirTreeNode [String] FilePath ())
readPath fp = do
  node <- getFileType fp
  foldDirTreeNode
    (const $ Directory <$> listDirectory fp)
    (const $ Symlink <$> getSymbolicLinkTarget fp)
    (const . return $ File ())
    node

-- | Reads the structure of the filepath
writePathWith ::
  (a -> IO ())
  -> (r -> IO ())
  -> FilePath
  -> (DirTreeNode r FilePath a)
  -> IO ()
writePathWith ffile ffolder fp node = do
  foldDirTreeNode
    (\r -> do
        createDirectory fp
        ffolder r
     )
    (\t -> createFileLink t fp)
    (ffile)
    node

-- * Utils

nubSet :: Ord a => [a] -> [a]
nubSet = go Set.empty
  where
    go s = \case
      [] -> []
      a:as
        | a `Set.member` s -> go s as
        | otherwise -> go (Set.insert a s) as

nubWith :: Ord k => (a -> a -> a) -> [(k, a)] -> [(k, a)]
nubWith fmerge m = do
  key <- nubSet (map fst m)
  return (key, mergedvalues Map.! key)
  where mergedvalues = Map.fromListWith (fmerge) m

data Anchored a = (:/)
  { base    ::  FilePath
  , dirTree :: a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)

