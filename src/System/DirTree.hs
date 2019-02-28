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
module System.DirTree where

-- containers
import qualified Data.Map         as Map
import qualified Data.Set         as Set

-- deepseq
import           Control.DeepSeq

-- directory
import           System.Directory

-- filepath
import           System.FilePath

-- void
import           Data.Void

-- base
import           Control.Monad
import           GHC.Generics

-- * DirTree

newtype DirTree s a = DirTree (DirTreeN s a)
  deriving (Show, Eq, Ord, NFData, Generic)

type DirTreeNode' r = DirTreeNode [(String, r)]

type DirTreeN s a = DirTreeNode' (DirTree s a) s a

instance Semigroup (DirTree s a) where
  DirTree (Directory as) <> DirTree (Directory bs) =
    DirTree (Directory (reverse . nubWith (<>) . reverse $ as <> bs))
  _ <> a = a

instance Functor (DirTree s) where
  fmap = mapDirTree id

instance Foldable (DirTree s) where
  foldMap = foldDirTree (foldMap snd) (const mempty)

instance Traversable (DirTree s) where
  traverse = traverseDirTree pure

-- | Get the underlying dirTreeNode
dirTreeNode :: DirTree s a -> DirTreeN s a
dirTreeNode (DirTree a) = a

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

-- | Folds over a dirtree
foldDirTree :: ([(String, m)] -> m) -> (s -> m) -> (a -> m) -> DirTree s a -> m
foldDirTree fr fs fa =
  foldDirTreeNode (fr . fmap (fmap (foldDirTree fr fs fa))) fs fa
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

-- | Flatten a directory tree. This is usefull for following symlinks, or
-- expanding zip-files.
flatten ::
  (s -> DirTree s' a')
  -> (a -> DirTree s' a')
  -> DirTree s a
  -> DirTree s' a'
flatten =
  foldDirTree directory

-- ** IO Methods

-- | Reads a DirTree
readDirTree ::
  FilePath
  -> IO (DirTree FilePath FilePath)
readDirTree fp = do
  node <- readPath fp
  foldDirTreeNode
    (fmap directory . mapM (\s -> (s,) <$> readDirTree (fp </> s)))
    (return . symlink . (\case a | isAbsolute a -> a | otherwise -> takeDirectory fp </> a))
    (const . return $ file fp)
    node

-- -- | Reads a DirTree
-- writeDirTreeWith ::
--   (FilePath -> a -> IO ())
--   -> FilePath
--   -> DirTree FilePath a
--   -> IO ()
-- writeDirTreeWith ffile fp =

-- | Follow the links to create the tree. This function might recurse forever.
followLinks :: DirTree FilePath FilePath -> IO (DirTree Void FilePath)
followLinks =
  fmap (flatten id file)
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

-- * DirTreeNode

-- | A directory tree node. Everything is either a file, a symbolic link, or a
-- directory.
data DirTreeNode r s a
  = Directory r
  | Symlink s
  | File a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)

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

-- ** IO Methods

-- | Check a filepath for Type, throws an IOException if path does not exist.
checkPath :: FilePath -> IO (DirTreeNode () () ())
checkPath fp =
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
  node <- checkPath fp
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

