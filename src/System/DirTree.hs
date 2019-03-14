{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
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

 , fromFiles
 , fromFile
 , toFiles

 -- ** Accessors
 , FileKey
 , fileKeyToPath
 , fileKeyFromPath

 , lookupFile

 -- ** Traversals
 -- These function are used for folding over the DirTree

 , traverseDirTree
 , traverseDirTree'
 , itraverseDirTree
 , itraverseDirTree'

 , mapDirTree'
 , imapDirTree'

 , depthfirst
 , foldDirTree
 , foldDirTree'
 , ifoldDirTree
 , ifoldDirTree'

 , flatten

 -- ** Utils
 , findNode
 , listNodes
 , forgetOrder

 -- ** IO operations
 -- These functions can be used to read a DirTree from the file system.
 , readDirTree
 , lazyReadDirTree

 , writeDirTree

 , followLinks
 , lazyFollowLinks

 -- * DirTreeNode
 , Link (..)
 , DirTreeNode (..)
 , FileType
 , fileTypeOfNode

 -- ** Folds
 , mapDirTreeNode
 , foldDirTreeNode
 , traverseDirTreeNode

 -- ** IO operations
 , getFileType
 , readPath

 -- * Helpers
 , DirTreeN

 -- * FileMap
 , FileMap (..)
 , toFileList
 , fromFileList
 , toFileNames
 , lookupFileMap
 ) where

-- containers
import qualified Data.List                as List
import qualified Data.List.NonEmpty       as NonEmpty
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set

-- deepseq
import           Control.DeepSeq

-- directory
import           System.Directory         hiding (findFile)

-- filepath
import           System.FilePath

-- lens
import           Control.Lens.Combinators
-- import           Control.Lens.Indexed

-- base
import           Data.Foldable
import           Data.Semigroup
import           Data.Void
import           GHC.Generics
import           System.IO.Unsafe
-- * DirTree

-- | A dir tree is a tree of nodes.
newtype DirTree s a = DirTree
  { dirTreeNode :: DirTreeN s a
  }
  deriving (Eq, Ord, NFData, Generic)

instance (Show v, Show c) => Show (DirTree v c) where
  showsPrec d c = showParen (d >9) $ (f $ dirTreeNode c)
    where
    f = \case
      Directory (FileMap a) ->
        showString "directory " . showsPrec 11 a
      Symlink a ->
        showString "symlink " . showsPrec 11 a
      File a ->
        showString "file " . showsPrec 11 a

-- | A `DirTreeN` is a `DirTreeNode` with a the directory as a recursive
-- DirTree.
type DirTreeN s a = DirTreeNode (FileMap (DirTree s a)) s a

instance Semigroup (DirTree s a) where
  DirTree (Directory as) <> DirTree (Directory bs) =
    DirTree (Directory (as <> bs))
  _ <> a = a

instance Functor (DirTree s) where
  fmap = mapDirTree' id

instance Foldable (DirTree s) where
  foldMap = foldDirTree' (const mempty)

instance Traversable (DirTree s) where
  traverse = traverseDirTree' pure

instance FunctorWithIndex FileKey (DirTree v)
instance FoldableWithIndex FileKey (DirTree v)
instance TraversableWithIndex FileKey (DirTree v) where
  itraverse = itraverseDirTree' (const pure)
  {-# INLINE itraverse #-}

-- ** Constructors

-- | Constructs a dirtree with only a file
file :: a -> DirTree s a
file = DirTree . File

-- | Constructs a dirtree with a symlink
symlink :: s -> DirTree s a
symlink = DirTree . Symlink

-- | Constructs a dirtree with a directory
directory :: [(String, DirTree s a)] -> DirTree s a
directory = DirTree . Directory . FileMap

-- ** Accessors

-- | A filekey is the filepath in reverse order
type FileKey = [String]

-- | Get a `FileKey` from a `FilePath`
fileKeyFromPath :: FilePath -> FileKey
fileKeyFromPath =
  reverse . splitDirectories

-- | Get a `FilePath` from a `FileKey`
fileKeyToPath :: FileKey -> FilePath
fileKeyToPath =
  joinPath . reverse

diffFileKey :: FileKey -> FileKey -> FilePath
diffFileKey f to' =
  let (n, bs) = (suffix f to')
  in fileKeyToPath (bs ++ replicate n "..")
  where
    suffix (a:as) (b:bs)
      | a /= b =
        (1 + length as, b:bs)
      | otherwise =
        suffix as bs
    suffix (_:as) [] =
      (1 + length as, [])
    suffix [] bs =
      (0, bs)

-- | Lookup a file in a `DirTree` using a `FileKey`
lookupFile :: FileKey -> DirTree v a -> Maybe (DirTree v a)
lookupFile fk = go (reverse fk)
  where
    go [] tree = Just tree
    go (a:rest) (DirTree (Directory x)) =
      go rest =<< lookupFileMap a x
    go _ _ = Nothing
{-# inline lookupFile #-}

toFiles :: DirTree v a -> [(FileKey, a)]
toFiles = itoList
{-# INLINE toFiles #-}

-- | Create a dirtree from a non-empty list of files.
fromFiles :: NonEmpty.NonEmpty (FileKey, a) -> DirTree Void a
fromFiles =
  sconcat . fmap (uncurry fromFile)
{-# INLINE fromFiles #-}

fromFile :: FileKey -> a -> DirTree Void a
fromFile key a =
  foldr (\s f -> directory [(s, f)]) (file a) key

-- ** Helpers

-- | Traverse over the tree
itraverseDirTree ::
  Applicative f
  => ( FileKey -> DirTreeNode (FileMap (f (DirTree s' a'))) s a -> f (DirTreeN s' a'))
  -> DirTree s a
  -> f (DirTree s' a')
itraverseDirTree f = go []
  where
    go x (DirTree fs) = fmap DirTree . f x $
      case fs of
        Directory fm ->
          Directory $ imap (\s a -> go (s:x) a) fm
        Symlink a -> Symlink a
        File a -> File a
{-# inline itraverseDirTree #-}

-- | Traverse over the tree with index. This method uses two functions one
-- symlinks and one for files.
itraverseDirTree' ::
  Applicative f
  => (FileKey -> s -> f s') -> (FileKey -> a -> f a')
  -> DirTree s a
  -> f (DirTree s' a')
itraverseDirTree' fs fa =
  itraverseDirTree
  (\key -> \case
    Directory fm ->
      Directory <$> traverse id fm
    Symlink a -> Symlink <$> fs key a
    File a -> File <$> fa key a
  )
{-# inline itraverseDirTree' #-}

-- | Maps over a `DirTree`
imapDirTree' :: (FileKey -> s -> s') -> (FileKey -> a -> a') -> DirTree s a -> DirTree s' a'
imapDirTree' fs fa =
  runIdentity . itraverseDirTree' (\i -> Identity . fs i) (\i -> Identity . fa i)
{-# inline imapDirTree' #-}

-- | Folds over a `DirTree`.
ifoldDirTree' :: Monoid m => (FileKey -> s -> m) -> (FileKey -> a -> m) -> DirTree s a -> m
ifoldDirTree' fs fa =
  ifoldDirTree (\i -> foldDirTreeNode fold (fs i) (fa i))
{-# inline ifoldDirTree' #-}

-- | Folds over a `DirTree` using the `DirTreeNode`.
ifoldDirTree :: (FileKey -> DirTreeNode (FileMap m) s a -> m) -> DirTree s a -> m
ifoldDirTree f = go []
  where
    go x (DirTree fs) = f x $
      case fs of
        Directory fm ->
          Directory $ imap (\s a -> go (s:x) a) fm
        Symlink a -> Symlink a
        File a -> File a
{-# inline ifoldDirTree #-}

-- | Traverse a DirTree
traverseDirTree ::
  Applicative f
  => (DirTreeNode (FileMap (f (DirTree s' a'))) s a -> f (DirTreeN s' a'))
  -> DirTree s a
  -> f (DirTree s' a')
traverseDirTree fm =
  itraverseDirTree (const fm)
{-# inline traverseDirTree #-}

-- | Traverse a DirTree
traverseDirTree' ::
  Applicative m
  => (s -> m s') -> (a -> m a')
  -> DirTree s a -> m (DirTree s' a')
traverseDirTree' fs fa =
  itraverseDirTree' (const fs) (const fa)
{-# inline traverseDirTree' #-}

-- | Folds over a dirtree
foldDirTree :: (DirTreeNode (FileMap m) s a -> m) -> DirTree s a -> m
foldDirTree f =
  ifoldDirTree (const f)
{-# inline foldDirTree #-}

-- | Folds over a dirtree
foldDirTree' :: Monoid m => (s -> m) -> (a -> m) -> DirTree s a -> m
foldDirTree' fs fa =
  ifoldDirTree' (const fs) (const fa)
{-# inline foldDirTree' #-}

-- | maps over a dirtree
mapDirTree' :: (s -> s') -> (a -> a') -> DirTree s a -> DirTree s' a'
mapDirTree' fs fa =
  imapDirTree' (const fs) (const fa)
{-# inline mapDirTree' #-}

-- | Flatten a directory tree. This is usefull for following symlinks, or
-- expanding zip-files.
flatten ::
  (s -> DirTree s' a')
  -> (a -> DirTree s' a')
  -> DirTree s a
  -> DirTree s' a'
flatten s a =
  foldDirTree (foldDirTreeNode (directory . toFileList) s a)
{-# inline flatten #-}

-- * Utils

-- | Recursively iterate over a folder.
depthfirst ::
  Monoid m
  =>(FileKey -> DirTreeNode [String] v a -> m)
  -> DirTree v a
  -> m
depthfirst fm =
  ifoldDirTree $ \key file' ->
    case file' of
      Directory files -> do
        fm key (Directory $ toFileNames files) <> fold files
      File a  ->
        fm key (File a)
      Symlink v  ->
        fm key (Symlink v)
{-# inline depthfirst #-}

-- | Find a file given a predicate that takes a `FileKey` and `DirTreeNode`.
findNode ::
  (FileKey -> DirTreeNode [String] v a -> Bool)
  -> DirTree v a
  -> Maybe (FileKey, DirTreeNode [String] v a)
findNode f =
  fmap getFirst . depthfirst
  (curry $ \case
      a | uncurry f a -> Just (First a)
        | otherwise -> Nothing
  )
{-# inline findNode #-}

-- | List all the nodes in the `DirTree`.
listNodes :: DirTree v a -> [(FileKey, DirTreeNode [String] v a)]
listNodes =
  flip appEndo [] . depthfirst (curry $ Endo . (:))
{-# inline listNodes #-}

-- | Forget the internal order of forget.
forgetOrder :: DirTree v a -> DirTree v a
forgetOrder =
  DirTree . mapDirTreeNode forgetFileMapOrder id id . dirTreeNode
{-# inline forgetOrder #-}

-- ** IO Methods

-- | A `Link` can either be `Internal`, pointing to something in the `DirTree` or
-- `External` pointing to an absolute `FilePath`.
data Link
  = Internal !FileKey
  | External !FilePath
  deriving (Show, Eq, Generic, NFData)

-- | Reads a DirTree. All file paths are absolute to the filepath
readDirTree ::
  NFData a =>
  (FilePath -> IO a)
  -> FilePath
  -> IO (DirTree Link a)
readDirTree reader' fp = do
  force <$> lazyReadDirTree reader' fp

-- | Lazy read a DirTree. This function uses `unsafeInterleaveIO` to
-- lazy interleave load a node. This means that it can be used to efficiently
-- search of a file. All paths are absolute
lazyReadDirTree ::
  (FilePath -> IO a)
  -> FilePath
  -> IO (DirTree Link a)
lazyReadDirTree reader' basepath = do
  from' <- canonicalizePath basepath
  go from' [] basepath
  where
    go from' key fp = unsafeInterleaveIO $ do
      node <- readPath fp
      foldDirTreeNode
        (fmap directory . mapM (\s -> (s,) <$> go from' (s:key) (fp </> s)))
        (fmap symlink . absolute)
        (const $ file <$> reader' fp)
        node
      where
        absolute a
          | isAbsolute a =
              return $ External a
          | otherwise = do
            a' <- canonicalizePath (takeDirectory fp </> a)
            let a'' =  makeRelative from' a'
            if a'' /= a'
              then return $ Internal (fileKeyFromPath a'')
              else return $ External a'

-- | Reads a DirTree
writeDirTree ::
  (FilePath -> a -> IO ())
  -> FilePath
  -> DirTree Link a
  -> IO ()
writeDirTree writer fp tree = do
  ifoldDirTree
    ( \fk i ->
      let fp' = fp </> fileKeyToPath fk in
      case i of
        Directory m -> do
          createDirectory fp'
          fold m
        Symlink (External target) ->
          createFileLink target fp'
        Symlink (Internal key) ->
          createFileLink
          (case (fk, key) of
              (_:fk',  _) -> diffFileKey fk' key
              ([],    []) -> "."
              ([],     _) -> error "Fail"
          ) fp'
        File a ->
          writer fp' a
    )
    tree

-- | Follow the links to create the tree. This function might recurse forever.
followLinks :: NFData a => (FilePath -> IO a) -> DirTree Link a -> IO (DirTree Void a)
followLinks fio dt =
  force <$> lazyFollowLinks fio dt

-- | Like follow links but uses lazy io to only load the recursive folder when
-- needed.
lazyFollowLinks :: (FilePath -> IO a) -> DirTree Link a -> IO (DirTree Void a)
lazyFollowLinks reader' tree =
  go tree tree
  where
    go basetree =
      unsafeInterleaveIO
      . fmap (flatten id file)
      . traverseDirTree' (readLink basetree) pure

    readLink basetree = \case
      Internal s -> do
        case lookupFile s basetree of
          Just a -> go basetree a
          Nothing ->
            error $ "Could not find " ++ show s ++ " in the dirtree " ++ show (fmap (const ()) tree)
      External s -> do
        t <- lazyReadDirTree reader' s
        lazyFollowLinks reader' t


-- * DirTreeNode

-- | A directory tree node. Everything is either a file, a symbolic link, or a
-- directory.
data DirTreeNode r s a
  = Directory r
  | Symlink s
  | File a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)

-- | A `FileType` is just a `DirTreeNode` with no contents.
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


-- | Gets the `FileType` of a `DirTreeNode`
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

-- | A map from file names to
newtype FileMap a =
  FileMap [(String, a)]
  deriving (Eq, Ord, NFData, Generic, Functor, Foldable, Traversable)

-- | Create a list of pairs of filenames and file values.
toFileList :: FileMap a -> [(String, a)]
toFileList (FileMap a) = a

-- | Create a `FileMap` from a list of pairs of filenames a file values.
fromFileList :: [(String, a)] -> FileMap a
fromFileList = FileMap

-- | To a list of filenames
toFileNames :: FileMap a -> [String]
toFileNames = map fst . toFileList

-- | Lookup a file using a filename
lookupFileMap :: String -> FileMap a -> Maybe a
lookupFileMap s (FileMap a) = List.lookup s a

instance Semigroup a => Semigroup (FileMap a) where
  FileMap as <> FileMap bs =
    FileMap (reverse . nubWith (<>) . reverse $ as <> bs)

instance Semigroup a => Monoid (FileMap a) where
  mempty = FileMap []

instance FunctorWithIndex String FileMap
instance FoldableWithIndex String FileMap
instance TraversableWithIndex String FileMap where
  itraverse f (FileMap fs) = FileMap <$> traverse (itraverse f) fs
  {-# INLINE itraverse #-}

forgetFileMapOrder :: FileMap a -> FileMap a
forgetFileMapOrder (FileMap a) = FileMap (List.sortOn fst a)

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

