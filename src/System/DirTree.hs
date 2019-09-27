{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FunctionalDependencies        #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-|
Module      : System.DirTree
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

A directory tree, with helper functions to do different cool stuff. Contrary to
`directory-tree`, this package does try to add as many accessors and handlers as
possible. This is alos the reason that it depends on the Lens library.

-}
module System.DirTree
 (
   -- * 'DirTreeNode'
   -- $DirTreeNode

   DirTreeNode (..)
 , RelativeFile (..)

   -- ** Helpers
 , FileType
 , fileTypeOfNode

 , AsDirTreeNode (..)
 , AsRelativeFile (..)

   -- ** IO
 , getFileType
 , readPath

   -- * 'FileMap'
   -- $FileMap

 , FileMap (..)

 -- ** Constructors
 , emptyFileMap
 , singletonFileMap
 , toFileList
 , fromFileList
 , (.*), (./), (.*>), (.*.)

 -- ** Accessors
 , lookupFileMap

   -- * 'DirTree'
   -- $DirTree
 , DirTree (..)
 , RelativeDirTree
 , asRelativeDirTree

   -- ** Constructors
 , file
 , realfile
 , symlink
 , directory

 , directory'

 , emptyDirectory

 , createDeepFile
 , createDeepTree

   -- ** Accessors
 , FileKey

 , fileKeyFromPath
 , fileKeyToPath

 , diffFileKey
 , diffPath

 , alterFile

   -- ** Iterators
   -- Most of the iterators can be done with the 'FunctorWithIndex',
   -- 'FoldableWithIndex', and 'TraversableWithIndex', but some accumilations
   -- are easier.
 , iflattenDirTree
 , flattenDirTree

 , depthfirst
 , findNode
 , listNodes

   -- ** IO

 , readDirTree
 , writeDirTree
 , Link (..)
 , toLink
 , readRelativeDirTree
 , followLinks
 , writeRelativeDirTree

 -- * 'DirForest'
 -- $DirForest

 , DirForest (..)
 , RelativeDirForest

 , ForestFileKey
 , fromForestFileKey
 , toForestFileKey

 -- ** Constructors
 , asRelativeDirForest
 , emptyForest
 , singletonForest
 , createDeepForest

 -- ** Iterators
 , alterForest

 ) where

-- containers
import qualified Data.Map                 as Map

-- deepseq
import           Control.DeepSeq

-- directory
import           System.Directory         hiding (findFile)

-- filepath
import           System.FilePath

-- lens
import           Control.Lens.Combinators
import           Control.Lens
-- import           Control.Lens.Indexed

-- base
import           Data.Functor
import           Data.Foldable
import           Data.Bifunctor
import           Data.Maybe
import           Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import           Data.Semigroup (sconcat)
import           Data.Monoid
import           Data.Bitraversable
import           Data.Bifoldable
import           Control.Monad
import           Text.Show
import           GHC.Generics

-- $DirTreeNode
-- The basic item of this library is a DirTreeNode.

-- | A directory tree node. Everything is either a file, or a
-- directory.
data DirTreeNode r a
  = Directory r
  | File a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)

instance Bifunctor DirTreeNode where
  bimap fr fa = \case
    Directory r -> Directory (fr r)
    File a -> File (fa a)

instance Bifoldable DirTreeNode where
  bifoldMap fr fa = \case
    Directory r -> fr r
    File a -> fa a

instance Bitraversable DirTreeNode where
  bitraverse fr fa = \case
    Directory r -> Directory <$> fr r
    File a -> File <$> fa a

makeClassyPrisms ''DirTreeNode

-- | A DirTree can contain relativeFile files. This means that some files might be
-- symlinks.
data RelativeFile s a
  = Symlink s
  | Real a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)

instance Bifunctor RelativeFile where
  bimap fr fa = \case
    Symlink r -> Symlink (fr r)
    Real a -> Real (fa a)

instance Bifoldable RelativeFile where
  bifoldMap fr fa = \case
    Symlink r -> fr r
    Real a -> fa a

instance Bitraversable RelativeFile where
  bitraverse fr fa = \case
    Symlink r -> Symlink <$> fr r
    Real a -> Real <$> fa a

makeClassyPrisms ''RelativeFile

-- | It is quite offten that a node will be used as a relative file.
instance AsRelativeFile (DirTreeNode a (RelativeFile b c)) b c where
  _RelativeFile = _File

-- | A `FileType` is just a `DirTreeNode` with no contents.
type FileType = DirTreeNode () (RelativeFile () ())

-- | Gets the `FileType` of a `DirTreeNode`
fileTypeOfNode :: DirTreeNode a (RelativeFile b c) -> FileType
fileTypeOfNode = bimap (const ()) (bimap (const ()) (const ()))

-- | Check a filepath for Type, throws an IOException if path does not exist.
getFileType :: FilePath -> IO FileType
getFileType fp =
  -- TODO: Throw a resonable exception if the file does not exist.
  pathIsSymbolicLink fp >>= \case
  True ->
    return $ File (Symlink ())
  False ->
    doesDirectoryExist fp >>= \case
    True ->
      return $ Directory ()
    False ->
      return $ File (Real ())

-- | Reads the structure of the filepath
readPath ::
  FilePath
  -> IO (DirTreeNode [String] (RelativeFile FilePath ()))
readPath fp = bitraverse
  (const $ listDirectory fp)
  (bitraverse
   (const $ getSymbolicLinkTarget fp)
   return
  ) =<< getFileType fp


-- $FileMap
-- The 'FileMap' is used to represent the content of a directory.

-- | A map from file names to
newtype FileMap a =
  FileMap { fileMapAsMap :: Map.Map String a }
  deriving (Eq, Ord, NFData, Generic, Functor, Foldable, Traversable)

-- | Single File
singletonFileMap :: String -> a -> FileMap a
singletonFileMap s a = FileMap (Map.singleton s a)

-- | empty filemap
emptyFileMap :: FileMap a
emptyFileMap = FileMap Map.empty

-- | The 'FileMap' is a semigroup if the contnent is. It tries
-- to union the content under each item.
instance Semigroup a => Semigroup (FileMap a) where
  FileMap as <> FileMap bs =
    FileMap (Map.unionWith (<>) as bs)

-- | The empty monoid is the emptyFileMap
instance Semigroup a => Monoid (FileMap a) where
  mempty = emptyFileMap

instance FunctorWithIndex String FileMap
instance FoldableWithIndex String FileMap
instance TraversableWithIndex String FileMap where
  itraverse f (FileMap fs) = FileMap <$> itraverse f fs
  {-# INLINE itraverse #-}

instance Show a => Show (DirForest a) where
  showsPrec d m = showParen (d > 9) $ showString "DirForest . fromFileList " . showFileList m
    where
      showFileList =
        showListWith (\(s, x) -> f s $ dirTreeNode x) . toFileList . getInternalFileMap

      f s (Directory x) =
        showsPrec (dir_prec+1) s .
        showString " ./ "      .
        showFileList x

      f s (File x) =
        showsPrec (dir_prec+1) s .
        showString " .* "      .
        showsPrec (dir_prec+1) x
      dir_prec = 5


-- | Create a list of pairs of filenames and file values.
toFileList :: FileMap a -> [(String, a)]
toFileList (FileMap a) = Map.toList a

-- | Create a `FileMap` from a list of pairs of filenames a file values.
fromFileList :: [(String, a)] -> FileMap a
fromFileList = FileMap . Map.fromList

-- | Find a list of names used in the FileMap
toFileNames :: FileMap a -> [String]
toFileNames = map fst . toFileList

-- | Lookup a file using a filename
lookupFileMap :: String -> FileMap a -> Maybe a
lookupFileMap s (FileMap a) = Map.lookup s a

-- | The 'Map.alterF' version to the FileMap.
alterFileMap ::
  Functor f
  => (Maybe a -> f (Maybe a))
  -> String
  -> FileMap a
  -> f (FileMap a)
alterFileMap fn key (FileMap fm) =
  FileMap <$> Map.alterF fn key fm

type instance Index (FileMap a) = String
type instance IxValue (FileMap a) = a

instance Ixed (FileMap a) where
  ix k f m = FileMap <$> ix k f (fileMapAsMap m)
  {-# INLINE ix #-}

instance At (FileMap a) where
  at = flip alterFileMap
  {-# INLINE at #-}

-- $DirTree
-- A 'DirTree' is a recursive difined tree.
--

-- | A 'FileKey' is a list of filenames to get to the final file
type FileKey = [String]

-- | A 'DirTreeN' represents a single level in the DirTree.
type DirTreeN a = DirTreeNode (DirForest a) a

-- | A specialized traversal of the DirTreeNode
itraverseDirTreeN ::
  Applicative f
  => (FileKey -> a -> f b)
  -> DirTreeN a
  -> f (DirTreeN b)
itraverseDirTreeN fia = \case
  Directory m ->
    Directory <$> itraverse (fia . fromForestFileKey) m
  File a ->
    File <$> fia [] a


-- | A dir tree is a tree of nodes.
newtype DirTree a = DirTree
  { dirTreeNode :: DirTreeNode (DirForest a) a
  }
  deriving (Eq, Ord, NFData, Generic)


instance Functor DirTree where
  fmap f (DirTree a) = DirTree $ bimap (fmap f) f a

instance Foldable DirTree where
  foldMap f (DirTree e) = bifoldMap (foldMap f) f e

instance Traversable DirTree where
  traverse f (DirTree e) = DirTree <$> bitraverse (traverse f) f e

instance FunctorWithIndex FileKey DirTree
instance FoldableWithIndex FileKey DirTree
instance TraversableWithIndex FileKey DirTree where
  itraverse f (DirTree fs) = DirTree <$> itraverseDirTreeN f fs
  {-# INLINE itraverse #-}

-- | A relative dir tree also exists.
type RelativeDirTree s a = DirTree (RelativeFile s a)

-- | All 'DirTree's are also relative.
asRelativeDirTree :: DirTree a -> RelativeDirTree s a
asRelativeDirTree = fmap Real


instance (Show a) => Show (DirTree a) where
  showsPrec d c = showParen (d >9) (f $ dirTreeNode c)
    where
    f = \case
      Directory a ->
        showString "directory " . showsPrec 11 a
      File a ->
        showString "file " . showsPrec 11 a

-- | A DirTree is a semigroup, where it merges directories and take the last
-- entry if there files.
--
-- >>> file 'a' <> file 'b'
-- file 'b'
--
-- >>> directory' [ "a" .* 'a', "b" .* 'b'] <> directory' [ "b" .* 'd', "c" .* 'c']
-- directory (fromFileList ["a" .* 'a',"b" .* 'd',"c" .* 'c'])
instance Semigroup (DirTree a) where
  DirTree (Directory as) <> DirTree (Directory bs) =
    DirTree (Directory (as <> bs))
  _ <> a = a

-- | Constructs a dirtree with only a file
file :: a -> DirTree a
file = DirTree . File
{-# INLINE file #-}

-- | Constructs a relative dirtree with only a real file
realfile :: a -> RelativeDirTree s a
realfile = file . Real
{-# INLINE realfile #-}

-- | Constructs a dirtree with a symlink
symlink :: s -> RelativeDirTree s a
symlink = file . Symlink
{-# INLINE symlink #-}

-- | Constructs a dirtree with a directory
directory :: DirForest a -> DirTree a
directory = DirTree . Directory
{-# INLINE directory #-}

-- | Constructs a dirtree with a file list
directory' :: [(String, DirTree a)] -> DirTree a
directory' = DirTree . Directory . DirForest . fromFileList
{-# INLINE directory' #-}

-- | Constructs a dirtree with a empty directory
emptyDirectory :: DirTree a
emptyDirectory = directory' []
{-# INLINE emptyDirectory #-}

-- | Create a file
(.*) :: String -> a -> (String, DirTree a)
(.*) s a = (s, file a)

-- | Create a symbolic link
(.*>) :: String -> s -> (String, RelativeDirTree s a)
(.*>) s a = (s, symlink a)

-- | Create a real file
(.*.) :: String -> a -> (String, RelativeDirTree s a)
(.*.) s a = (s, realfile a)

-- | Create a directory
(./) :: String -> [(String, DirTree a)] -> (String, DirTree a)
(./) s a = (s, directory' a)

-- | Get a `FileKey` from a `FilePath`
fileKeyFromPath :: FilePath -> FileKey
fileKeyFromPath =
  splitDirectories

-- | Get a `FilePath` from a `FileKey`
fileKeyToPath :: FileKey -> FilePath
fileKeyToPath =
  joinPath

-- | 'diffFileKey' produces a filepath which is needed to
-- navigate from one FileKey to a other.
--
-- >>> diffFileKey ["hello", "world"] ["hello"]
-- ".."
--
-- >>> diffFileKey ["hello"] ["hello", "world", "test"]
-- "world/test"
--
-- >>> diffFileKey ["world", "test"] ["hello"]
-- "../../hello"
diffFileKey :: FileKey -> FileKey -> FilePath
diffFileKey f to' =
  let (n, bs) = prefix f to'
  in fileKeyToPath (replicate n ".." ++ bs)
  where
    prefix al@(a:as) bl@(b:bs)
      | a == b = prefix as bs
      | otherwise =
        (length al, bl)
    prefix (_:as) [] =
      (1 + length as, [])
    prefix [] bs =
      (0, bs)

-- | 'diffPath' produces a the filekey at the end of
-- a relative filepath, from one filekey.
--
-- >>> diffPath ["hello", "world"] ".."
-- Just ["hello"]
--
-- >>> diffPath ["hello"] "world/test"
-- Just ["hello","world","test"]
--
-- >>> diffPath ["world", "test"] "../../hello"
-- Just ["hello"]
--
-- >>> diffPath ["world", "test"] "/hello"
-- Nothing
--
-- >>> diffPath ["world", "test"] "../../.."
-- Nothing
diffPath :: FileKey -> FilePath -> Maybe FileKey
diffPath f path
  | isAbsolute path = Nothing
  | otherwise = go (fileKeyFromPath path) (reverse f)
  where
    go = \case
      "..":rest -> \case
        _:as -> go rest as
        [] -> Nothing
      rest -> \m -> Just (reverse m ++ rest)

-- | Alter File is the 'DirTree' version of 'Map.alterF'.
--
-- >>> alterFile (\x -> [Nothing, x, Just (file 'b')]) [] (Just (file 'a'))
-- [Nothing,Just (file 'a'),Just (file 'b')]
alterFile ::
  forall f a. Functor f
  => (Maybe (DirTree a) -> f (Maybe (DirTree a)))
  -> FileKey
  -> Maybe (DirTree a)
  -> f (Maybe (DirTree a))
alterFile fn key = maybe (newFile key) (go key) where
  go key' tree@(DirTree node) =
    case key' of
      [] -> fn (Just tree)
      k : rest ->
        case node of
          Directory a -> Just . directory <$> alterForest fn (k :| rest) a
          File _ -> newFile rest

  newFile :: FileKey -> f (Maybe (DirTree a))
  newFile key' = fmap (createDeepTree key') <$> fn Nothing
{-# INLINE alterFile #-}

-- | Create a recursive `DirTree` from a FileKey and a value.
createDeepFile :: FileKey -> a -> DirTree a
createDeepFile key a =
  createDeepTree key (file a)
{-# INLINE createDeepFile #-}

-- | Create a recursive `DirTree` from a FileKey and a value.
createDeepTree :: FileKey -> DirTree a -> DirTree a
createDeepTree key a =
  foldr (\s f -> directory (singletonForest s f)) a key
{-# INLINE createDeepTree #-}

type instance Index (DirTree a) = FileKey
type instance IxValue (DirTree a) = DirTree a

instance Ixed (DirTree a) where
  ix key fn = go key where
    go key' tree@(DirTree node) =
      case nonEmpty key' of
        Nothing -> fn tree
        Just fk ->
          case node of
            Directory a -> directory <$> ix fk fn a
            File _ -> pure tree
  {-# INLINE ix #-}

-- | Not a completly correct Lens, since it is implossible to
-- delete the current DirTree. To use a correct Lens, see
-- 'alterFile'.
--
-- >>> emptyDirectory & at ["file", "path"] ?~ file 'x'
-- directory (fromFileList ["file" ./ ["path" .* 'x']])
instance At (DirTree a) where
  at k f m = fromMaybe m <$> alterFile f k (Just m)
  {-# INLINE at #-}

-- | This method enables eta reduction of a DirTree a with an index.
iflattenDirTree ::
  (FileKey -> DirTreeNode (FileMap m) a -> m)
  -> DirTree a
  -> m
iflattenDirTree f = go id where
  go fk =
    f (fk []) . first (imap (\k -> go (fk . (k:))) . getInternalFileMap) . dirTreeNode
{-# inline iflattenDirTree #-}

-- | This method enables eta reduction of a DirTree a.
flattenDirTree ::
  (DirTreeNode (FileMap m) a -> m)
  -> DirTree a
  -> m
flattenDirTree f = go where
  go = f . first (fmap go . getInternalFileMap) . dirTreeNode
{-# inline flattenDirTree #-}

-- | Uses a semigroup to join together the results, This is slightly
-- less powerfull than 'iflattenDirTree', but more convinient for
-- summations.
depthfirst ::
  (Semigroup m)
  => (FileKey -> DirTreeNode [String] a -> m)
  -> DirTree a
  -> m
depthfirst f = iflattenDirTree $ \k -> \case
  Directory fm -> sconcat $
    f k (Directory . toFileNames $ fm) :| Data.Foldable.toList fm
  File a -> f k (File a)
{-# inline depthfirst #-}

-- | Find a file given a predicate that takes a `FileKey` and `DirTreeNode`.
findNode ::
  (FileKey -> DirTreeNode [String] a -> Bool)
  -> DirTree a
  -> Maybe (FileKey, DirTreeNode [String] a)
findNode f =
  getFirst . depthfirst (\k a -> First $ guard (f k a) $> (k, a))
{-# inline findNode #-}

-- List all the nodes in the dirtree
listNodes :: DirTree a -> [(FileKey, DirTreeNode [String] a)]
listNodes =
  (`appEndo` []) . depthfirst (\k a -> Endo ((k, a):))
{-# inline listNodes #-}


-- ** IO Methods

-- | A `Link` can either be `Internal`, pointing to something in the `DirTree` or
-- `External` pointing to an absolute `FilePath`.
data Link
  = Internal !FileKey
  | External !FilePath
  deriving (Show, Eq, Generic, NFData)

-- | Figure out a link from the FileKey and FilePath of Link
toLink :: FileKey -> FilePath -> Link
toLink key f =
  maybe (External f) Internal (diffPath (Prelude.init key) f)


-- | Reads a DirTree. All file paths are absolute to the filepath
readRelativeDirTree ::
  (FilePath -> IO a)
  -> FilePath
  -> IO (RelativeDirTree Link a)
readRelativeDirTree reader' fp = do
  from' <- canonicalizePath fp
  go from' fp
  where
    go from' fp' = do
      node <- readPath fp'
      DirTree <$> bimapM
        ( fmap (DirForest . fromFileList) . mapM (\k -> (k,) <$> go from' (fp' </> k)) )
        ( bimapM absolute (const $ reader' fp') )
        node
      where
        absolute a
          | isAbsolute a =
              return $ External a
          | otherwise = do
            a' <- canonicalizePath (takeDirectory fp' </> a)
            let a'' = makeRelative from' a'
            return $ if a'' /= a'
              then Internal (fileKeyFromPath a'')
              else External a'

-- | Reads a DirTree and follow all the relative links. Might recurse forever.
readDirTree ::
  (FilePath -> IO a)
  -> FilePath
  -> IO (DirTree a)
readDirTree fn fp =
  readRelativeDirTree fn fp >>= followLinks fn

-- | Follow the links to create the tree. This function might recurse forever.
followLinks :: forall a. (FilePath -> IO a) -> RelativeDirTree Link a -> IO (DirTree a)
followLinks fn tree = go tree where
  go = flattenDirTree $ \case
    File (Symlink a) -> case a of
      Internal s ->
        case tree ^? ix s of
          Just a' ->
            go a'
          Nothing ->
            error $ "Could not find " ++ show s
                ++ " in the dirtree " ++ show (void tree)
      External s ->
        readDirTree fn s

    File (Real a) ->
      return $ file a

    Directory a ->
      directory . DirForest <$> sequence a

-- | Writes a Relative DirTree to a file
writeRelativeDirTree ::
  (FilePath -> a -> IO ())
  -> FilePath
  -> RelativeDirTree Link a
  -> IO ()
writeRelativeDirTree writer fp = depthfirst go where
  go key = \case
    Directory _ ->
      createDirectory fp'
    File a ->
      case a of
        Symlink (External target) ->
          createFileLink target fp'
        Symlink (Internal key') ->
          createFileLink
          (case (key, key') of
              (_:fk',  _) -> diffFileKey fk' key'
              ([],    []) -> "."
              ([],     _) -> error "Fail"
          ) fp'
        Real a' ->
          writer fp' a'
    where fp' = fp </> fileKeyToPath key
{-# INLINE writeRelativeDirTree #-}

-- | Writes a Relative DirTree to a file
writeDirTree ::
  (FilePath -> a -> IO ())
  -> FilePath
  -> DirTree a
  -> IO ()
writeDirTree writer fp = writeRelativeDirTree writer fp . asRelativeDirTree
{-# INLINE writeDirTree #-}


-- $DirForest
-- A 'DirForest' is the content of a directory. A 'DirForest' is more
-- useful in some cases

newtype DirForest a = DirForest
  { getInternalFileMap :: FileMap (DirTree a)
  } deriving (Eq, Ord, NFData, Generic)

instance Functor DirForest where
  fmap f (DirForest a) = DirForest $ fmap (fmap f) a

instance Foldable DirForest where
  foldMap f (DirForest e) = foldMap (foldMap f) e

instance Traversable DirForest where
  traverse f (DirForest e) = DirForest <$> traverse (traverse f) e

instance FunctorWithIndex ForestFileKey DirForest
instance FoldableWithIndex ForestFileKey DirForest
instance TraversableWithIndex ForestFileKey DirForest where
  itraverse f (DirForest fs) =
    DirForest <$> itraverse (\k -> itraverse (f . (k:|))) fs
  {-# INLINE itraverse #-}

instance Semigroup (DirForest a) where
  (DirForest a) <> (DirForest b) = DirForest (a <> b)

instance Monoid (DirForest a) where
  mempty = DirForest mempty

-- | All entries in a DirForest has to be non-empty
type ForestFileKey = NonEmpty String

-- | Convert a 'ForestFileKey' to a 'FileKey'
fromForestFileKey :: ForestFileKey -> FileKey
fromForestFileKey = toList

-- | Convert a 'FileKey' to a 'ForestFileKey'
toForestFileKey :: FileKey -> Maybe ForestFileKey
toForestFileKey = nonEmpty

-- | Creates an empty forest
emptyForest :: DirForest a
emptyForest = mempty

-- | Creates an singleton forest
singletonForest :: String -> DirTree a -> DirForest a
singletonForest k f =
  DirForest $ singletonFileMap k f

-- | Creates an deep file in a forest
createDeepForest :: ForestFileKey -> DirTree a -> DirForest a
createDeepForest (k :| rest) f =
  singletonForest k (createDeepTree rest f)

-- | A relative dir forest also exists.
type RelativeDirForest s a = DirForest (RelativeFile s a)

-- | All 'DirTree's are also relative.
asRelativeDirForest :: DirForest a -> RelativeDirForest s a
asRelativeDirForest = fmap Real

type instance Index (DirForest a) = ForestFileKey
type instance IxValue (DirForest a) = DirTree a

instance Ixed (DirForest a) where
  ix (k :| key) fn a = DirForest <$> ix k (ix key fn) (getInternalFileMap a)
  --   -- ix key fn . getInternalFileMap
  --   go key' tree@(DirTree node) =
  --     case key' of
  --       [] -> fn tree
  --       k : rest ->
  --         case node of
  --           Directory a -> directory <$> ix k (ix rest fn) a
  --           File _ -> pure tree
  {-# INLINE ix #-}

alterForest ::
  forall f a. Functor f
  => (Maybe (DirTree a) -> f (Maybe (DirTree a)))
  -> ForestFileKey
  -> DirForest a
  -> f (DirForest a)
alterForest fn (k :| key) a =
  DirForest <$> alterFileMap (alterFile fn key) k (getInternalFileMap a)

-- >>> emptyDirForest & at ("file" :| ["path"]) ?~ file 'x'
-- fromFileList ["file" ./ ["path" .* 'x']]
instance At (DirForest a) where
  at k f = alterForest f k
  {-# INLINE at #-}

makeWrapped ''DirTree
makeWrapped ''DirForest

instance AsDirTreeNode (DirTree a) (DirForest a) a where
  _DirTreeNode = _Wrapped
  {-# INLINE _DirTreeNode #-}
