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

-- -- data-fix
-- import           Data.Fix

-- directory
import           System.Directory

-- filepath
import           System.FilePath

-- void
import           Data.Void

-- base
import           Control.Monad
import           GHC.Generics

-- | A directory tree node. Everything is either a file, a symbolic link, or a
-- directory.
data DirTreeNode s a r
  = Symlink s
  | File a
  | Directory r
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)

-- | A DirTreeNode is a weird kind of algebra.
flattenDirTreeNode :: DirTreeNode m m m -> m
flattenDirTreeNode = \case
  File m -> m
  Symlink m -> m
  Directory m -> m

-- | We can map over a DirTreeNode
mapDirTreeNode ::
  (s -> s') -> (a -> a') -> (r -> r')
  -> DirTreeNode s a r
  -> DirTreeNode s' a' r'
mapDirTreeNode fs fa fr = \case
  File a -> File $ fa a
  Symlink s -> Symlink $ fs s
  Directory r -> Directory $ fr r

-- | We can fold over a DirTreeNode by providing a function for each case.
foldDirTreeNode :: (s -> m) -> (a -> m) -> (r -> m) -> DirTreeNode s a r -> m
foldDirTreeNode fs fa fr =
  flattenDirTreeNode . mapDirTreeNode fs fa fr

-- | We can fold over a DirTreeNode by providing a function for each case.
traverseDirTreeNode ::
  Functor m
  => (s -> m s') -> (a -> m a') -> (r -> m r')
  -> DirTreeNode s a r -> m (DirTreeNode s' a' r')
traverseDirTreeNode fs fa fr =
  flattenDirTreeNode . mapDirTreeNode
    (fmap Symlink . fs)
    (fmap File . fa)
    (fmap Directory . fr)

type DirTreeN s a = DirTreeNode s a [(String, DirTree s a)]

newtype DirTree s a = DirTree (DirTreeNode s a [(String, DirTree s a)])
  deriving (Show, Eq, Ord, NFData, Generic)

-- | Get the underlying dirTreeNode
dirTreeNode :: DirTree s a -> DirTreeN s a
dirTreeNode (DirTree a) = a

-- | Constructs a dirtree with only a file
file :: a -> DirTree s a
file = DirTree . File

-- | Constructs a dirtree with a symlink
symlink :: s -> DirTree s a
symlink = DirTree . Symlink

-- | Constructs a dirtree with a directory
directory :: [(String, DirTree s a)] -> DirTree s a
directory = DirTree . Directory

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

instance Semigroup (DirTree s a) where
  DirTree (Directory as) <> DirTree (Directory bs) =
    DirTree (Directory (reverse . nubWith (<>) . reverse $ as <> bs))
  _ <> a = a

instance Functor (DirTree s) where
  fmap = mapDirTree id

mapDirTree :: (s -> s') -> (a -> a') -> DirTree s a -> DirTree s' a'
mapDirTree fs fa =
    DirTree
    . mapDirTreeNode fs fa (map.fmap.mapDirTree fs $ fa)
    . dirTreeNode

instance Foldable (DirTree s) where
  foldMap = foldDirTree (foldMap snd) (const mempty)

-- | Folds over a dirtree
foldDirTree :: ([(String, m)] -> m) -> (s -> m) -> (a -> m) -> DirTree s a -> m
foldDirTree fr fs fa =
  foldDirTreeNode fs fa (fr . fmap (fmap (foldDirTree fr fs fa)))
  . dirTreeNode

instance Traversable (DirTree s) where
  traverse = traverseDirTree pure

traverseDirTree ::
  Applicative m
  => (s -> m s') -> (a -> m a')
  -> DirTree s a -> m (DirTree s' a')
traverseDirTree fs fa =
  fmap DirTree
  . traverseDirTreeNode fs fa (traverse.traverse.traverseDirTree fs $ fa)
  . dirTreeNode

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
readDirTreeNode ::
  FilePath
  -> IO (DirTreeNode FilePath () [String])
readDirTreeNode fp = do
  node <- checkPath fp
  foldDirTreeNode
    (const $ Symlink <$> getSymbolicLinkTarget fp)
    (const . return $ File ())
    (const $ Directory <$> listDirectory fp)
    node

-- | Reads a DirTree
readDirTree ::
  FilePath
  -> IO (DirTree FilePath FilePath)
readDirTree fp = do
  node <- readDirTreeNode fp
  foldDirTreeNode
    (return . symlink . (\case a | isAbsolute a -> a | otherwise -> takeDirectory fp </> a))
    (const . return $ file fp)
    (fmap directory . mapM (\s -> (s,) <$> readDirTree (fp </> s)))
    node

-- | Flatten a directory tree. This is usefull for following symlinks, or
-- expanding zip-files.
flatten ::
  (s -> DirTree s' a')
  -> (a -> DirTree s' a')
  -> DirTree s a
  -> DirTree s' a'
flatten =
  foldDirTree directory

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

data Anchored a = (:/)
  { base    ::  FilePath
  , dirTree :: a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)


-- read :: FilePath -> IO (Anchored (DirTree FilePath))
-- read fp = do
  -- tree <- DirTree <$> anaM go fp
  -- return $ fp :/ tree
  -- where
  --   go fp' = do
  --     files <- listDirectory fp'
  --     DirTreeF . Map.fromList <$> mapM (readDir fp') files

  --   readDir :: FilePath -> FilePath -> IO (FilePath, DirTreeNode FilePath FilePath)
  --   readDir fp' f = do
  --     let f' = fp' </> f
  --     x <- doesDirectoryExist f'
  --     return . (f,) $ if x
  --       then Dir f'
  --       else File f'


-- -- | A directory tree node.
-- data DirTreeNode a r
--   = File !a
--   | Dir !r
--   deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)

-- foldDirTreeNode :: (a -> b) -> (r -> b) -> DirTreeNode a r -> b
-- foldDirTreeNode ff fd =
--   \case
--     File a -> ff a
--     Dir  r -> fd r


-- newtype DirTreeF a r =
--   DirTreeF { unDirTreeF :: Map.Map FilePath (DirTreeNode a r) }
--   deriving (Show, Eq, Ord, Functor, Foldable, Traversable, NFData, Generic)

-- type DirTree' a = Fix (DirTreeF a)

-- -- | A directory tree
-- newtype DirTree a =
--   DirTree {unDirTree :: Fix (DirTreeF a)}
--   deriving (Show, Eq, Ord, Generic)

-- -- | A directory tree
-- newtype DirNode a =
--   DirNode { unDirNode :: DirTreeNode a (DirTree a) }
--   deriving (Show, Eq, Ord, Generic)

-- instance Functor DirTree where
--   fmap f =
--     DirTree . cata (Fix . DirTreeF . Map.map node . unDirTreeF) . unDirTree
--     where
--       node = \case
--         File a -> File (f a)
--         Dir b -> Dir b

-- instance Foldable DirTree where
--   foldMap f =
--     cata (foldMap (foldDirTreeNode f id) . unDirTreeF) . unDirTree

-- instance Traversable DirTree where
--   traverse f (DirTree m) = DirTree <$> cata go m
--     where
--       go (DirTreeF m') = Fix . DirTreeF <$> traverse help m'
--       help = foldDirTreeNode (fmap File . f) (fmap Dir)

-- -- | An Right biased semi group
-- instance Semigroup (DirTree a) where
--   (<>) (DirTree a') (DirTree b') =
--     DirTree $ go (a', b')
--     where
--       go (Fix (DirTreeF a), Fix (DirTreeF b)) =
--         Fix . DirTreeF $ Map.unionWith (curry unioner) a b
--       unioner = \case
--         (Dir a, Dir b) -> Dir $ go (a, b)
--         (_, a) -> a

-- data AnchoredTree a = (:/)
--   { anchor  :: ! FilePath
--   , dirTree :: ! (DirTree a)
--   } deriving (Show, Eq, Functor, Foldable, Traversable, Generic)


-- filterTree :: (DirTreeNode (String, a) String -> Bool) -> DirTree a -> DirTree a
-- filterTree fn (DirTree dtree) =
--   DirTree $ go dtree
--   where
--       go (Fix (DirTreeF a)) =
--         Fix . DirTreeF $
--           Map.filterWithKey
--           (\key -> fn . foldDirTreeNode (File .(key,)) (Dir . (const key)))
--           a

-- filterTreeOnFiles :: (FilePath -> Bool) -> DirTree a -> DirTree a
-- filterTreeOnFiles fn =
--   fromJust . fromFileList . filter (fn . fst) . toFileList

-- foldTreeWithFilePath ::
--   Monoid m
--   => (FilePath -> a -> m)
--   -> (FilePath -> m)
--   -> DirTree a
--   -> m
-- foldTreeWithFilePath f d (DirTree tree) =
--   go id tree
--   where
--     go fpm tree'  =
--       flip Map.foldMapWithKey (unDirTreeF . unFix $ tree') $ \fp -> \case
--         File a -> f (fpm fp) a
--         Dir r -> d (fpm fp) <> go (fpm fp </>) r

-- toFileList :: DirTree a -> [(FilePath, a)]
-- toFileList tree =
--   let x = foldTreeWithFilePath (\fp a -> Endo ((fp, a):)) (const mempty) tree
--   in appEndo x []

-- fromFileList :: [(FilePath, a)] -> Maybe (DirTree a)
-- fromFileList lst =
--   fmap DirTree . group $ map (\(fp, a) -> (splitDirectories fp, a)) lst
--   where
--     group :: [([String], a)] -> Maybe (DirTree' a)
--     group grp =
--       case partitionEithers $ map unwind grp of
--         ([], rest) ->
--           Fix . DirTreeF <$> traverse casx (Map.fromListWith (++) rest)
--         _ ->
--           Nothing

--     casx [([], a)] = Just $ File a
--     casx res = Dir <$> group res

--     unwind (x, a) =
--       maybe (Left a) (\(f,rest) -> Right (f, [(rest, a)])) $ List.uncons x

-- readTree :: FilePath -> IO (AnchoredTree FilePath)
-- readTree fp = do
--   tree <- DirTree <$> anaM go fp
--   return $ fp :/ tree
--   where
--     go fp' = do
--       files <- listDirectory fp'
--       DirTreeF . Map.fromList <$> mapM (readDir fp') files

--     readDir :: FilePath -> FilePath -> IO (FilePath, DirTreeNode FilePath FilePath)
--     readDir fp' f = do
--       let f' = fp' </> f
--       x <- doesDirectoryExist f'
--       return . (f,) $ if x
--         then Dir f'
--         else File f'

-- writeTreeWith :: (FilePath -> a -> IO ()) -> AnchoredTree a -> IO ()
-- writeTreeWith f (fp :/ tree) = do
--   createDirectoryIfMissing True fp
--   foldTreeWithFilePath handleFile handleDirectory  tree

--   where
--     handleFile f' a = do
--       f (fp </> f' ) a
--     handleDirectory f' = do
--       createDirectory (fp </> f')

-- data FileContent
--   = Content BL.ByteString
--   | SameAs FilePath
--   deriving (Show, Eq)

-- writeContent :: FilePath -> FileContent -> IO ()
-- writeContent fp = \case
--   Content bs ->
--     BL.writeFile fp bs
--   SameAs old ->
--     createFileLink old fp
