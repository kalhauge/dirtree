{-# LANGUAGE OverloadedStrings #-}
-- |
module System.DirTree.ZipSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import qualified Data.ByteString.Lazy as BL

import Control.Lens

import Codec.Archive.Zip

import System.Directory hiding (findFile)
import System.IO.Error

import System.DirTree
import System.DirTree.Zip

spec :: Spec
spec = do
  return ()
  -- describe "reading a zipfile" $ do
  --   it "can read more.zip" $ do
  --     zipfile <- toArchive <$> BL.readFile "test/more.zip"
  --     (zipfile ^. entries . to entriesToDirForest)
  --       `shouldBe` Just
  --       ( DirForest . fromFileList $
  --         [ "data" ./
  --           [ "abslink" .*> External "/dev/null"
  --           , "deeplink" .*> Internal ["data","folder","deepfile"]
  --           , "file" .*. ""
  --           , "folder" ./
  --             [ "deepfile" .*. ""
  --             , "revlink" .*> Internal ["data","file"]
  --             ]
  --           , "folderlink" .*> Internal ["data","folder"]
  --           , "symlink" .*> Internal ["data", "file"]
  --           ]
  --         ]
  --       )

  -- describe "writing a zipfile" $ do
  --   before (do _ <- tryIOError $ removeDirectoryRecursive "test/zip-output/"
  --              createDirectory "test/zip-output"
  --          ) $ do
  --     it "can write more.zip" $ do
  --       zipfile <- toArchive <$> BL.readFile "test/more.zip"
  --       let Just forest = zipfile ^. entries . to entriesToDirForest

  --       let bc = fromArchive $ zipfile & entries .~ entriesFromDirForest 0 forest
  --       BL.writeFile "test/zip-output/more.zip" bc
  --       toArchive bc ^. entries . to entriesToDirForest `shouldBe` Just forest
