module System.DirTreeSpec where

import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Pretty

import System.DirTree

spec :: Spec
spec = do
  describe "checkPath" $ do
    it "should find a directory" $ do
      checkPath "test/data" `shouldReturn` Directory ()

    it "should find a file" $ do
      checkPath "test/data/file" `shouldReturn` File ()

    it "should find a symbolic link" $ do
      checkPath "test/data/symlink" `shouldReturn` Symlink ()

    it "should throw an IOException if nothing is found" $ do
      checkPath "test/data/nothing" `shouldThrow` anyException

  describe "readPath" $ do
    it "should find a directory" $ do
      readPath "test/data" `shouldReturn`
        Directory ["symlink", "file", "folderlink", "folder", "abslink"]

    it "should find a file" $ do
      readPath "test/data/file" `shouldReturn`
        File ()

    it "should find a symbolic link" $ do
      readPath "test/data/symlink" `shouldReturn`
        Symlink "file"

    it "should find a symbolic to a folder" $ do
      readPath "test/data/folderlink" `shouldReturn`
        Symlink "folder"

    it "should throw an IOException if nothing exists" $ do
      readPath "test/data/nothing" `shouldThrow` anyException

  describe "readDirTree" $ do
    it "should read the data directory" $ do
      readDirTree "test/data" `shouldReturn`
        DirTree (Directory
                 [ ("symlink", DirTree (Symlink "test/data/file"))
                 , ("file", DirTree (File "test/data/file"))
                 , ("folderlink", DirTree (Symlink "test/data/folder"))
                 , ("folder", DirTree
                     (Directory [
                         ("deepfile", DirTree (File "test/data/folder/deepfile"))
                         ]))
                 , ("abslink", DirTree (Symlink "/dev/null"))
                 ])

  describe "followLinks" $ do
    it "should read the follow the links in the data directory" $ do
      (readDirTree "test/data" >>= followLinks)
        `shouldReturn`
          DirTree (Directory
                   [ ("symlink", DirTree (File "test/data/file"))
                   , ("file", DirTree (File "test/data/file"))
                   , ("folderlink", DirTree
                       (Directory [("deepfile", DirTree (File "test/data/folder/deepfile"))]))
                   , ("folder", DirTree
                       (Directory [("deepfile", DirTree (File "test/data/folder/deepfile"))]))
                   , ("abslink", DirTree (File "/dev/null"))
                   ])
