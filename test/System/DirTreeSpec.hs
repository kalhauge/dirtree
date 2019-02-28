module System.DirTreeSpec where

import Test.Hspec (Spec, describe, it, before)
import Test.Hspec.Expectations.Pretty

import System.DirTree

import System.FilePath
import System.Directory hiding (findFile)

import System.IO.Error

spec :: Spec
spec = do
  describe "getFileType" $ do
    it "should find a directory" $ do
      getFileType "test/data" `shouldReturn` Directory ()

    it "should find a file" $ do
      getFileType "test/data/file" `shouldReturn` File ()

    it "should find a symbolic link" $ do
      getFileType "test/data/symlink" `shouldReturn` Symlink ()

    it "should throw an IOException if nothing is found" $ do
      getFileType "test/data/nothing" `shouldThrow` anyException

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
    it "should read and follow the links in the data directory" $ do
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

  describe "listFiles" $ do
    it "should read the data directory" $ do
      x <- listFiles <$> readDirTree "test/data"
      map fst x `shouldBe`
        [ "."
        , "./symlink"
        , "./file"
        , "./folderlink"
        , "./folder"
        , "./folder/deepfile"
        , "./abslink"
        ]

  describe "findFile" $ do
    it "can find deepfile" $ do
      x <- findFile (\fp _ -> takeBaseName fp == "deepfile") <$> readDirTree "test/data"
      fmap fst x `shouldBe` Just "./folder/deepfile"

    it "can't find notafile" $ do
      x <- findFile (\fp _ -> takeBaseName fp == "notafile") <$> readDirTree "test/data"
      fmap fst x `shouldBe` Nothing

  describe "findFile" $ do
    before (do
               _ <- tryIOError $ removeDirectoryRecursive "test/output/"
               createDirectory "test/output"
           ) $ do
      it "can write a file" $ do
        let newfile = file "Hello, World!"
        writeDirTree writeFile "test/output/newfile" newfile

        readFile "test/output/newfile" `shouldReturn` "Hello, World!"

      it "can write a folder" $ do
        let folder = directory
              [ ("file1", file "Hello, World!" )
              , ("file2", file "Some other file" )
              , ("file3", symlink "test/output/folder/file2" )
              ]
        writeDirTree writeFile "test/output/folder" folder
        forgetOrder <$> (readFiles readFile =<< readDirTree "test/output/folder")
          `shouldReturn` folder
