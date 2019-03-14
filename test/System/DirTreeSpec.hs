module System.DirTreeSpec where

import Test.Hspec (Spec, describe, it, before, describe)
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
        Directory ["symlink", "file", "folderlink", "folder", "abslink", "deeplink"]

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
      readDirTree (\f -> return $ makeRelative "test/data" f) "test/data" `shouldReturn`
        directory
        [ ("symlink", symlink (Internal ["file"]))
        , ("file", file "file")
        , ("folderlink", symlink (Internal ["folder"]))
        , ("folder", directory
          [ ("revlink", symlink (Internal ["file"]))
          , ("deepfile", file "folder/deepfile")
          ])
        , ("abslink", symlink (External "/dev/null"))
        , ("deeplink", symlink (Internal ["deepfile", "folder"]))
        ]

    it "should read the folder in the data directory" $ do
      x <- makeAbsolute "test/data/file"
      readDirTree (\f -> return $ makeRelative "test/data/folder" f) "test/data/folder" `shouldReturn`
        directory
        [ ("revlink", symlink (External x))
        , ("deepfile", file "deepfile")
        ]

  describe "followLinks" $ do
    it "should read and follow the links in the data directory" $ do
      let relname f = return $ makeRelative "test/data" f
      (readDirTree relname "test/data" >>= followLinks relname)
        `shouldReturn`
        directory
        [ ("symlink", file "file")
        , ("file", file "file")
        , ("folderlink", directory
          [ ("revlink", file "file")
          , ("deepfile", file "folder/deepfile")
          ])
        , ("folder", directory
          [ ("revlink", file "file")
          , ("deepfile", file "folder/deepfile")
          ])
        , ("abslink", file "/dev/null")
        , ("deeplink", file "folder/deepfile")
        ]

  describe "listNodes" $ do
    it "should read the data directory" $ do
      x <- listNodes <$> readDirTree return "test/data"
      map fst x `shouldBe`
        [ []
        , ["symlink"]
        , ["file"]
        , ["folderlink"]
        , ["folder"]
        , ["revlink", "folder"]
        , ["deepfile", "folder"]
        , ["abslink"]
        , ["deeplink"]
        ]

  describe "findNode" $ do
    it "can find deepfile" $ do
      x <- findNode (\fp _ -> takeBaseName (fileKeyToPath fp) == "deepfile") <$> readDirTree return "test/data"
      fmap fst x `shouldBe` Just ["deepfile", "folder"]

    it "can't find notafile" $ do
      x <- findNode (\fp _ -> takeBaseName (fileKeyToPath fp) == "notafile") <$> readDirTree return "test/data"
      fmap fst x `shouldBe` Nothing

  describe "writeDirTree" $ do
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
              , ("file3", symlink (Internal ["file2"]))
              ]
        writeDirTree writeFile "test/output/folder" folder
        forgetOrder <$> readDirTree readFile "test/output/folder"
          `shouldReturn` folder

      it "can copy a folder" $ do
        datatree1 <- readDirTree return "test/data"
        writeDirTree (flip copyFile) "test/output/data" datatree1
        datatree2 <- readDirTree (return . ("test/data" </>) . makeRelative "test/output/data") "test/output/data"

        datatree2 `shouldBe` datatree1
