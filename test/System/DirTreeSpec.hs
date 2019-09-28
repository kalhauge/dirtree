module System.DirTreeSpec where

import Test.Hspec (Spec, it, before, describe)
import Test.Hspec.Expectations.Pretty

import System.DirTree

import System.FilePath
import System.Directory hiding (findFile)

import System.IO.Error

spec :: Spec
spec = do
  return () 
  -- possibleSpec

possibleSpec :: Spec
possibleSpec = do 
  describe "getFileType" $ do
    it "should find a directory" $ do
      getFileType "test/data" `shouldReturn` Directory ()

    it "should find a file" $ do
      getFileType "test/data/file" `shouldReturn` File (Real ())

    it "should find a symbolic link" $ do
      getFileType "test/data/symlink" `shouldReturn` File (Symlink ())

    it "should throw an IOException if nothing is found" $ do
      getFileType "test/data/nothing" `shouldThrow` anyException

  describe "readPath" $ do
    it "should find a directory" $ do
      readPath "test/data" `shouldReturn`
        Directory ["symlink", "file", "folderlink", "folder", "abslink", "deeplink"]

    it "should find a file" $ do
      readPath "test/data/file" `shouldReturn`
        File (Real ())

    it "should find a symbolic link" $ do
      readPath "test/data/symlink" `shouldReturn`
        File (Symlink "file")

    it "should find a symbolic to a folder" $ do
      readPath "test/data/folderlink" `shouldReturn`
        File (Symlink "folder")

    it "should throw an IOException if nothing exists" $ do
      readPath "test/data/nothing" `shouldThrow` anyException

  describe "readRelativeDirTree" $ do
    it "should read the data directory" $ do
      readRelativeDirTree (\f -> return $ makeRelative "test/data" f) "test/data" `shouldReturn`
        directory'
        [ "abslink" .*> External "/dev/null"
        , "file" .*. "file"
        , "folderlink" .*> Internal ["folder"]
        , "folder" ./
          [ "revlink" .*> Internal ["file"]
          , "deepfile" .*. "folder/deepfile"
          ]
        , "deeplink" .*> Internal ["folder", "deepfile"]
        , "symlink" .*> Internal ["file"]
        ]

    it "should read the folder in the data directory" $ do
      x <- makeAbsolute "test/data/file"
      readRelativeDirTree (\f -> return $ makeRelative "test/data/folder" f) "test/data/folder" `shouldReturn`
        directory'
        [ "revlink" .*> External x
        , "deepfile" .*. "deepfile"
        ]

  describe "followLinks" $ do
    it "should read and follow the links in the data directory" $ do
      let relname f = return $ makeRelative "test/data" f
      (readRelativeDirTree relname "test/data" >>= followLinks relname)
        `shouldReturn`
        directory'
        [ "symlink" .* "file"
        , "file" .* "file"
        , "folderlink" ./
          [ "revlink" .* "file"
          , "deepfile" .* "folder/deepfile"
          ]
        , "folder" ./
          [ "revlink" .* "file"
          , "deepfile" .* "folder/deepfile"
          ]
        , "abslink" .* "/dev/null"
        , "deeplink" .* "folder/deepfile"
        ]

  describe "listNodes" $ do
    it "should read the data directory" $ do
      x <- listNodes <$> readRelativeDirTree return "test/data"
      map fst x `shouldBe`
        [ []
        , ["abslink"]
        , ["deeplink"]
        , ["file"]
        , ["folder"]
        , ["folder", "deepfile"]
        , ["folder", "revlink"]
        , ["folderlink"]
        , ["symlink"]
        ]

  describe "findNode" $ do
    let isFile f fp _ = (takeBaseName (fileKeyToPath fp)) == f
    it "can find deepfile" $ do
      x <- findNode (isFile "deepfile") <$> readDirTree return "test/data"
      fmap fst x `shouldBe` Just ["folder", "deepfile"]

    it "can't find notafile" $ do
      x <- findNode (isFile "notafile") <$> readDirTree return "test/data"
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
        let folder = directory'
              [ "file1" .*. "Hello, World!"
              , "file2" .*. "Some other file"
              , "folder1" ./
                [ "file4" .*. "More files"
                ]
              , "symfile1" .*> Internal ["file2"]
              , "symfile2" .*> Internal ["folder1", "file4"]
              , "symfile3" .*> External "/dev/null"
              ]
        writeRelativeDirTree writeFile "test/output/folder" folder
        readRelativeDirTree readFile "test/output/folder" `shouldReturn` folder

      it "can copy a folder" $ do
        datatree1 <- readRelativeDirTree return "test/data"
        writeRelativeDirTree (flip copyFile) "test/output/data" datatree1
        datatree2 <- readRelativeDirTree (return . ("test/data" </>) . makeRelative "test/output/data") "test/output/data"

        datatree2 `shouldBe` datatree1

  describe "semigroup" $ do
    it "can join trees together" $ do
      directory' ["a" .* "x"]
        <> directory' [ "b" .* "x" ]
        <> directory' [ "x" ./ [ "a" .* "y"]]
        <> directory' [ "x" ./ [ "b" .* "y"]]
        `shouldBe`
        directory'
         [ "a" .* "x"
         , "b" .* "x"
         , "x" ./
           [ "a" .* "y"
           , "b" .* "y"
           ]
         ]

    it "joins different types to latest file" $ do
      directory' ["a" .* "x"]
        <> directory' [ "a" .* "y" ]
        `shouldBe`
        directory' [ "a" .* "y"]

    it "can join FileMaps together" $ do
      (DirForest . fromFileList) ["a" .* "x"]
        <> (DirForest . fromFileList) [ "b" .* "x" ]
        `shouldBe`
        (DirForest . fromFileList)
         [ "a" .* "x"
         , "b" .* "x"
         ]
