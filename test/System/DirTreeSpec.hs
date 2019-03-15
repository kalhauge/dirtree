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
        directoryFromFiles
        [ ("abslink", symlink (External "/dev/null"))
        , ("file", file "file")
        , ("folderlink", symlink (Internal ["folder"]))
        , ("folder", directoryFromFiles
          [ ("revlink", symlink (Internal ["file"]))
          , ("deepfile", file "folder/deepfile")
          ])
        , ("deeplink", symlink (Internal ["deepfile", "folder"]))
        , ("symlink", symlink (Internal ["file"]))
        ]

    it "should read the folder in the data directory" $ do
      x <- makeAbsolute "test/data/file"
      readDirTree (\f -> return $ makeRelative "test/data/folder" f) "test/data/folder" `shouldReturn`
        directoryFromFiles
        [ ("revlink", symlink (External x))
        , ("deepfile", file "deepfile")
        ]

  describe "followLinks" $ do
    it "should read and follow the links in the data directory" $ do
      let relname f = return $ makeRelative "test/data" f
      (readDirTree relname "test/data" >>= followLinks relname)
        `shouldReturn`
        directoryFromFiles
        [ ("symlink", file "file")
        , ("file", file "file")
        , ("folderlink", directoryFromFiles
          [ ("revlink", file "file")
          , ("deepfile", file "folder/deepfile")
          ])
        , ("folder", directoryFromFiles
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
        , ["abslink"]
        , ["deeplink"]
        , ["file"]
        , ["folder"]
        , ["deepfile", "folder"]
        , ["revlink", "folder"]
        , ["folderlink"]
        , ["symlink"]
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
        let folder = directoryFromFiles
              [ ("file1", file "Hello, World!" )
              , ("file2", file "Some other file" )
              , ("file3", symlink (Internal ["file2"]))
              ]
        writeDirTree writeFile "test/output/folder" folder
        readDirTree readFile "test/output/folder" `shouldReturn` folder

      it "can copy a folder" $ do
        datatree1 <- readDirTree return "test/data"
        writeDirTree (flip copyFile) "test/output/data" datatree1
        datatree2 <- readDirTree (return . ("test/data" </>) . makeRelative "test/output/data") "test/output/data"

        datatree2 `shouldBe` datatree1

  describe "semigroup" $ do
    it "can join trees together" $ do
      directoryFromFiles ["a" -.> "x"]
        <> directoryFromFiles [ "b" -|> "x" ]
        <> directoryFromFiles [ "x" -/> [ "a" -.> "y"]]
        <> directoryFromFiles [ "x" -/> [ "b" -.> "y"]]
        `shouldBe`
        directoryFromFiles
         [ "a" -.> "x"
         , "b" -|> "x"
         , "x" -/>
           [ "a" -.> "y"
           , "b" -.> "y"
           ]
         ]

    it "joins different types to latest file" $ do
      directoryFromFiles ["a" -.> "x"]
        <> directoryFromFiles [ "a" -|> "y" ]
        `shouldBe`
        directoryFromFiles
         [ "a" -|> "y"
         ]

    it "can join FileMaps together" $ do
      fromFileList ["a" -.> "x"]
        <> fromFileList [ "b" -|> "x" ]
        `shouldBe`
        fromFileList
         [ "a" -.> "x"
         , "b" -|> "x"
         ]

  describe "fromFiles" $ do
    it "can create a DirTree from an list of files" $ do
      fromFiles [(["a"], Right "x"), (["b", "c"], Left "y") ]
        `shouldBe`
        (Just $ directoryFromFiles
         [ "a" -.> "x"
         , "b" -/>
           [ "c" -|> "y"
           ]
         ])
