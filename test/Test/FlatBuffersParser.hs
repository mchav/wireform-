module Test.FlatBuffersParser (flatBuffersParserTests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Syd

import FlatBuffers.Parser
import FlatBuffers.Schema

flatBuffersParserTests :: Spec
flatBuffersParserTests = describe "FlatBuffers Parser" $ sequence_
  [ it "parse table with mixed fields and defaults" $ do
      let input = T.pack $ unlines
            [ "table Monster {"
            , "  hp:int = 100;"
            , "  mana:short = 150;"
            , "  name:string;"
            , "  friendly:bool = false;"
            , "  inventory:[ubyte];"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema -> do
          V.length (fbsDecls schema) `shouldBe` 1
          case fbsDecls schema V.! 0 of
            FBTable tbl -> do
              tdName tbl `shouldBe` "Monster"
              V.length (tdFields tbl) `shouldBe` 5
              let f0 = tdFields tbl V.! 0
              tfName f0 `shouldBe` "hp"
              tfType f0 `shouldBe` FTInt
              tfDefault f0 `shouldBe` Just "100"
              let f1 = tdFields tbl V.! 1
              tfName f1 `shouldBe` "mana"
              tfType f1 `shouldBe` FTShort
              tfDefault f1 `shouldBe` Just "150"
              let f2 = tdFields tbl V.! 2
              tfName f2 `shouldBe` "name"
              tfType f2 `shouldBe` FTString
              tfDefault f2 `shouldBe` Nothing
              let f3 = tdFields tbl V.! 3
              tfName f3 `shouldBe` "friendly"
              tfType f3 `shouldBe` FTBool
              tfDefault f3 `shouldBe` Just "false"
              let f4 = tdFields tbl V.! 4
              tfName f4 `shouldBe` "inventory"
              tfType f4 `shouldBe` FTVector FTUByte
              tfDefault f4 `shouldBe` Nothing
            other -> expectationFailure $ "expected FBTable, got " ++ show other

  , it "parse struct" $ do
      let input = T.pack $ unlines
            [ "struct Vec3 {"
            , "  x:float;"
            , "  y:float;"
            , "  z:float;"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema -> do
          V.length (fbsDecls schema) `shouldBe` 1
          case fbsDecls schema V.! 0 of
            FBStruct s -> do
              fsdName s `shouldBe` "Vec3"
              V.length (fsdFields s) `shouldBe` 3
              fsdFields s V.! 0 `shouldBe` ("x", FTFloat)
              fsdFields s V.! 1 `shouldBe` ("y", FTFloat)
              fsdFields s V.! 2 `shouldBe` ("z", FTFloat)
            other -> expectationFailure $ "expected FBStruct, got " ++ show other

  , it "parse enum with underlying type" $ do
      let input = T.pack $ unlines
            [ "enum Color : byte {"
            , "  Red = 0,"
            , "  Green = 1,"
            , "  Blue = 2,"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema -> do
          V.length (fbsDecls schema) `shouldBe` 1
          case fbsDecls schema V.! 0 of
            FBEnum e -> do
              fedName e `shouldBe` "Color"
              fedUnderlyingType e `shouldBe` FTByte
              V.length (fedValues e) `shouldBe` 3
              fedValues e V.! 0 `shouldBe` ("Red", Just 0)
              fedValues e V.! 1 `shouldBe` ("Green", Just 1)
              fedValues e V.! 2 `shouldBe` ("Blue", Just 2)
            other -> expectationFailure $ "expected FBEnum, got " ++ show other

  , it "parse union" $ do
      let input = T.pack $ unlines
            [ "union Equipment {"
            , "  Weapon,"
            , "  Armor,"
            , "  Shield,"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema -> do
          V.length (fbsDecls schema) `shouldBe` 1
          case fbsDecls schema V.! 0 of
            FBUnion u -> do
              fudName u `shouldBe` "Equipment"
              V.length (fudMembers u) `shouldBe` 3
              fudMembers u V.! 0 `shouldBe` "Weapon"
              fudMembers u V.! 1 `shouldBe` "Armor"
              fudMembers u V.! 2 `shouldBe` "Shield"
            other -> expectationFailure $ "expected FBUnion, got " ++ show other

  , it "parse namespace + root_type + file_identifier" $ do
      let input = T.pack $ unlines
            [ "namespace MyGame.Sample;"
            , ""
            , "table Monster {"
            , "  hp:int;"
            , "}"
            , ""
            , "root_type Monster;"
            , "file_identifier \"MONS\";"
            , "file_extension \"mon\";"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema -> do
          fbsNamespace schema `shouldBe` Just "MyGame.Sample"
          fbsRootType schema `shouldBe` Just "Monster"
          fbsFileIdentifier schema `shouldBe` Just "MONS"
          fbsFileExtension schema `shouldBe` Just "mon"
          V.length (fbsDecls schema) `shouldBe` 1

  , it "parse include" $ do
      let input = T.pack $ unlines
            [ "include \"common.fbs\";"
            , "include \"weapons.fbs\";"
            , ""
            , "table Empty {"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema -> do
          V.length (fbsIncludes schema) `shouldBe` 2
          fbsIncludes schema V.! 0 `shouldBe` "common.fbs"
          fbsIncludes schema V.! 1 `shouldBe` "weapons.fbs"

  , it "parse enum without explicit values" $ do
      let input = T.pack $ unlines
            [ "enum Status : ubyte {"
            , "  Active,"
            , "  Inactive,"
            , "  Deleted,"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema -> do
          case fbsDecls schema V.! 0 of
            FBEnum e -> do
              fedValues e V.! 0 `shouldBe` ("Active", Nothing)
              fedValues e V.! 1 `shouldBe` ("Inactive", Nothing)
              fedValues e V.! 2 `shouldBe` ("Deleted", Nothing)
            other -> expectationFailure $ "expected FBEnum, got " ++ show other

  , it "parse attribute declaration" $ do
      let input = T.pack $ unlines
            [ "attribute \"priority\";"
            , ""
            , "table Item {"
            , "  name:string;"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema ->
          V.length (fbsDecls schema) `shouldBe` 1

  , it "parse comments" $ do
      let input = T.pack $ unlines
            [ "// This is a line comment"
            , "/* This is a"
            , "   block comment */"
            , "table Foo {"
            , "  bar:int; // inline comment"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema -> do
          V.length (fbsDecls schema) `shouldBe` 1
          case fbsDecls schema V.! 0 of
            FBTable tbl -> tdName tbl `shouldBe` "Foo"
            other -> expectationFailure $ "expected FBTable, got " ++ show other

  , it "parse vector of named type" $ do
      let input = T.pack $ unlines
            [ "table Inventory {"
            , "  items:[Item];"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema -> do
          case fbsDecls schema V.! 0 of
            FBTable tbl ->
              tfType (tdFields tbl V.! 0) `shouldBe` FTVector (FTNamed "Item")
            other -> expectationFailure $ "expected FBTable, got " ++ show other

  , it "parse deprecated field" $ do
      let input = T.pack $ unlines
            [ "table Legacy {"
            , "  old_field:int (deprecated);"
            , "  new_field:string;"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema -> do
          case fbsDecls schema V.! 0 of
            FBTable tbl -> do
              tfDeprecated (tdFields tbl V.! 0) `shouldBe` True
              tfDeprecated (tdFields tbl V.! 1) `shouldBe` False
            other -> expectationFailure $ "expected FBTable, got " ++ show other

  , it "parse complex schema" $ do
      let input = T.pack $ unlines
            [ "namespace Game;"
            , ""
            , "include \"shared.fbs\";"
            , ""
            , "enum Color : byte { Red = 0, Green, Blue = 5 }"
            , ""
            , "struct Vec2 {"
            , "  x:float;"
            , "  y:float;"
            , "}"
            , ""
            , "union Any { Monster, Weapon }"
            , ""
            , "table Monster {"
            , "  pos:Vec2;"
            , "  hp:short = 100;"
            , "  name:string;"
            , "  equipped:Any;"
            , "}"
            , ""
            , "root_type Monster;"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema -> do
          fbsNamespace schema `shouldBe` Just "Game"
          V.length (fbsIncludes schema) `shouldBe` 1
          fbsRootType schema `shouldBe` Just "Monster"
          V.length (fbsDecls schema) `shouldBe` 4

  , it "parse field with metadata" $ do
      let input = T.pack $ unlines
            [ "table Config {"
            , "  priority:int (id: 1, deprecated);"
            , "  name:string (required);"
            , "  value:float;"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema -> do
          case fbsDecls schema V.! 0 of
            FBTable tbl -> do
              tdName tbl `shouldBe` "Config"
              V.length (tdFields tbl) `shouldBe` 3
              let f0 = tdFields tbl V.! 0
              tfName f0 `shouldBe` "priority"
              tfDeprecated f0 `shouldBe` True
              V.length (tfMetadata f0) `shouldBe` 2
              tfMetadata f0 V.! 0 `shouldBe` ("id", Just "1")
              tfMetadata f0 V.! 1 `shouldBe` ("deprecated", Nothing)
              let f1 = tdFields tbl V.! 1
              tfName f1 `shouldBe` "name"
              V.length (tfMetadata f1) `shouldBe` 1
              tfMetadata f1 V.! 0 `shouldBe` ("required", Nothing)
              let f2 = tdFields tbl V.! 2
              V.length (tfMetadata f2) `shouldBe` 0
            other -> expectationFailure $ "expected FBTable, got " ++ show other

  , it "parse attribute declarations stored" $ do
      let input = T.pack $ unlines
            [ "attribute \"priority\";"
            , "attribute \"custom_hash\";"
            , ""
            , "table Item {"
            , "  name:string;"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> expectationFailure err
        Right schema -> do
          V.length (fbsAttributes schema) `shouldBe` 2
          fbsAttributes schema V.! 0 `shouldBe` "priority"
          fbsAttributes schema V.! 1 `shouldBe` "custom_hash"
  ]
