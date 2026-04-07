module Test.FlatBuffersParser (flatBuffersParserTests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import FlatBuffers.Parser
import FlatBuffers.Schema

flatBuffersParserTests :: TestTree
flatBuffersParserTests = testGroup "FlatBuffers Parser"
  [ testCase "parse table with mixed fields and defaults" $ do
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
        Left err -> assertFailure err
        Right schema -> do
          V.length (fbsDecls schema) @?= 1
          case fbsDecls schema V.! 0 of
            FBTable tbl -> do
              tdName tbl @?= "Monster"
              V.length (tdFields tbl) @?= 5
              let f0 = tdFields tbl V.! 0
              tfName f0 @?= "hp"
              tfType f0 @?= FTInt
              tfDefault f0 @?= Just "100"
              let f1 = tdFields tbl V.! 1
              tfName f1 @?= "mana"
              tfType f1 @?= FTShort
              tfDefault f1 @?= Just "150"
              let f2 = tdFields tbl V.! 2
              tfName f2 @?= "name"
              tfType f2 @?= FTString
              tfDefault f2 @?= Nothing
              let f3 = tdFields tbl V.! 3
              tfName f3 @?= "friendly"
              tfType f3 @?= FTBool
              tfDefault f3 @?= Just "false"
              let f4 = tdFields tbl V.! 4
              tfName f4 @?= "inventory"
              tfType f4 @?= FTVector FTUByte
              tfDefault f4 @?= Nothing
            other -> assertFailure $ "expected FBTable, got " ++ show other

  , testCase "parse struct" $ do
      let input = T.pack $ unlines
            [ "struct Vec3 {"
            , "  x:float;"
            , "  y:float;"
            , "  z:float;"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> assertFailure err
        Right schema -> do
          V.length (fbsDecls schema) @?= 1
          case fbsDecls schema V.! 0 of
            FBStruct s -> do
              fsdName s @?= "Vec3"
              V.length (fsdFields s) @?= 3
              fsdFields s V.! 0 @?= ("x", FTFloat)
              fsdFields s V.! 1 @?= ("y", FTFloat)
              fsdFields s V.! 2 @?= ("z", FTFloat)
            other -> assertFailure $ "expected FBStruct, got " ++ show other

  , testCase "parse enum with underlying type" $ do
      let input = T.pack $ unlines
            [ "enum Color : byte {"
            , "  Red = 0,"
            , "  Green = 1,"
            , "  Blue = 2,"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> assertFailure err
        Right schema -> do
          V.length (fbsDecls schema) @?= 1
          case fbsDecls schema V.! 0 of
            FBEnum e -> do
              fedName e @?= "Color"
              fedUnderlyingType e @?= FTByte
              V.length (fedValues e) @?= 3
              fedValues e V.! 0 @?= ("Red", Just 0)
              fedValues e V.! 1 @?= ("Green", Just 1)
              fedValues e V.! 2 @?= ("Blue", Just 2)
            other -> assertFailure $ "expected FBEnum, got " ++ show other

  , testCase "parse union" $ do
      let input = T.pack $ unlines
            [ "union Equipment {"
            , "  Weapon,"
            , "  Armor,"
            , "  Shield,"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> assertFailure err
        Right schema -> do
          V.length (fbsDecls schema) @?= 1
          case fbsDecls schema V.! 0 of
            FBUnion u -> do
              fudName u @?= "Equipment"
              V.length (fudMembers u) @?= 3
              fudMembers u V.! 0 @?= "Weapon"
              fudMembers u V.! 1 @?= "Armor"
              fudMembers u V.! 2 @?= "Shield"
            other -> assertFailure $ "expected FBUnion, got " ++ show other

  , testCase "parse namespace + root_type + file_identifier" $ do
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
        Left err -> assertFailure err
        Right schema -> do
          fbsNamespace schema @?= Just "MyGame.Sample"
          fbsRootType schema @?= Just "Monster"
          fbsFileIdentifier schema @?= Just "MONS"
          fbsFileExtension schema @?= Just "mon"
          V.length (fbsDecls schema) @?= 1

  , testCase "parse include" $ do
      let input = T.pack $ unlines
            [ "include \"common.fbs\";"
            , "include \"weapons.fbs\";"
            , ""
            , "table Empty {"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> assertFailure err
        Right schema -> do
          V.length (fbsIncludes schema) @?= 2
          fbsIncludes schema V.! 0 @?= "common.fbs"
          fbsIncludes schema V.! 1 @?= "weapons.fbs"

  , testCase "parse enum without explicit values" $ do
      let input = T.pack $ unlines
            [ "enum Status : ubyte {"
            , "  Active,"
            , "  Inactive,"
            , "  Deleted,"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> assertFailure err
        Right schema -> do
          case fbsDecls schema V.! 0 of
            FBEnum e -> do
              fedValues e V.! 0 @?= ("Active", Nothing)
              fedValues e V.! 1 @?= ("Inactive", Nothing)
              fedValues e V.! 2 @?= ("Deleted", Nothing)
            other -> assertFailure $ "expected FBEnum, got " ++ show other

  , testCase "parse attribute declaration" $ do
      let input = T.pack $ unlines
            [ "attribute \"priority\";"
            , ""
            , "table Item {"
            , "  name:string;"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> assertFailure err
        Right schema ->
          V.length (fbsDecls schema) @?= 1

  , testCase "parse comments" $ do
      let input = T.pack $ unlines
            [ "// This is a line comment"
            , "/* This is a"
            , "   block comment */"
            , "table Foo {"
            , "  bar:int; // inline comment"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> assertFailure err
        Right schema -> do
          V.length (fbsDecls schema) @?= 1
          case fbsDecls schema V.! 0 of
            FBTable tbl -> tdName tbl @?= "Foo"
            other -> assertFailure $ "expected FBTable, got " ++ show other

  , testCase "parse vector of named type" $ do
      let input = T.pack $ unlines
            [ "table Inventory {"
            , "  items:[Item];"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> assertFailure err
        Right schema -> do
          case fbsDecls schema V.! 0 of
            FBTable tbl ->
              tfType (tdFields tbl V.! 0) @?= FTVector (FTNamed "Item")
            other -> assertFailure $ "expected FBTable, got " ++ show other

  , testCase "parse deprecated field" $ do
      let input = T.pack $ unlines
            [ "table Legacy {"
            , "  old_field:int (deprecated);"
            , "  new_field:string;"
            , "}"
            ]
      case parseFlatBuffers input of
        Left err -> assertFailure err
        Right schema -> do
          case fbsDecls schema V.! 0 of
            FBTable tbl -> do
              tfDeprecated (tdFields tbl V.! 0) @?= True
              tfDeprecated (tdFields tbl V.! 1) @?= False
            other -> assertFailure $ "expected FBTable, got " ++ show other

  , testCase "parse complex schema" $ do
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
        Left err -> assertFailure err
        Right schema -> do
          fbsNamespace schema @?= Just "Game"
          V.length (fbsIncludes schema) @?= 1
          fbsRootType schema @?= Just "Monster"
          V.length (fbsDecls schema) @?= 4
  ]
