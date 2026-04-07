module Test.CapnProtoParser (capnProtoParserTests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import CapnProto.Parser
import CapnProto.Schema

capnProtoParserTests :: TestTree
capnProtoParserTests = testGroup "CapnProto Parser"
  [ testCase "parse file ID" $ do
      let input = "@0xdbb9ad1f14bf0b36;\n"
      case parseCapnProto input of
        Left err -> assertFailure err
        Right schema ->
          csFileId schema @?= Just 0xdbb9ad1f14bf0b36

  , testCase "parse struct + enum" $ do
      let input = T.pack $ unlines
            [ "@0xabcdef0123456789;"
            , ""
            , "struct Person {"
            , "  name @0 :Text;"
            , "  age @1 :UInt32;"
            , "  email @2 :Text;"
            , "}"
            , ""
            , "enum Color {"
            , "  red @0;"
            , "  green @1;"
            , "  blue @2;"
            , "}"
            ]
      case parseCapnProto input of
        Left err -> assertFailure err
        Right schema -> do
          csFileId schema @?= Just 0xabcdef0123456789
          V.length (csDecls schema) @?= 2
          case csDecls schema V.! 0 of
            DStruct s -> do
              sdName s @?= "Person"
              V.length (sdFields s) @?= 3
              let f0 = sdFields s V.! 0
              fdName f0 @?= "name"
              fdOrdinal f0 @?= 0
              fdType f0 @?= CTText
              let f1 = sdFields s V.! 1
              fdName f1 @?= "age"
              fdOrdinal f1 @?= 1
              fdType f1 @?= CTUInt32
            other -> assertFailure $ "expected DStruct, got " ++ show other
          case csDecls schema V.! 1 of
            DEnum e -> do
              edName e @?= "Color"
              V.length (edValues e) @?= 3
              edValues e V.! 0 @?= ("red", 0)
              edValues e V.! 1 @?= ("green", 1)
              edValues e V.! 2 @?= ("blue", 2)
            other -> assertFailure $ "expected DEnum, got " ++ show other

  , testCase "parse nested struct" $ do
      let input = T.pack $ unlines
            [ "struct Outer {"
            , "  inner @0 :Inner;"
            , ""
            , "  struct Inner {"
            , "    value @0 :Int32;"
            , "  }"
            , "}"
            ]
      case parseCapnProto input of
        Left err -> assertFailure err
        Right schema -> do
          V.length (csDecls schema) @?= 1
          case csDecls schema V.! 0 of
            DStruct s -> do
              sdName s @?= "Outer"
              V.length (sdFields s) @?= 1
              V.length (sdNested s) @?= 1
              case sdNested s V.! 0 of
                DStruct inner -> do
                  sdName inner @?= "Inner"
                  V.length (sdFields inner) @?= 1
                  fdName (sdFields inner V.! 0) @?= "value"
                  fdType (sdFields inner V.! 0) @?= CTInt32
                other -> assertFailure $ "expected nested DStruct, got " ++ show other
            other -> assertFailure $ "expected DStruct, got " ++ show other

  , testCase "parse union inside struct" $ do
      let input = T.pack $ unlines
            [ "struct Shape {"
            , "  name @0 :Text;"
            , ""
            , "  union {"
            , "    circle @1 :Float64;"
            , "    rectangle @2 :Text;"
            , "    triangle @3 :Void;"
            , "  }"
            , "}"
            ]
      case parseCapnProto input of
        Left err -> assertFailure err
        Right schema -> do
          case csDecls schema V.! 0 of
            DStruct s -> do
              sdName s @?= "Shape"
              V.length (sdFields s) @?= 1
              fdName (sdFields s V.! 0) @?= "name"
              V.length (sdUnions s) @?= 1
              let u = sdUnions s V.! 0
              V.length (udFields u) @?= 3
              fdName (udFields u V.! 0) @?= "circle"
              fdOrdinal (udFields u V.! 0) @?= 1
              fdType (udFields u V.! 0) @?= CTFloat64
              fdName (udFields u V.! 1) @?= "rectangle"
              fdType (udFields u V.! 2) @?= CTVoid
            other -> assertFailure $ "expected DStruct, got " ++ show other

  , testCase "parse interface with methods" $ do
      let input = T.pack $ unlines
            [ "interface Calculator {"
            , "  add @0 (a :Int32, b :Int32) -> (result :Int32);"
            , "  getVersion @1 () -> (version :Text);"
            , "}"
            ]
      case parseCapnProto input of
        Left err -> assertFailure err
        Right schema -> do
          V.length (csDecls schema) @?= 1
          case csDecls schema V.! 0 of
            DInterface iface -> do
              idName iface @?= "Calculator"
              V.length (idMethods iface) @?= 2
              let m0 = idMethods iface V.! 0
              mdName m0 @?= "add"
              V.length (mdParams m0) @?= 2
              mdParams m0 V.! 0 @?= ("a", CTInt32)
              mdParams m0 V.! 1 @?= ("b", CTInt32)
              mdReturn m0 @?= CTInt32
              let m1 = idMethods iface V.! 1
              mdName m1 @?= "getVersion"
              V.length (mdParams m1) @?= 0
              mdReturn m1 @?= CTText
            other -> assertFailure $ "expected DInterface, got " ++ show other

  , testCase "parse const" $ do
      let input = T.pack $ unlines
            [ "const maxConnections :UInt32 = 100;"
            ]
      case parseCapnProto input of
        Left err -> assertFailure err
        Right schema -> do
          V.length (csDecls schema) @?= 1
          case csDecls schema V.! 0 of
            DConst name ty val -> do
              name @?= "maxConnections"
              ty @?= CTUInt32
              val @?= "100"
            other -> assertFailure $ "expected DConst, got " ++ show other

  , testCase "parse List type" $ do
      let input = T.pack $ unlines
            [ "struct Container {"
            , "  items @0 :List(Text);"
            , "  nested @1 :List(List(Int32));"
            , "}"
            ]
      case parseCapnProto input of
        Left err -> assertFailure err
        Right schema -> do
          case csDecls schema V.! 0 of
            DStruct s -> do
              fdType (sdFields s V.! 0) @?= CTList CTText
              fdType (sdFields s V.! 1) @?= CTList (CTList CTInt32)
            other -> assertFailure $ "expected DStruct, got " ++ show other

  , testCase "parse field with default" $ do
      let input = T.pack $ unlines
            [ "struct Config {"
            , "  timeout @0 :UInt32 = 30;"
            , "}"
            ]
      case parseCapnProto input of
        Left err -> assertFailure err
        Right schema -> do
          case csDecls schema V.! 0 of
            DStruct s -> do
              fdDefault (sdFields s V.! 0) @?= Just "30"
            other -> assertFailure $ "expected DStruct, got " ++ show other

  , testCase "parse comments" $ do
      let input = T.pack $ unlines
            [ "# This is a comment"
            , "struct Foo {"
            , "  # field comment"
            , "  bar @0 :Text;"
            , "}"
            ]
      case parseCapnProto input of
        Left err -> assertFailure err
        Right schema -> do
          V.length (csDecls schema) @?= 1
          case csDecls schema V.! 0 of
            DStruct s -> sdName s @?= "Foo"
            other -> assertFailure $ "expected DStruct, got " ++ show other

  , testCase "parse named type" $ do
      let input = T.pack $ unlines
            [ "struct Wrapper {"
            , "  inner @0 :MyCustomType;"
            , "}"
            ]
      case parseCapnProto input of
        Left err -> assertFailure err
        Right schema -> do
          case csDecls schema V.! 0 of
            DStruct s ->
              fdType (sdFields s V.! 0) @?= CTNamed "MyCustomType"
            other -> assertFailure $ "expected DStruct, got " ++ show other
  ]
