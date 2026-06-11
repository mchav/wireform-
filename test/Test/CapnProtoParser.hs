module Test.CapnProtoParser (capnProtoParserTests) where

import CapnProto.Parser
import CapnProto.Schema
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd


capnProtoParserTests :: Spec
capnProtoParserTests =
  describe "CapnProto Parser" $
    sequence_
      [ it "parse file ID" $ do
          let input = "@0xdbb9ad1f14bf0b36;\n"
          case parseCapnProto input of
            Left err -> expectationFailure err
            Right schema ->
              csFileId schema `shouldBe` Just 0xdbb9ad1f14bf0b36
      , it "parse struct + enum" $ do
          let input =
                T.pack $
                  unlines
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
            Left err -> expectationFailure err
            Right schema -> do
              csFileId schema `shouldBe` Just 0xabcdef0123456789
              V.length (csDecls schema) `shouldBe` 2
              case csDecls schema V.! 0 of
                DStruct s -> do
                  sdName s `shouldBe` "Person"
                  V.length (sdFields s) `shouldBe` 3
                  let f0 = sdFields s V.! 0
                  fdName f0 `shouldBe` "name"
                  fdOrdinal f0 `shouldBe` 0
                  fdType f0 `shouldBe` CTText
                  let f1 = sdFields s V.! 1
                  fdName f1 `shouldBe` "age"
                  fdOrdinal f1 `shouldBe` 1
                  fdType f1 `shouldBe` CTUInt32
                other -> expectationFailure $ "expected DStruct, got " ++ show other
              case csDecls schema V.! 1 of
                DEnum e -> do
                  edName e `shouldBe` "Color"
                  V.length (edValues e) `shouldBe` 3
                  edValues e V.! 0 `shouldBe` ("red", 0)
                  edValues e V.! 1 `shouldBe` ("green", 1)
                  edValues e V.! 2 `shouldBe` ("blue", 2)
                other -> expectationFailure $ "expected DEnum, got " ++ show other
      , it "parse nested struct" $ do
          let input =
                T.pack $
                  unlines
                    [ "struct Outer {"
                    , "  inner @0 :Inner;"
                    , ""
                    , "  struct Inner {"
                    , "    value @0 :Int32;"
                    , "  }"
                    , "}"
                    ]
          case parseCapnProto input of
            Left err -> expectationFailure err
            Right schema -> do
              V.length (csDecls schema) `shouldBe` 1
              case csDecls schema V.! 0 of
                DStruct s -> do
                  sdName s `shouldBe` "Outer"
                  V.length (sdFields s) `shouldBe` 1
                  V.length (sdNested s) `shouldBe` 1
                  case sdNested s V.! 0 of
                    DStruct inner -> do
                      sdName inner `shouldBe` "Inner"
                      V.length (sdFields inner) `shouldBe` 1
                      fdName (sdFields inner V.! 0) `shouldBe` "value"
                      fdType (sdFields inner V.! 0) `shouldBe` CTInt32
                    other -> expectationFailure $ "expected nested DStruct, got " ++ show other
                other -> expectationFailure $ "expected DStruct, got " ++ show other
      , it "parse union inside struct" $ do
          let input =
                T.pack $
                  unlines
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
            Left err -> expectationFailure err
            Right schema -> do
              case csDecls schema V.! 0 of
                DStruct s -> do
                  sdName s `shouldBe` "Shape"
                  V.length (sdFields s) `shouldBe` 1
                  fdName (sdFields s V.! 0) `shouldBe` "name"
                  V.length (sdUnions s) `shouldBe` 1
                  let u = sdUnions s V.! 0
                  V.length (udFields u) `shouldBe` 3
                  fdName (udFields u V.! 0) `shouldBe` "circle"
                  fdOrdinal (udFields u V.! 0) `shouldBe` 1
                  fdType (udFields u V.! 0) `shouldBe` CTFloat64
                  fdName (udFields u V.! 1) `shouldBe` "rectangle"
                  fdType (udFields u V.! 2) `shouldBe` CTVoid
                other -> expectationFailure $ "expected DStruct, got " ++ show other
      , it "parse interface with methods" $ do
          let input =
                T.pack $
                  unlines
                    [ "interface Calculator {"
                    , "  add @0 (a :Int32, b :Int32) -> (result :Int32);"
                    , "  getVersion @1 () -> (version :Text);"
                    , "}"
                    ]
          case parseCapnProto input of
            Left err -> expectationFailure err
            Right schema -> do
              V.length (csDecls schema) `shouldBe` 1
              case csDecls schema V.! 0 of
                DInterface iface -> do
                  idName iface `shouldBe` "Calculator"
                  V.length (idMethods iface) `shouldBe` 2
                  let m0 = idMethods iface V.! 0
                  mdName m0 `shouldBe` "add"
                  V.length (mdParams m0) `shouldBe` 2
                  mdParams m0 V.! 0 `shouldBe` ("a", CTInt32)
                  mdParams m0 V.! 1 `shouldBe` ("b", CTInt32)
                  mdReturn m0 `shouldBe` CTInt32
                  let m1 = idMethods iface V.! 1
                  mdName m1 `shouldBe` "getVersion"
                  V.length (mdParams m1) `shouldBe` 0
                  mdReturn m1 `shouldBe` CTText
                other -> expectationFailure $ "expected DInterface, got " ++ show other
      , it "parse const" $ do
          let input =
                T.pack $
                  unlines
                    [ "const maxConnections :UInt32 = 100;"
                    ]
          case parseCapnProto input of
            Left err -> expectationFailure err
            Right schema -> do
              V.length (csDecls schema) `shouldBe` 1
              case csDecls schema V.! 0 of
                DConst name ty val -> do
                  name `shouldBe` "maxConnections"
                  ty `shouldBe` CTUInt32
                  val `shouldBe` "100"
                other -> expectationFailure $ "expected DConst, got " ++ show other
      , it "parse List type" $ do
          let input =
                T.pack $
                  unlines
                    [ "struct Container {"
                    , "  items @0 :List(Text);"
                    , "  nested @1 :List(List(Int32));"
                    , "}"
                    ]
          case parseCapnProto input of
            Left err -> expectationFailure err
            Right schema -> do
              case csDecls schema V.! 0 of
                DStruct s -> do
                  fdType (sdFields s V.! 0) `shouldBe` CTList CTText
                  fdType (sdFields s V.! 1) `shouldBe` CTList (CTList CTInt32)
                other -> expectationFailure $ "expected DStruct, got " ++ show other
      , it "parse field with default" $ do
          let input =
                T.pack $
                  unlines
                    [ "struct Config {"
                    , "  timeout @0 :UInt32 = 30;"
                    , "}"
                    ]
          case parseCapnProto input of
            Left err -> expectationFailure err
            Right schema -> do
              case csDecls schema V.! 0 of
                DStruct s -> do
                  fdDefault (sdFields s V.! 0) `shouldBe` Just "30"
                other -> expectationFailure $ "expected DStruct, got " ++ show other
      , it "parse comments" $ do
          let input =
                T.pack $
                  unlines
                    [ "# This is a comment"
                    , "struct Foo {"
                    , "  # field comment"
                    , "  bar @0 :Text;"
                    , "}"
                    ]
          case parseCapnProto input of
            Left err -> expectationFailure err
            Right schema -> do
              V.length (csDecls schema) `shouldBe` 1
              case csDecls schema V.! 0 of
                DStruct s -> sdName s `shouldBe` "Foo"
                other -> expectationFailure $ "expected DStruct, got " ++ show other
      , it "parse named type" $ do
          let input =
                T.pack $
                  unlines
                    [ "struct Wrapper {"
                    , "  inner @0 :MyCustomType;"
                    , "}"
                    ]
          case parseCapnProto input of
            Left err -> expectationFailure err
            Right schema -> do
              case csDecls schema V.! 0 of
                DStruct s ->
                  fdType (sdFields s V.! 0) `shouldBe` CTNamed "MyCustomType"
                other -> expectationFailure $ "expected DStruct, got " ++ show other
      , it "parse field with $annotation" $ do
          let input =
                T.pack $
                  unlines
                    [ "struct Config {"
                    , "  timeout @0 :UInt32 $jsonName(\"timeout_ms\");"
                    , "  name @1 :Text $deprecated $label(\"display\");"
                    , "}"
                    ]
          case parseCapnProto input of
            Left err -> expectationFailure err
            Right schema -> do
              case csDecls schema V.! 0 of
                DStruct s -> do
                  sdName s `shouldBe` "Config"
                  V.length (sdFields s) `shouldBe` 2
                  let f0 = sdFields s V.! 0
                  fdName f0 `shouldBe` "timeout"
                  V.length (fdAnnotations f0) `shouldBe` 1
                  fdAnnotations f0 V.! 0 `shouldBe` ("jsonName", Just "timeout_ms")
                  let f1 = sdFields s V.! 1
                  fdName f1 `shouldBe` "name"
                  V.length (fdAnnotations f1) `shouldBe` 2
                  fst (fdAnnotations f1 V.! 0) `shouldBe` "deprecated"
                  fdAnnotations f1 V.! 1 `shouldBe` ("label", Just "display")
                other -> expectationFailure $ "expected DStruct, got " ++ show other
      ]
