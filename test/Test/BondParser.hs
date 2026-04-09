module Test.BondParser (bondParserTests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Bond.Schema
import Bond.Parser

bondParserTests :: TestTree
bondParserTests = testGroup "Bond Parser"
  [ testCase "parse empty document" $ do
      case parseBond "" of
        Left err -> assertFailure err
        Right schema -> do
          bondNamespace schema @?= Nothing
          bondImports schema @?= []
          bondDecls schema @?= []

  , testCase "parse namespace" $ do
      case parseBond "namespace example" of
        Left err -> assertFailure err
        Right schema ->
          bondNamespace schema @?= Just "example"

  , testCase "parse import" $ do
      case parseBond "import \"base.bond\"" of
        Left err -> assertFailure err
        Right schema ->
          bondImports schema @?= ["base.bond"]

  , testCase "parse struct" $ do
      let input = T.pack $ unlines
            [ "struct Person {"
            , "  0: string name;"
            , "  1: int32 age;"
            , "}"
            ]
      case parseBond input of
        Left err -> assertFailure err
        Right schema -> do
          length (bondDecls schema) @?= 1
          case head (bondDecls schema) of
            BondDeclStruct s -> do
              bsName s @?= "Person"
              length (bsFields s) @?= 2
              let f1 = head (bsFields s)
              bfName f1 @?= "name"
              bfType f1 @?= BFTString
              bfFieldId f1 @?= 0
              let f2 = (bsFields s) !! 1
              bfName f2 @?= "age"
              bfType f2 @?= BFTInt32
              bfFieldId f2 @?= 1
            _ -> assertFailure "expected struct declaration"

  , testCase "parse struct with modifiers" $ do
      let input = T.pack $ unlines
            [ "struct Data {"
            , "  0: required string id;"
            , "  1: optional int64 value;"
            , "  2: required_optional bool flag;"
            , "}"
            ]
      case parseBond input of
        Left err -> assertFailure err
        Right schema -> do
          case head (bondDecls schema) of
            BondDeclStruct s -> do
              let f1 = head (bsFields s)
              bfModifier f1 @?= BondRequired
              let f2 = (bsFields s) !! 1
              bfModifier f2 @?= BondOptional
              let f3 = (bsFields s) !! 2
              bfModifier f3 @?= BondRequiredOptional
            _ -> assertFailure "expected struct"

  , testCase "parse enum" $ do
      let input = T.pack $ unlines
            [ "enum Color {"
            , "  RED = 0,"
            , "  GREEN = 1,"
            , "  BLUE = 2"
            , "}"
            ]
      case parseBond input of
        Left err -> assertFailure err
        Right schema -> do
          case head (bondDecls schema) of
            BondDeclEnum e -> do
              beName e @?= "Color"
              length (beValues e) @?= 3
              bevName (head (beValues e)) @?= "RED"
              bevValue (head (beValues e)) @?= Just 0
            _ -> assertFailure "expected enum"

  , testCase "parse container types" $ do
      let input = T.pack $ unlines
            [ "struct Containers {"
            , "  0: list<int32> numbers;"
            , "  1: set<string> names;"
            , "  2: map<string, int64> scores;"
            , "  3: nullable<string> maybe_name;"
            , "}"
            ]
      case parseBond input of
        Left err -> assertFailure err
        Right schema ->
          case head (bondDecls schema) of
            BondDeclStruct s -> do
              let f1 = head (bsFields s)
              bfType f1 @?= BFTList BFTInt32
              let f2 = (bsFields s) !! 1
              bfType f2 @?= BFTSet BFTString
              let f3 = (bsFields s) !! 2
              bfType f3 @?= BFTMap BFTString BFTInt64
              let f4 = (bsFields s) !! 3
              bfType f4 @?= BFTNullable BFTString
            _ -> assertFailure "expected struct"

  , testCase "parse struct with type parameter" $ do
      let input = T.pack $ unlines
            [ "struct Box<T> {"
            , "  0: string label;"
            , "}"
            ]
      case parseBond input of
        Left err -> assertFailure err
        Right schema ->
          case head (bondDecls schema) of
            BondDeclStruct s -> do
              bsName s @?= "Box"
              bsTypeParam s @?= Just "T"
            _ -> assertFailure "expected struct"

  , testCase "parse all primitive types" $ do
      let input = T.pack $ unlines
            [ "struct AllTypes {"
            , "  0: bool f_bool;"
            , "  1: int8 f_int8;"
            , "  2: int16 f_int16;"
            , "  3: int32 f_int32;"
            , "  4: int64 f_int64;"
            , "  5: uint8 f_uint8;"
            , "  6: uint16 f_uint16;"
            , "  7: uint32 f_uint32;"
            , "  8: uint64 f_uint64;"
            , "  9: float f_float;"
            , "  10: double f_double;"
            , "  11: string f_string;"
            , "  12: wstring f_wstring;"
            , "  13: blob f_blob;"
            , "}"
            ]
      case parseBond input of
        Left err -> assertFailure err
        Right schema ->
          case head (bondDecls schema) of
            BondDeclStruct s -> do
              length (bsFields s) @?= 14
              bfType (head (bsFields s)) @?= BFTBool
              bfType ((bsFields s) !! 1) @?= BFTInt8
              bfType ((bsFields s) !! 13) @?= BFTBlob
            _ -> assertFailure "expected struct"

  , testCase "parse complex document" $ do
      let input = T.pack $ unlines
            [ "namespace example.models"
            , ""
            , "import \"base.bond\""
            , ""
            , "enum Status {"
            , "  Active = 0,"
            , "  Inactive = 1"
            , "}"
            , ""
            , "struct User {"
            , "  0: required string name;"
            , "  1: optional int32 age;"
            , "  2: Status status;"
            , "}"
            , ""
            , "struct UserList {"
            , "  0: list<User> users;"
            , "}"
            ]
      case parseBond input of
        Left err -> assertFailure err
        Right schema -> do
          bondNamespace schema @?= Just "example.models"
          bondImports schema @?= ["base.bond"]
          length (bondDecls schema) @?= 3

  , testCase "parse with comments" $ do
      let input = T.pack $ unlines
            [ "// This is a comment"
            , "namespace test"
            , "/* Block comment */"
            , "struct Empty {"
            , "}"
            ]
      case parseBond input of
        Left err -> assertFailure err
        Right schema -> do
          bondNamespace schema @?= Just "test"
          length (bondDecls schema) @?= 1

  , testCase "parse struct with attributes" $ do
      let input = T.pack $ unlines
            [ "[Namespace(\"example\")]"
            , "[Schema(\"v2\")]"
            , "struct Annotated {"
            , "  [JsonName(\"display_name\")]"
            , "  0: string name;"
            , "  1: int32 age;"
            , "}"
            ]
      case parseBond input of
        Left err -> assertFailure err
        Right schema -> do
          case head (bondDecls schema) of
            BondDeclStruct s -> do
              bsName s @?= "Annotated"
              V.length (bsAttributes s) @?= 2
              bsAttributes s V.! 0 @?= ("Namespace", Just "example")
              bsAttributes s V.! 1 @?= ("Schema", Just "v2")
              let f1 = head (bsFields s)
              bfName f1 @?= "name"
              V.length (bfAttributes f1) @?= 1
              bfAttributes f1 V.! 0 @?= ("JsonName", Just "display_name")
              let f2 = (bsFields s) !! 1
              V.length (bfAttributes f2) @?= 0
            _ -> assertFailure "expected struct declaration"
  ]
