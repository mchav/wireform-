module Test.BondParser (bondParserTests) where

import Bond.Parser
import Bond.Schema
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd


bondParserTests :: Spec
bondParserTests =
  describe "Bond Parser" $
    sequence_
      [ it "parse empty document" $ do
          case parseBond "" of
            Left err -> expectationFailure err
            Right schema -> do
              bondNamespace schema `shouldBe` Nothing
              bondImports schema `shouldBe` []
              bondDecls schema `shouldBe` []
      , it "parse namespace" $ do
          case parseBond "namespace example" of
            Left err -> expectationFailure err
            Right schema ->
              bondNamespace schema `shouldBe` Just "example"
      , it "parse import" $ do
          case parseBond "import \"base.bond\"" of
            Left err -> expectationFailure err
            Right schema ->
              bondImports schema `shouldBe` ["base.bond"]
      , it "parse struct" $ do
          let input =
                T.pack $
                  unlines
                    [ "struct Person {"
                    , "  0: string name;"
                    , "  1: int32 age;"
                    , "}"
                    ]
          case parseBond input of
            Left err -> expectationFailure err
            Right schema -> do
              length (bondDecls schema) `shouldBe` 1
              case head (bondDecls schema) of
                BondDeclStruct s -> do
                  bsName s `shouldBe` "Person"
                  length (bsFields s) `shouldBe` 2
                  let f1 = head (bsFields s)
                  bfName f1 `shouldBe` "name"
                  bfType f1 `shouldBe` BFTString
                  bfFieldId f1 `shouldBe` 0
                  let f2 = (bsFields s) !! 1
                  bfName f2 `shouldBe` "age"
                  bfType f2 `shouldBe` BFTInt32
                  bfFieldId f2 `shouldBe` 1
                _ -> expectationFailure "expected struct declaration"
      , it "parse struct with modifiers" $ do
          let input =
                T.pack $
                  unlines
                    [ "struct Data {"
                    , "  0: required string id;"
                    , "  1: optional int64 value;"
                    , "  2: required_optional bool flag;"
                    , "}"
                    ]
          case parseBond input of
            Left err -> expectationFailure err
            Right schema -> do
              case head (bondDecls schema) of
                BondDeclStruct s -> do
                  let f1 = head (bsFields s)
                  bfModifier f1 `shouldBe` BondRequired
                  let f2 = (bsFields s) !! 1
                  bfModifier f2 `shouldBe` BondOptional
                  let f3 = (bsFields s) !! 2
                  bfModifier f3 `shouldBe` BondRequiredOptional
                _ -> expectationFailure "expected struct"
      , it "parse enum" $ do
          let input =
                T.pack $
                  unlines
                    [ "enum Color {"
                    , "  RED = 0,"
                    , "  GREEN = 1,"
                    , "  BLUE = 2"
                    , "}"
                    ]
          case parseBond input of
            Left err -> expectationFailure err
            Right schema -> do
              case head (bondDecls schema) of
                BondDeclEnum e -> do
                  beName e `shouldBe` "Color"
                  length (beValues e) `shouldBe` 3
                  bevName (head (beValues e)) `shouldBe` "RED"
                  bevValue (head (beValues e)) `shouldBe` Just 0
                _ -> expectationFailure "expected enum"
      , it "parse container types" $ do
          let input =
                T.pack $
                  unlines
                    [ "struct Containers {"
                    , "  0: list<int32> numbers;"
                    , "  1: set<string> names;"
                    , "  2: map<string, int64> scores;"
                    , "  3: nullable<string> maybe_name;"
                    , "}"
                    ]
          case parseBond input of
            Left err -> expectationFailure err
            Right schema ->
              case head (bondDecls schema) of
                BondDeclStruct s -> do
                  let f1 = head (bsFields s)
                  bfType f1 `shouldBe` BFTList BFTInt32
                  let f2 = (bsFields s) !! 1
                  bfType f2 `shouldBe` BFTSet BFTString
                  let f3 = (bsFields s) !! 2
                  bfType f3 `shouldBe` BFTMap BFTString BFTInt64
                  let f4 = (bsFields s) !! 3
                  bfType f4 `shouldBe` BFTNullable BFTString
                _ -> expectationFailure "expected struct"
      , it "parse struct with type parameter" $ do
          let input =
                T.pack $
                  unlines
                    [ "struct Box<T> {"
                    , "  0: string label;"
                    , "}"
                    ]
          case parseBond input of
            Left err -> expectationFailure err
            Right schema ->
              case head (bondDecls schema) of
                BondDeclStruct s -> do
                  bsName s `shouldBe` "Box"
                  bsTypeParam s `shouldBe` Just "T"
                _ -> expectationFailure "expected struct"
      , it "parse all primitive types" $ do
          let input =
                T.pack $
                  unlines
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
            Left err -> expectationFailure err
            Right schema ->
              case head (bondDecls schema) of
                BondDeclStruct s -> do
                  length (bsFields s) `shouldBe` 14
                  bfType (head (bsFields s)) `shouldBe` BFTBool
                  bfType ((bsFields s) !! 1) `shouldBe` BFTInt8
                  bfType ((bsFields s) !! 13) `shouldBe` BFTBlob
                _ -> expectationFailure "expected struct"
      , it "parse complex document" $ do
          let input =
                T.pack $
                  unlines
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
            Left err -> expectationFailure err
            Right schema -> do
              bondNamespace schema `shouldBe` Just "example.models"
              bondImports schema `shouldBe` ["base.bond"]
              length (bondDecls schema) `shouldBe` 3
      , it "parse with comments" $ do
          let input =
                T.pack $
                  unlines
                    [ "// This is a comment"
                    , "namespace test"
                    , "/* Block comment */"
                    , "struct Empty {"
                    , "}"
                    ]
          case parseBond input of
            Left err -> expectationFailure err
            Right schema -> do
              bondNamespace schema `shouldBe` Just "test"
              length (bondDecls schema) `shouldBe` 1
      , it "parse struct with attributes" $ do
          let input =
                T.pack $
                  unlines
                    [ "[Namespace(\"example\")]"
                    , "[Schema(\"v2\")]"
                    , "struct Annotated {"
                    , "  [JsonName(\"display_name\")]"
                    , "  0: string name;"
                    , "  1: int32 age;"
                    , "}"
                    ]
          case parseBond input of
            Left err -> expectationFailure err
            Right schema -> do
              case head (bondDecls schema) of
                BondDeclStruct s -> do
                  bsName s `shouldBe` "Annotated"
                  V.length (bsAttributes s) `shouldBe` 2
                  bsAttributes s V.! 0 `shouldBe` ("Namespace", Just "example")
                  bsAttributes s V.! 1 `shouldBe` ("Schema", Just "v2")
                  let f1 = head (bsFields s)
                  bfName f1 `shouldBe` "name"
                  V.length (bfAttributes f1) `shouldBe` 1
                  bfAttributes f1 V.! 0 `shouldBe` ("JsonName", Just "display_name")
                  let f2 = (bsFields s) !! 1
                  V.length (bfAttributes f2) `shouldBe` 0
                _ -> expectationFailure "expected struct declaration"
      ]
