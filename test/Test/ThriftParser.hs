module Test.ThriftParser (thriftParserTests) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd
import Thrift.Parser
import Thrift.Schema


thriftParserTests :: Spec
thriftParserTests =
  describe "Thrift Parser" $
    sequence_
      [ it "parse empty document" $ do
          case parseThrift "" of
            Left err -> expectationFailure err
            Right schema -> do
              tsStructs schema `shouldBe` []
              tsEnums schema `shouldBe` []
              tsServices schema `shouldBe` []
      , it "parse struct" $ do
          let input = "struct User {\n  1: required string name\n  2: optional i32 age\n}"
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema -> do
              length (tsStructs schema) `shouldBe` 1
              let s = head (tsStructs schema)
              tsName s `shouldBe` "User"
              length (tsFields s) `shouldBe` 2
              let f1 = head (tsFields s)
              tfFieldName f1 `shouldBe` "name"
              tfFieldType f1 `shouldBe` TString
              tfRequiredness f1 `shouldBe` Required
              let f2 = (tsFields s) !! 1
              tfFieldName f2 `shouldBe` "age"
              tfFieldType f2 `shouldBe` TI32
              tfRequiredness f2 `shouldBe` Optional
      , it "parse enum" $ do
          let input = "enum Color {\n  RED = 0,\n  GREEN = 1,\n  BLUE = 2\n}"
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema -> do
              length (tsEnums schema) `shouldBe` 1
              let e = head (tsEnums schema)
              teName e `shouldBe` "Color"
              teValues e `shouldBe` [("RED", 0), ("GREEN", 1), ("BLUE", 2)]
      , it "parse enum auto-numbering" $ do
          let input = "enum Status {\n  ACTIVE,\n  INACTIVE,\n  DELETED\n}"
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema -> do
              let e = head (tsEnums schema)
              teValues e `shouldBe` [("ACTIVE", 0), ("INACTIVE", 1), ("DELETED", 2)]
      , it "parse service" $ do
          let input =
                T.pack $
                  unlines
                    [ "service Calculator {"
                    , "  i32 add(1: i32 a, 2: i32 b)"
                    , "  oneway void ping()"
                    , "}"
                    ]
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema -> do
              length (tsServices schema) `shouldBe` 1
              let svc = head (tsServices schema)
              tsvName svc `shouldBe` "Calculator"
              length (tsvMethods svc) `shouldBe` 2
              let m1 = head (tsvMethods svc)
              tmName m1 `shouldBe` "add"
              tmReturnType m1 `shouldBe` Just TI32
              tmOneway m1 `shouldBe` False
              length (tmParams m1) `shouldBe` 2
              let m2 = (tsvMethods svc) !! 1
              tmName m2 `shouldBe` "ping"
              tmReturnType m2 `shouldBe` Nothing
              tmOneway m2 `shouldBe` True
      , it "parse typedef" $ do
          let input = "typedef i64 UserId"
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema -> do
              length (tsTypedefs schema) `shouldBe` 1
              let t = head (tsTypedefs schema)
              ttName t `shouldBe` "UserId"
              ttType t `shouldBe` TI64
      , it "parse const" $ do
          let input = "const i32 MAX_SIZE = 100"
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema -> do
              length (tsConsts schema) `shouldBe` 1
              let c = head (tsConsts schema)
              tcName c `shouldBe` "MAX_SIZE"
              tcValue c `shouldBe` TCVInt 100
      , it "parse union" $ do
          let input = "union Result {\n  1: string message\n  2: i32 code\n}"
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema -> do
              length (tsStructs schema) `shouldBe` 1
              let s = head (tsStructs schema)
              tsKind s `shouldBe` StructUnion
      , it "parse exception" $ do
          let input = "exception NotFound {\n  1: string message\n}"
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema -> do
              length (tsStructs schema) `shouldBe` 1
              let s = head (tsStructs schema)
              tsKind s `shouldBe` StructException
      , it "parse container types" $ do
          let input =
                T.pack $
                  unlines
                    [ "struct Containers {"
                    , "  1: list<i32> numbers;"
                    , "  2: set<string> names;"
                    , "  3: map<string, i64> scores;"
                    , "}"
                    ]
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema -> do
              let s = head (tsStructs schema)
              let f1 = head (tsFields s)
              tfFieldType f1 `shouldBe` TList TI32
              let f2 = (tsFields s) !! 1
              tfFieldType f2 `shouldBe` TSet TString
              let f3 = (tsFields s) !! 2
              tfFieldType f3 `shouldBe` TMap TString TI64
      , it "parse namespace and include" $ do
          let input =
                T.pack $
                  unlines
                    [ "namespace java com.example"
                    , "include \"shared.thrift\""
                    , "struct Empty {}"
                    ]
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema ->
              length (tsStructs schema) `shouldBe` 1
      , it "parse service with extends" $ do
          let input = "service Child extends Parent {\n  void doStuff()\n}"
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema -> do
              let svc = head (tsServices schema)
              tsvExtends svc `shouldBe` Just "Parent"
      , it "parse method with throws" $ do
          let input =
                T.pack $
                  unlines
                    [ "service Svc {"
                    , "  string lookup(1: i32 id) throws (1: NotFound nf)"
                    , "}"
                    ]
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema -> do
              let m = head (tsvMethods (head (tsServices schema)))
              length (tmThrows m) `shouldBe` 1
      , it "parse complex document" $ do
          let input =
                T.pack $
                  unlines
                    [ "namespace py example"
                    , ""
                    , "enum Status { ACTIVE = 1, INACTIVE = 2 }"
                    , ""
                    , "struct User {"
                    , "  1: required string name"
                    , "  2: optional i32 age"
                    , "  3: Status status = Status.ACTIVE"
                    , "}"
                    , ""
                    , "exception NotFound {"
                    , "  1: string message"
                    , "}"
                    , ""
                    , "service UserService {"
                    , "  User getUser(1: i32 id) throws (1: NotFound nf)"
                    , "  oneway void ping()"
                    , "}"
                    ]
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema -> do
              length (tsStructs schema) `shouldBe` 2
              length (tsEnums schema) `shouldBe` 1
              length (tsServices schema) `shouldBe` 1
      , it "parse struct with annotations" $ do
          let input =
                T.pack $
                  unlines
                    [ "struct Annotated {"
                    , "  1: required string name (max_length = \"255\")"
                    , "  2: optional i32 age"
                    , "} (cpp.type = \"AnnotatedStruct\", java.final = \"true\")"
                    ]
          case parseThrift input of
            Left err -> expectationFailure err
            Right schema -> do
              length (tsStructs schema) `shouldBe` 1
              let s = head (tsStructs schema)
              tsName s `shouldBe` "Annotated"
              V.length (tsAnnotations s) `shouldBe` 2
              tsAnnotations s V.! 0 `shouldBe` ("cpp.type", "AnnotatedStruct")
              tsAnnotations s V.! 1 `shouldBe` ("java.final", "true")
              let f1 = head (tsFields s)
              tfFieldName f1 `shouldBe` "name"
              V.length (tfAnnotations f1) `shouldBe` 1
              tfAnnotations f1 V.! 0 `shouldBe` ("max_length", "255")
              let f2 = (tsFields s) !! 1
              V.length (tfAnnotations f2) `shouldBe` 0
      ]
