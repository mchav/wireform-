module Test.ThriftParser (thriftParserTests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Thrift.Parser
import Thrift.Schema

thriftParserTests :: TestTree
thriftParserTests = testGroup "Thrift Parser"
  [ testCase "parse empty document" $ do
      case parseThrift "" of
        Left err -> assertFailure err
        Right schema -> do
          tsStructs schema @?= []
          tsEnums schema @?= []
          tsServices schema @?= []

  , testCase "parse struct" $ do
      let input = "struct User {\n  1: required string name\n  2: optional i32 age\n}"
      case parseThrift input of
        Left err -> assertFailure err
        Right schema -> do
          length (tsStructs schema) @?= 1
          let s = head (tsStructs schema)
          tsName s @?= "User"
          length (tsFields s) @?= 2
          let f1 = head (tsFields s)
          tfFieldName f1 @?= "name"
          tfFieldType f1 @?= TString
          tfRequiredness f1 @?= Required
          let f2 = (tsFields s) !! 1
          tfFieldName f2 @?= "age"
          tfFieldType f2 @?= TI32
          tfRequiredness f2 @?= Optional

  , testCase "parse enum" $ do
      let input = "enum Color {\n  RED = 0,\n  GREEN = 1,\n  BLUE = 2\n}"
      case parseThrift input of
        Left err -> assertFailure err
        Right schema -> do
          length (tsEnums schema) @?= 1
          let e = head (tsEnums schema)
          teName e @?= "Color"
          teValues e @?= [("RED", 0), ("GREEN", 1), ("BLUE", 2)]

  , testCase "parse enum auto-numbering" $ do
      let input = "enum Status {\n  ACTIVE,\n  INACTIVE,\n  DELETED\n}"
      case parseThrift input of
        Left err -> assertFailure err
        Right schema -> do
          let e = head (tsEnums schema)
          teValues e @?= [("ACTIVE", 0), ("INACTIVE", 1), ("DELETED", 2)]

  , testCase "parse service" $ do
      let input = T.pack $ unlines
            [ "service Calculator {"
            , "  i32 add(1: i32 a, 2: i32 b)"
            , "  oneway void ping()"
            , "}"
            ]
      case parseThrift input of
        Left err -> assertFailure err
        Right schema -> do
          length (tsServices schema) @?= 1
          let svc = head (tsServices schema)
          tsvName svc @?= "Calculator"
          length (tsvMethods svc) @?= 2
          let m1 = head (tsvMethods svc)
          tmName m1 @?= "add"
          tmReturnType m1 @?= Just TI32
          tmOneway m1 @?= False
          length (tmParams m1) @?= 2
          let m2 = (tsvMethods svc) !! 1
          tmName m2 @?= "ping"
          tmReturnType m2 @?= Nothing
          tmOneway m2 @?= True

  , testCase "parse typedef" $ do
      let input = "typedef i64 UserId"
      case parseThrift input of
        Left err -> assertFailure err
        Right schema -> do
          length (tsTypedefs schema) @?= 1
          let t = head (tsTypedefs schema)
          ttName t @?= "UserId"
          ttType t @?= TI64

  , testCase "parse const" $ do
      let input = "const i32 MAX_SIZE = 100"
      case parseThrift input of
        Left err -> assertFailure err
        Right schema -> do
          length (tsConsts schema) @?= 1
          let c = head (tsConsts schema)
          tcName c @?= "MAX_SIZE"
          tcValue c @?= TCVInt 100

  , testCase "parse union" $ do
      let input = "union Result {\n  1: string message\n  2: i32 code\n}"
      case parseThrift input of
        Left err -> assertFailure err
        Right schema -> do
          length (tsStructs schema) @?= 1
          let s = head (tsStructs schema)
          tsKind s @?= StructUnion

  , testCase "parse exception" $ do
      let input = "exception NotFound {\n  1: string message\n}"
      case parseThrift input of
        Left err -> assertFailure err
        Right schema -> do
          length (tsStructs schema) @?= 1
          let s = head (tsStructs schema)
          tsKind s @?= StructException

  , testCase "parse container types" $ do
      let input = T.pack $ unlines
            [ "struct Containers {"
            , "  1: list<i32> numbers;"
            , "  2: set<string> names;"
            , "  3: map<string, i64> scores;"
            , "}"
            ]
      case parseThrift input of
        Left err -> assertFailure err
        Right schema -> do
          let s = head (tsStructs schema)
          let f1 = head (tsFields s)
          tfFieldType f1 @?= TList TI32
          let f2 = (tsFields s) !! 1
          tfFieldType f2 @?= TSet TString
          let f3 = (tsFields s) !! 2
          tfFieldType f3 @?= TMap TString TI64

  , testCase "parse namespace and include" $ do
      let input = T.pack $ unlines
            [ "namespace java com.example"
            , "include \"shared.thrift\""
            , "struct Empty {}"
            ]
      case parseThrift input of
        Left err -> assertFailure err
        Right schema ->
          length (tsStructs schema) @?= 1

  , testCase "parse service with extends" $ do
      let input = "service Child extends Parent {\n  void doStuff()\n}"
      case parseThrift input of
        Left err -> assertFailure err
        Right schema -> do
          let svc = head (tsServices schema)
          tsvExtends svc @?= Just "Parent"

  , testCase "parse method with throws" $ do
      let input = T.pack $ unlines
            [ "service Svc {"
            , "  string lookup(1: i32 id) throws (1: NotFound nf)"
            , "}"
            ]
      case parseThrift input of
        Left err -> assertFailure err
        Right schema -> do
          let m = head (tsvMethods (head (tsServices schema)))
          length (tmThrows m) @?= 1

  , testCase "parse complex document" $ do
      let input = T.pack $ unlines
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
        Left err -> assertFailure err
        Right schema -> do
          length (tsStructs schema) @?= 2
          length (tsEnums schema) @?= 1
          length (tsServices schema) @?= 1

  , testCase "parse struct with annotations" $ do
      let input = T.pack $ unlines
            [ "struct Annotated {"
            , "  1: required string name (max_length = \"255\")"
            , "  2: optional i32 age"
            , "} (cpp.type = \"AnnotatedStruct\", java.final = \"true\")"
            ]
      case parseThrift input of
        Left err -> assertFailure err
        Right schema -> do
          length (tsStructs schema) @?= 1
          let s = head (tsStructs schema)
          tsName s @?= "Annotated"
          V.length (tsAnnotations s) @?= 2
          tsAnnotations s V.! 0 @?= ("cpp.type", "AnnotatedStruct")
          tsAnnotations s V.! 1 @?= ("java.final", "true")
          let f1 = head (tsFields s)
          tfFieldName f1 @?= "name"
          V.length (tfAnnotations f1) @?= 1
          tfAnnotations f1 V.! 0 @?= ("max_length", "255")
          let f2 = (tsFields s) !! 1
          V.length (tfAnnotations f2) @?= 0
  ]
