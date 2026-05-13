module Test.PrintInspect (printInspectTests) where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Proto.IDL.AST
import Proto.IDL.Inspect
import Proto.IDL.Parser
import Proto.IDL.Print
import Test.Tasty
import Test.Tasty.HUnit


printInspectTests :: TestTree
printInspectTests =
  testGroup
    "Print & Inspect"
    [ testGroup
        "Exact printing"
        [ testCase "roundtrip: parse -> print -> parse yields same AST" $ do
            let original = complexProto
            case parseProtoFile "<test>" original of
              Left e -> assertFailure (show e)
              Right ast1 -> do
                let printed = printProtoFile ast1
                case parseProtoFile "<printed>" printed of
                  Left e -> assertFailure ("Re-parse failed: " <> show e <> "\n\nPrinted:\n" <> T.unpack printed)
                  Right ast2 -> ast1 @?= ast2
        , testCase "syntax declaration" $ do
            let ast = ProtoFile Proto3 Nothing [] [] [] Nothing
            T.isInfixOf "syntax = \"proto3\";" (printProtoFile ast) @?= True
        , testCase "syntax proto2" $ do
            let ast = ProtoFile Proto2 Nothing [] [] [] Nothing
            T.isInfixOf "syntax = \"proto2\";" (printProtoFile ast) @?= True
        , testCase "package declaration" $ do
            let ast = ProtoFile Proto3 (Just "my.package") [] [] [] Nothing
            T.isInfixOf "package my.package;" (printProtoFile ast) @?= True
        , testCase "import" $ do
            let ast = ProtoFile Proto3 Nothing [ImportDef () Nothing "other.proto"] [] [] Nothing
            T.isInfixOf "import \"other.proto\";" (printProtoFile ast) @?= True
        , testCase "public import" $ do
            let ast = ProtoFile Proto3 Nothing [ImportDef () (Just ImportPublic) "other.proto"] [] [] Nothing
            T.isInfixOf "import public \"other.proto\";" (printProtoFile ast) @?= True
        , testCase "simple message roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "message Foo {"
                    , "  string name = 1;"
                    , "  int32 value = 2;"
                    , "}"
                    ]
            roundtripTest src
        , testCase "enum roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "enum Status {"
                    , "  UNKNOWN = 0;"
                    , "  ACTIVE = 1;"
                    , "}"
                    ]
            roundtripTest src
        , testCase "service roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "service Greeter {"
                    , "  rpc SayHello (HelloReq) returns (HelloReply);"
                    , "}"
                    ]
            roundtripTest src
        , testCase "map field roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "message Foo {"
                    , "  map<string, int32> labels = 1;"
                    , "}"
                    ]
            roundtripTest src
        , testCase "oneof roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "message Foo {"
                    , "  oneof val {"
                    , "    string text = 1;"
                    , "    int32 number = 2;"
                    , "  }"
                    , "}"
                    ]
            roundtripTest src
        , testCase "reserved roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "message Foo {"
                    , "  reserved 2, 10 to 20;"
                    , "  reserved \"old_field\";"
                    , "}"
                    ]
            roundtripTest src
        , testCase "field options roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "message Foo {"
                    , "  string name = 1 [deprecated = true];"
                    , "}"
                    ]
            roundtripTest src
        , testCase "option with extension name roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "option (my_custom_opt) = true;"
                    , "message Foo {"
                    , "  string name = 1;"
                    , "}"
                    ]
            roundtripTest src
        , testCase "nested message roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "message Outer {"
                    , "  message Inner {"
                    , "    int32 x = 1;"
                    , "  }"
                    , "  Inner inner = 1;"
                    , "}"
                    ]
            roundtripTest src
        , testCase "streaming rpc roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "service Chat {"
                    , "  rpc Stream (stream Msg) returns (stream Msg);"
                    , "}"
                    ]
            roundtripTest src
        ]
    , testGroup
        "AST inspection"
        [ testCase "allMessages finds top-level and nested" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> assertFailure (show e)
              Right pf -> do
                let msgs = allMessages pf
                assertBool "Should find Person" (any (\m -> msgName m == "Person") msgs)
                assertBool "Should find PhoneNumber" (any (\m -> msgName m == "PhoneNumber") msgs)
                assertBool "Should find Address" (any (\m -> msgName m == "Address") msgs)
                assertBool "Should find AddressBook" (any (\m -> msgName m == "AddressBook") msgs)
        , testCase "findMessage" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> assertFailure (show e)
              Right pf -> do
                assertBool "Find Person" (isJust (findMessage "Person" pf))
                assertBool "No Nonexistent" (isNothing (findMessage "Nonexistent" pf))
        , testCase "allEnums" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> assertFailure (show e)
              Right pf -> do
                let enums = allEnums pf
                assertBool "Should find Status" (any (\e -> enumName e == "Status") enums)
                assertBool "Should find PhoneType" (any (\e -> enumName e == "PhoneType") enums)
        , testCase "allServices" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> assertFailure (show e)
              Right pf -> do
                let svcs = allServices pf
                length svcs @?= 1
                svcName (head svcs) @?= "PersonService"
        , testCase "messageFields" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> assertFailure (show e)
              Right pf -> case findMessage "Person" pf of
                Nothing -> assertFailure "Person not found"
                Just msg -> do
                  let fields = messageFields msg
                  assertBool "Has name field" (any (\f -> fieldName f == "name") fields)
                  assertBool "Has id field" (any (\f -> fieldName f == "id") fields)
                  assertBool "Has email field" (any (\f -> fieldName f == "email") fields)
        , testCase "nestedMessages" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> assertFailure (show e)
              Right pf -> case findMessage "Person" pf of
                Nothing -> assertFailure "Person not found"
                Just msg -> do
                  let nested = nestedMessages msg
                  assertBool "Has PhoneNumber" (any (\m -> msgName m == "PhoneNumber") nested)
                  assertBool "Has Address" (any (\m -> msgName m == "Address") nested)
        , testCase "messageOneofs" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> assertFailure (show e)
              Right pf -> case findMessage "Person" pf of
                Nothing -> assertFailure "Person not found"
                Just msg -> do
                  let oneofs = messageOneofs msg
                  length oneofs @?= 1
                  oneofName (head oneofs) @?= "contact"
        , testCase "messageMapFields" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> assertFailure (show e)
              Right pf -> case findMessage "Person" pf of
                Nothing -> assertFailure "Person not found"
                Just msg -> do
                  let maps = messageMapFields msg
                  length maps @?= 1
                  mapFieldName (head maps) @?= "metadata"
        , testCase "allTypeNames" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> assertFailure (show e)
              Right pf -> do
                let types = allTypeNames pf
                assertBool "Has Person" (elem "Person" types)
                assertBool "Has Status" (elem "Status" types)
        , testCase "referencedTypes" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> assertFailure (show e)
              Right pf -> do
                let refs = referencedTypes pf
                assertBool "References PhoneNumber" (elem "PhoneNumber" refs)
                assertBool "References Status" (elem "Status" refs)
        , testCase "summarize" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> assertFailure (show e)
              Right pf -> do
                let s = summarize pf
                summSyntax s @?= Proto3
                summPackage s @?= Just "example.api"
                summMessageCount s > 0 @?= True
                summEnumCount s > 0 @?= True
                summServiceCount s @?= 1
        ]
    ]


roundtripTest :: Text -> IO ()
roundtripTest src =
  case parseProtoFile "<test>" src of
    Left e -> assertFailure ("Initial parse failed: " <> show e)
    Right ast1 -> do
      let printed = printProtoFile ast1
      case parseProtoFile "<printed>" printed of
        Left e -> assertFailure ("Re-parse failed: " <> show e <> "\n\nPrinted:\n" <> T.unpack printed)
        Right ast2 -> ast1 @?= ast2


complexProto :: Text
complexProto =
  T.unlines
    [ "syntax = \"proto3\";"
    , "package example.api;"
    , ""
    , "import \"google/protobuf/timestamp.proto\";"
    , "import public \"common.proto\";"
    , ""
    , "option java_package = \"com.example.api\";"
    , "option (custom_file_opt) = true;"
    , ""
    , "enum Status {"
    , "  STATUS_UNKNOWN = 0;"
    , "  STATUS_ACTIVE = 1;"
    , "  STATUS_INACTIVE = 2;"
    , "}"
    , ""
    , "message Person {"
    , "  string name = 1;"
    , "  int32 id = 2;"
    , "  string email = 3;"
    , ""
    , "  enum PhoneType {"
    , "    MOBILE = 0;"
    , "    HOME = 1;"
    , "    WORK = 2;"
    , "  }"
    , ""
    , "  message PhoneNumber {"
    , "    string number = 1;"
    , "    PhoneType type = 2;"
    , "  }"
    , ""
    , "  message Address {"
    , "    string street = 1;"
    , "    string city = 2;"
    , "  }"
    , ""
    , "  repeated PhoneNumber phones = 4;"
    , "  Status status = 5;"
    , "  map<string, string> metadata = 6;"
    , ""
    , "  oneof contact {"
    , "    string phone = 7;"
    , "    string email_alt = 8;"
    , "  }"
    , ""
    , "  reserved 10, 12 to 15;"
    , "  reserved \"old_field\";"
    , "}"
    , ""
    , "message AddressBook {"
    , "  repeated Person people = 1;"
    , "}"
    , ""
    , "service PersonService {"
    , "  rpc GetPerson (GetPersonRequest) returns (Person);"
    , "  rpc ListPersons (ListRequest) returns (stream Person);"
    , "  rpc UpdatePerson (stream Person) returns (UpdateResponse) {"
    , "    option deprecated = true;"
    , "  }"
    , "}"
    ]
