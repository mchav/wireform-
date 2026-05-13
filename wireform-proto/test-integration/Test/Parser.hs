module Test.Parser (parserTests) where

import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)
import Data.Text (Text)
import Proto.IDL.AST
import Proto.IDL.Parser
import Test.Tasty
import Test.Tasty.HUnit


parserTests :: TestTree
parserTests =
  testGroup
    "Parser"
    [ testGroup
        "Syntax declaration"
        [ testCase "proto3 syntax" $ do
            let input = "syntax = \"proto3\";\n"
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> protoSyntax pf @?= Proto3
        , testCase "proto2 syntax" $ do
            let input = "syntax = \"proto2\";\n"
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> protoSyntax pf @?= Proto2
        , testCase "default syntax is proto3" $ do
            case parseProtoFile "<test>" "" of
              Left e -> assertFailure (show e)
              Right pf -> protoSyntax pf @?= Proto3
        ]
    , testGroup
        "Package declaration"
        [ testCase "simple package" $ do
            let input = "syntax = \"proto3\";\npackage mypackage;\n"
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> protoPackage pf @?= Just "mypackage"
        , testCase "dotted package" $ do
            let input = "syntax = \"proto3\";\npackage com.example.api;\n"
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> protoPackage pf @?= Just "com.example.api"
        ]
    , testGroup
        "Import declarations"
        [ testCase "simple import" $ do
            let input = "syntax = \"proto3\";\nimport \"other.proto\";\n"
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoImports pf of
                [imp] -> importPath imp @?= "other.proto"
                _ -> assertFailure "Expected exactly one import"
        , testCase "public import" $ do
            let input = "syntax = \"proto3\";\nimport public \"other.proto\";\n"
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoImports pf of
                [imp] -> importModifier imp @?= Just ImportPublic
                _ -> assertFailure "Expected exactly one import"
        ]
    , testGroup
        "Message definitions"
        [ testCase "simple message with scalar fields" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message Person {"
                    , "  string name = 1;"
                    , "  int32 age = 2;"
                    , "  bool active = 3;"
                    , "}"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoTopLevels pf of
                [TLMessage msg] -> do
                  msgName msg @?= "Person"
                  length (msgElements msg) @?= 3
                _ -> assertFailure "Expected one message"
        , testCase "message with all scalar types" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message AllTypes {"
                    , "  double f1 = 1;"
                    , "  float f2 = 2;"
                    , "  int32 f3 = 3;"
                    , "  int64 f4 = 4;"
                    , "  uint32 f5 = 5;"
                    , "  uint64 f6 = 6;"
                    , "  sint32 f7 = 7;"
                    , "  sint64 f8 = 8;"
                    , "  fixed32 f9 = 9;"
                    , "  fixed64 f10 = 10;"
                    , "  sfixed32 f11 = 11;"
                    , "  sfixed64 f12 = 12;"
                    , "  bool f13 = 13;"
                    , "  string f14 = 14;"
                    , "  bytes f15 = 15;"
                    , "}"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoTopLevels pf of
                [TLMessage msg] -> length (msgElements msg) @?= 15
                _ -> assertFailure "Expected one message"
        , testCase "nested message" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message Outer {"
                    , "  message Inner {"
                    , "    int32 value = 1;"
                    , "  }"
                    , "  Inner inner = 1;"
                    , "}"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoTopLevels pf of
                [TLMessage msg] -> do
                  msgName msg @?= "Outer"
                  let hasNestedMsg = any isNestedMsg (msgElements msg)
                  assertBool "Should have nested message" hasNestedMsg
                _ -> assertFailure "Expected one message"
        , testCase "repeated fields" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message Msg {"
                    , "  repeated string tags = 1;"
                    , "  repeated int32 values = 2;"
                    , "}"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoTopLevels pf of
                [TLMessage msg] ->
                  let fields = extractFieldDefs (msgElements msg)
                  in all (\f -> fieldLabel f == Just Repeated) fields @?= True
                _ -> assertFailure "Expected one message"
        , testCase "map fields" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message Msg {"
                    , "  map<string, int32> counts = 1;"
                    , "}"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoTopLevels pf of
                [TLMessage msg] -> case msgElements msg of
                  [MEMapField mf] -> do
                    mapKeyType mf @?= SString
                    mapValueType mf @?= FTScalar SInt32
                    mapFieldName mf @?= "counts"
                  _ -> assertFailure "Expected one map field"
                _ -> assertFailure "Expected one message"
        ]
    , testGroup
        "Oneof"
        [ testCase "oneof definition" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message Msg {"
                    , "  oneof value {"
                    , "    string name = 1;"
                    , "    int32 id = 2;"
                    , "  }"
                    , "}"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoTopLevels pf of
                [TLMessage msg] -> case msgElements msg of
                  [MEOneof od] -> do
                    oneofName od @?= "value"
                    length (oneofFields od) @?= 2
                  _ -> assertFailure "Expected one oneof"
                _ -> assertFailure "Expected one message"
        ]
    , testGroup
        "Enum definitions"
        [ testCase "simple enum" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "enum Status {"
                    , "  UNKNOWN = 0;"
                    , "  ACTIVE = 1;"
                    , "  INACTIVE = 2;"
                    , "}"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoTopLevels pf of
                [TLEnum ed] -> do
                  enumName ed @?= "Status"
                  length (enumValues ed) @?= 3
                _ -> assertFailure "Expected one enum"
        , testCase "enum with allow_alias option" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "enum Status {"
                    , "  option allow_alias = true;"
                    , "  UNKNOWN = 0;"
                    , "  ACTIVE = 1;"
                    , "  RUNNING = 1;"
                    , "}"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoTopLevels pf of
                [TLEnum ed] -> do
                  length (enumOptions ed) @?= 1
                  length (enumValues ed) @?= 3
                _ -> assertFailure "Expected one enum"
        ]
    , testGroup
        "Service definitions"
        [ testCase "simple service" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "service Greeter {"
                    , "  rpc SayHello (HelloRequest) returns (HelloReply);"
                    , "}"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoTopLevels pf of
                [TLService svc] -> do
                  svcName svc @?= "Greeter"
                  case svcRpcs svc of
                    [rpc] -> do
                      rpcName rpc @?= "SayHello"
                      rpcInput rpc @?= "HelloRequest"
                      rpcOutput rpc @?= "HelloReply"
                      rpcInputStr rpc @?= NoStream
                      rpcOutputStr rpc @?= NoStream
                    _ -> assertFailure "Expected one RPC"
                _ -> assertFailure "Expected one service"
        , testCase "streaming rpc" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "service Chat {"
                    , "  rpc Stream (stream Msg) returns (stream Msg);"
                    , "}"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoTopLevels pf of
                [TLService svc] -> case svcRpcs svc of
                  [rpc] -> do
                    rpcInputStr rpc @?= Streaming
                    rpcOutputStr rpc @?= Streaming
                  _ -> assertFailure "Expected one RPC"
                _ -> assertFailure "Expected one service"
        ]
    , testGroup
        "Options and annotations"
        [ testCase "file-level option" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "option java_package = \"com.example\";"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoOptions pf of
                [opt] -> do
                  optValue opt @?= CString "com.example"
                _ -> assertFailure "Expected one option"
        , testCase "custom extension option" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "option (my_custom_opt) = true;"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoOptions pf of
                [opt] -> do
                  case optNameParts (optName opt) of
                    [ExtensionOption n] -> n @?= "my_custom_opt"
                    _ -> assertFailure "Expected extension option"
                  optValue opt @?= CBool True
                _ -> assertFailure "Expected one option"
        , testCase "field options" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message Msg {"
                    , "  string name = 1 [deprecated = true, json_name = \"Name\"];"
                    , "}"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoTopLevels pf of
                [TLMessage msg] -> case msgElements msg of
                  [MEField fd] -> length (fieldOptions fd) @?= 2
                  _ -> assertFailure "Expected one field"
                _ -> assertFailure "Expected one message"
        , testCase "aggregate option value" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "option (my_opt) = { foo: 1 bar: \"hello\" };"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoOptions pf of
                [opt] -> case optValue opt of
                  CAggregate kvs -> length kvs @?= 2
                  _ -> assertFailure "Expected aggregate constant"
                _ -> assertFailure "Expected one option"
        ]
    , testGroup
        "Reserved"
        [ testCase "reserved field numbers" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message Msg {"
                    , "  reserved 2, 15, 9 to 11;"
                    , "}"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoTopLevels pf of
                [TLMessage msg] -> case msgElements msg of
                  [MEReserved (ReservedNumbers ranges)] ->
                    length ranges @?= 3
                  _ -> assertFailure "Expected reserved numbers"
                _ -> assertFailure "Expected one message"
        , testCase "reserved field names" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message Msg {"
                    , "  reserved \"foo\", \"bar\";"
                    , "}"
                    ]
            case parseProtoFile "<test>" input of
              Left e -> assertFailure (show e)
              Right pf -> case protoTopLevels pf of
                [TLMessage msg] -> case msgElements msg of
                  [MEReserved (ReservedNames names)] ->
                    names @?= ["foo", "bar"]
                  _ -> assertFailure "Expected reserved names"
                _ -> assertFailure "Expected one message"
        ]
    , testGroup
        "Complex proto files"
        [ testCase "full proto file" $ do
            let input = complexProto
            assertBool "Should parse complex proto" (isRight (parseProtoFile "<test>" input))
        ]
    , testGroup
        "Comments"
        [ testCase "line comments" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\"; // this is a comment"
                    , "// another comment"
                    , "message Msg {"
                    , "  int32 x = 1; // field comment"
                    , "}"
                    ]
            assertBool "Should parse with line comments" (isRight (parseProtoFile "<test>" input))
        , testCase "block comments" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "/* block comment */"
                    , "message Msg {"
                    , "  /* multi"
                    , "     line"
                    , "     comment */"
                    , "  int32 x = 1;"
                    , "}"
                    ]
            assertBool "Should parse with block comments" (isRight (parseProtoFile "<test>" input))
        ]
    , testGroup
        "Error message quality"
        [ testCase "missing semicolon points to correct location" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message Foo {"
                    , "  string name = 1"
                    , "}"
                    ]
            case parseProtoFile "test.proto" input of
              Left e -> do
                let msg = renderParseError e
                assertBool "should mention ';'" ("';'" `isInfixOf` msg)
                assertBool "should show file location" ("test.proto:4:" `isInfixOf` msg)
                assertBool "should show source context" ("string name = 1" `isInfixOf` msg)
                assertBool "should have caret pointer" ("^" `isInfixOf` msg)
              Right _ -> assertFailure "Should have failed to parse"
        , testCase "missing field number gives helpful message" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message Foo {"
                    , "  string name = ;"
                    , "}"
                    ]
            case parseProtoFile "test.proto" input of
              Left e -> do
                let msg = renderParseError e
                assertBool "should mention field number" ("field number" `isInfixOf` msg || "integer" `isInfixOf` msg)
                assertBool "should show source line" ("string name" `isInfixOf` msg)
              Right _ -> assertFailure "Should have failed to parse"
        , testCase "missing equals sign gives helpful message" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message Foo {"
                    , "  string name 1;"
                    , "}"
                    ]
            case parseProtoFile "test.proto" input of
              Left e -> do
                let msg = renderParseError e
                assertBool "should mention '='" ("'='" `isInfixOf` msg)
                assertBool "should point to correct column" ("test.proto:3:15" `isInfixOf` msg)
              Right _ -> assertFailure "Should have failed to parse"
        , testCase "invalid syntax version gives clear message" $ do
            let input = "syntax = \"proto4\";\n"
            case parseProtoFile "test.proto" input of
              Left e -> do
                let msg = renderParseError e
                assertBool "should mention proto4" ("proto4" `isInfixOf` msg)
                assertBool "should mention expected versions" ("proto2" `isInfixOf` msg && "proto3" `isInfixOf` msg)
              Right _ -> assertFailure "Should have failed to parse"
        , testCase "unclosed string literal gives clear message" $ do
            let input = "syntax = \"proto3;\n"
            case parseProtoFile "test.proto" input of
              Left e -> do
                let msg = renderParseError e
                assertBool "should mention newline" ("newline" `isInfixOf` msg)
                assertBool "should point to correct location" ("test.proto:1:" `isInfixOf` msg)
              Right _ -> assertFailure "Should have failed to parse"
        , testCase "missing message name gives clear message" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message {"
                    , "  string name = 1;"
                    , "}"
                    ]
            case parseProtoFile "test.proto" input of
              Left e -> do
                let msg = renderParseError e
                assertBool "should mention message name" ("message name" `isInfixOf` msg)
              Right _ -> assertFailure "Should have failed to parse"
        , testCase "unexpected token at top level gives clear message" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "12345"
                    ]
            case parseProtoFile "test.proto" input of
              Left e -> do
                let msg = renderParseError e
                assertBool "should mention expected declarations" ("message" `isInfixOf` msg || "top-level" `isInfixOf` msg)
              Right _ -> assertFailure "Should have failed to parse"
        , testCase "missing comma in map type gives clear message" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "message Foo {"
                    , "  map<string int32> x = 1;"
                    , "}"
                    ]
            case parseProtoFile "test.proto" input of
              Left e -> do
                let msg = renderParseError e
                assertBool "should mention comma" ("','" `isInfixOf` msg)
                assertBool "should point to correct location" ("test.proto:3:14" `isInfixOf` msg)
              Right _ -> assertFailure "Should have failed to parse"
        , testCase "error messages include source context with line numbers" $ do
            let input =
                  unlines'
                    [ "syntax = \"proto3\";"
                    , "package myapp;"
                    , ""
                    , "message User {"
                    , "  string name = 1;"
                    , "  int32 age = 2;"
                    , "  string email = 3"
                    , "}"
                    ]
            case parseProtoFile "api/user.proto" input of
              Left e -> do
                let msg = renderParseError e
                assertBool "should have file:line:col format" ("api/user.proto:" `isInfixOf` msg)
                assertBool "should have --> arrow" ("-->" `isInfixOf` msg)
                assertBool "should have pipe separator" (" | " `isInfixOf` msg)
                assertBool "should have caret pointer" ("^" `isInfixOf` msg)
              Right _ -> assertFailure "Should have failed to parse"
        , testCase "error output rejects invalid inputs" $ do
            let cases =
                  [ ("empty message body missing brace", "syntax = \"proto3\";\nmessage Foo {\n")
                  , ("unknown keyword at top level", "syntax = \"proto3\";\nfoobar baz;\n")
                  , ("missing closing brace for enum", "syntax = \"proto3\";\nenum Foo {\n  A = 0;\n")
                  ]
            mapM_
              ( \(desc, input) ->
                  assertBool desc (isLeft (parseProtoFile "test.proto" input))
              )
              cases
        ]
    ]


complexProto :: Text
complexProto =
  unlines'
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
    , "service PersonService {"
    , "  rpc GetPerson (GetPersonRequest) returns (Person);"
    , "  rpc ListPersons (ListRequest) returns (stream Person);"
    , "  rpc UpdatePerson (stream Person) returns (UpdateResponse) {"
    , "    option deprecated = true;"
    , "  }"
    , "}"
    ]


isNestedMsg :: MessageElement -> Bool
isNestedMsg (MEMessage _) = True
isNestedMsg _ = False


extractFieldDefs :: [MessageElement] -> [FieldDef]
extractFieldDefs = concatMap go
  where
    go (MEField fd) = [fd]
    go _ = []


unlines' :: [Text] -> Text
unlines' = mconcat . fmap (<> "\n")
