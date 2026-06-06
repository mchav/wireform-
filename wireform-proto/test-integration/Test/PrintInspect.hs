module Test.PrintInspect (printInspectTests) where

import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Proto.IDL.AST
import Proto.IDL.Inspect
import Proto.IDL.Parser
import Proto.IDL.Print
import Test.Syd


printInspectTests :: Spec
printInspectTests =
  describe
    "Print & Inspect" $ sequence_
    [ describe
        "Exact printing" $ sequence_
        [ it "roundtrip: parse -> print -> parse yields same AST" $ do
            let original = complexProto
            case parseProtoFile "<test>" original of
              Left e -> expectationFailure (show e)
              Right ast1 -> do
                let printed = printProtoFile ast1
                case parseProtoFile "<printed>" printed of
                  Left e -> expectationFailure ("Re-parse failed: " <> show e <> "\n\nPrinted:\n" <> T.unpack printed)
                  Right ast2 -> ast1 `shouldBe` ast2
        , it "syntax declaration" $ do
            let ast = ProtoFile Proto3 Nothing [] [] [] Nothing
            T.isInfixOf "syntax = \"proto3\";" (printProtoFile ast) `shouldBe` True
        , it "syntax proto2" $ do
            let ast = ProtoFile Proto2 Nothing [] [] [] Nothing
            T.isInfixOf "syntax = \"proto2\";" (printProtoFile ast) `shouldBe` True
        , it "package declaration" $ do
            let ast = ProtoFile Proto3 (Just "my.package") [] [] [] Nothing
            T.isInfixOf "package my.package;" (printProtoFile ast) `shouldBe` True
        , it "import" $ do
            let ast = ProtoFile Proto3 Nothing [ImportDef () Nothing "other.proto"] [] [] Nothing
            T.isInfixOf "import \"other.proto\";" (printProtoFile ast) `shouldBe` True
        , it "public import" $ do
            let ast = ProtoFile Proto3 Nothing [ImportDef () (Just ImportPublic) "other.proto"] [] [] Nothing
            T.isInfixOf "import public \"other.proto\";" (printProtoFile ast) `shouldBe` True
        , it "simple message roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "message Foo {"
                    , "  string name = 1;"
                    , "  int32 value = 2;"
                    , "}"
                    ]
            roundtripTest src
        , it "enum roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "enum Status {"
                    , "  UNKNOWN = 0;"
                    , "  ACTIVE = 1;"
                    , "}"
                    ]
            roundtripTest src
        , it "service roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "service Greeter {"
                    , "  rpc SayHello (HelloReq) returns (HelloReply);"
                    , "}"
                    ]
            roundtripTest src
        , it "map field roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "message Foo {"
                    , "  map<string, int32> labels = 1;"
                    , "}"
                    ]
            roundtripTest src
        , it "oneof roundtrip" $ do
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
        , it "reserved roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "message Foo {"
                    , "  reserved 2, 10 to 20;"
                    , "  reserved \"old_field\";"
                    , "}"
                    ]
            roundtripTest src
        , it "field options roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "message Foo {"
                    , "  string name = 1 [deprecated = true];"
                    , "}"
                    ]
            roundtripTest src
        , it "option with extension name roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "option (my_custom_opt) = true;"
                    , "message Foo {"
                    , "  string name = 1;"
                    , "}"
                    ]
            roundtripTest src
        , it "nested message roundtrip" $ do
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
        , it "streaming rpc roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "service Chat {"
                    , "  rpc Stream (stream Msg) returns (stream Msg);"
                    , "}"
                    ]
            roundtripTest src
        , it "proto2 group roundtrip (desugared to message + field)" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto2\";"
                    , "message M {"
                    , "  repeated group Result = 1 {"
                    , "    optional int32 x = 1;"
                    , "  }"
                    , "}"
                    ]
            roundtripTest src
        , it "extensions with options roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto2\";"
                    , "message M {"
                    , "  extensions 4 to 8 [verification = UNVERIFIED];"
                    , "}"
                    ]
            roundtripTest src
        , it "nested extend roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto2\";"
                    , "message M {"
                    , "  extend N {"
                    , "    optional int32 e = 100;"
                    , "  }"
                    , "}"
                    ]
            roundtripTest src
        , it "aggregate with list / extension-key values roundtrip" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "option (x) = { tags: [\"a\", \"b\"] };"
                    , "option (y) = { [foo.bar]: 1 };"
                    ]
            roundtripTest src
        , it "editions reserved identifiers roundtrip" $ do
            let src =
                  T.unlines
                    [ "edition = \"2023\";"
                    , "message M {"
                    , "  reserved foo, bar;"
                    , "}"
                    ]
            roundtripTest src
        , it "editions reserved identifiers print unquoted" $ do
            let src =
                  T.unlines
                    [ "edition = \"2023\";"
                    , "message M {"
                    , "  reserved foo, bar;"
                    , "}"
                    ]
            case parseProtoFile "<test>" src of
              Left e -> expectationFailure (show e)
              Right pf -> do
                let printed = printProtoFile pf
                T.isInfixOf "reserved foo, bar;" printed `shouldBe` True
                T.isInfixOf "\"foo\"" printed `shouldBe` False
        , it "proto3 reserved names still print quoted" $ do
            let src =
                  T.unlines
                    [ "syntax = \"proto3\";"
                    , "message M {"
                    , "  reserved \"foo\", \"bar\";"
                    , "}"
                    ]
            case parseProtoFile "<test>" src of
              Left e -> expectationFailure (show e)
              Right pf -> do
                let printed = printProtoFile pf
                T.isInfixOf "reserved \"foo\", \"bar\";" printed `shouldBe` True
        ]
    , describe
        "Exact printing (byte-for-byte)" $ sequence_
        [ it "reproduces a complex file byte-for-byte" $
            exactPrintTest complexProto
        , it "preserves a trailing ';' after a message block" $
            exactPrintTest $
              T.unlines
                [ "syntax = \"proto3\";"
                , "message Foo {"
                , "  string name = 1;"
                , "};"
                ]
        , it "preserves a trailing ';' after enum and service blocks" $
            exactPrintTest $
              T.unlines
                [ "syntax = \"proto3\";"
                , "enum Status {"
                , "  UNKNOWN = 0;"
                , "};"
                , "service Greeter {"
                , "  rpc SayHello (Req) returns (Resp);"
                , "};"
                ]
        , it "preserves stray ';' between and inside declarations" $
            exactPrintTest $
              T.unlines
                [ "syntax = \"proto3\";"
                , ";"
                , "message A {"
                , "  ;"
                , "  int32 x = 1;"
                , "  ;"
                , "  message Inner {"
                , "    int32 y = 1;"
                , "  };"
                , "}"
                , ";"
                ]
        , it "preserves proto2 group source verbatim" $
            exactPrintTest $
              T.unlines
                [ "syntax = \"proto2\";"
                , "message M {"
                , "  repeated group Result = 1 {"
                , "    optional int32 x = 1;"
                , "  }"
                , "}"
                ]
        , it "preserves extensions options source verbatim" $
            exactPrintTest $
              T.unlines
                [ "syntax = \"proto2\";"
                , "message M {"
                , "  extensions 4 to 8 [verification = UNVERIFIED];"
                , "}"
                ]
        ]
    , describe
        "AST inspection" $ sequence_
        [ it "allMessages finds top-level and nested" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> expectationFailure (show e)
              Right pf -> do
                let msgs = allMessages pf
                (any (\m -> msgName m == "Person") msgs) `shouldBe` True
                (any (\m -> msgName m == "PhoneNumber") msgs) `shouldBe` True
                (any (\m -> msgName m == "Address") msgs) `shouldBe` True
                (any (\m -> msgName m == "AddressBook") msgs) `shouldBe` True
        , it "findMessage" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> expectationFailure (show e)
              Right pf -> do
                (isJust (findMessage "Person" pf)) `shouldBe` True
                (isNothing (findMessage "Nonexistent" pf)) `shouldBe` True
        , it "allEnums" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> expectationFailure (show e)
              Right pf -> do
                let enums = allEnums pf
                (any (\e -> enumName e == "Status") enums) `shouldBe` True
                (any (\e -> enumName e == "PhoneType") enums) `shouldBe` True
        , it "allServices" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> expectationFailure (show e)
              Right pf -> do
                let svcs = allServices pf
                length svcs `shouldBe` 1
                svcName (head svcs) `shouldBe` "PersonService"
        , it "messageFields" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> expectationFailure (show e)
              Right pf -> case findMessage "Person" pf of
                Nothing -> expectationFailure "Person not found"
                Just msg -> do
                  let fields = messageFields msg
                  (any (\f -> fieldName f == "name") fields) `shouldBe` True
                  (any (\f -> fieldName f == "id") fields) `shouldBe` True
                  (any (\f -> fieldName f == "email") fields) `shouldBe` True
        , it "nestedMessages" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> expectationFailure (show e)
              Right pf -> case findMessage "Person" pf of
                Nothing -> expectationFailure "Person not found"
                Just msg -> do
                  let nested = nestedMessages msg
                  (any (\m -> msgName m == "PhoneNumber") nested) `shouldBe` True
                  (any (\m -> msgName m == "Address") nested) `shouldBe` True
        , it "messageOneofs" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> expectationFailure (show e)
              Right pf -> case findMessage "Person" pf of
                Nothing -> expectationFailure "Person not found"
                Just msg -> do
                  let oneofs = messageOneofs msg
                  length oneofs `shouldBe` 1
                  oneofName (head oneofs) `shouldBe` "contact"
        , it "messageMapFields" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> expectationFailure (show e)
              Right pf -> case findMessage "Person" pf of
                Nothing -> expectationFailure "Person not found"
                Just msg -> do
                  let maps = messageMapFields msg
                  length maps `shouldBe` 1
                  mapFieldName (head maps) `shouldBe` "metadata"
        , it "allTypeNames" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> expectationFailure (show e)
              Right pf -> do
                let types = allTypeNames pf
                ("Person" `elem` types) `shouldBe` True
                ("Status" `elem` types) `shouldBe` True
        , it "referencedTypes" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> expectationFailure (show e)
              Right pf -> do
                let refs = referencedTypes pf
                ("PhoneNumber" `elem` refs) `shouldBe` True
                ("Status" `elem` refs) `shouldBe` True
        , it "summarize" $ do
            case parseProtoFile "<test>" complexProto of
              Left e -> expectationFailure (show e)
              Right pf -> do
                let s = summarize pf
                summSyntax s `shouldBe` Proto3
                summPackage s `shouldBe` Just "example.api"
                summMessageCount s > 0 `shouldBe` True
                summEnumCount s > 0 `shouldBe` True
                summServiceCount s `shouldBe` 1
        ]
    ]


roundtripTest :: Text -> IO ()
roundtripTest src =
  case parseProtoFile "<test>" src of
    Left e -> expectationFailure ("Initial parse failed: " <> show e)
    Right ast1 -> do
      let printed = printProtoFile ast1
      case parseProtoFile "<printed>" printed of
        Left e -> expectationFailure ("Re-parse failed: " <> show e <> "\n\nPrinted:\n" <> T.unpack printed)
        Right ast2 -> ast1 `shouldBe` ast2


-- | Parse with spans, then assert 'exactPrint' reproduces the source byte-for-byte.
exactPrintTest :: Text -> IO ()
exactPrintTest src =
  case parseProtoFileWithSpans "<test>" src of
    Left e -> expectationFailure ("Parse failed: " <> show e)
    Right ast -> exactPrint ast `shouldBe` src


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
