module Test.AvroIDL (avroIDLTests) where

import Avro.IDL
import Avro.IDLConvert
import Avro.Protocol (AvroMessage (..), AvroParam (..), AvroProtocol (..))
import Avro.Schema
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd


t :: [Text] -> Text
t = T.unlines


avroIDLTests :: Spec
avroIDLTests =
  describe "Avro IDL" $
    sequence_
      [ it "parse simple protocol with one record" $ do
          let input = "protocol Simple { record Msg { string body; } }" :: Text
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              aidlProtocolName idl `shouldBe` "Simple"
              V.length (aidlDeclarations idl) `shouldBe` 1
              case V.head (aidlDeclarations idl) of
                IDLRecord name fields _ _ -> do
                  name `shouldBe` "Msg"
                  V.length fields `shouldBe` 1
                  ifdName (V.head fields) `shouldBe` "body"
                  ifdType (V.head fields) `shouldBe` ITString
                other -> expectationFailure $ "expected record, got " ++ show other
      , it "parse record with all field types" $ do
          let input =
                t
                  [ "protocol Types {"
                  , "  record AllTypes {"
                  , "    null n;"
                  , "    boolean b;"
                  , "    int i;"
                  , "    long l;"
                  , "    float f;"
                  , "    double d;"
                  , "    bytes by;"
                  , "    string s;"
                  , "    array<int> arr;"
                  , "    map<string> m;"
                  , "    union { null, string } u;"
                  , "    OtherRecord ref;"
                  , "  }"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              case V.head (aidlDeclarations idl) of
                IDLRecord _ fields _ _ -> do
                  V.length fields `shouldBe` 12
                  ifdType (fields V.! 0) `shouldBe` ITNull
                  ifdType (fields V.! 1) `shouldBe` ITBoolean
                  ifdType (fields V.! 2) `shouldBe` ITInt
                  ifdType (fields V.! 3) `shouldBe` ITLong
                  ifdType (fields V.! 4) `shouldBe` ITFloat
                  ifdType (fields V.! 5) `shouldBe` ITDouble
                  ifdType (fields V.! 6) `shouldBe` ITBytes
                  ifdType (fields V.! 7) `shouldBe` ITString
                  ifdType (fields V.! 8) `shouldBe` ITArray ITInt
                  ifdType (fields V.! 9) `shouldBe` ITMap ITString
                  ifdType (fields V.! 10) `shouldBe` ITUnion (V.fromList [ITNull, ITString])
                  ifdType (fields V.! 11) `shouldBe` ITNamed "OtherRecord"
                other -> expectationFailure $ "expected record, got " ++ show other
      , it "parse enum" $ do
          let input = "protocol E { enum Color { RED, GREEN, BLUE } }" :: Text
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              case V.head (aidlDeclarations idl) of
                IDLEnum name syms _ -> do
                  name `shouldBe` "Color"
                  syms `shouldBe` V.fromList ["RED", "GREEN", "BLUE"]
                other -> expectationFailure $ "expected enum, got " ++ show other
      , it "parse fixed" $ do
          let input = "protocol F { fixed MD5(16); }" :: Text
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              case V.head (aidlDeclarations idl) of
                IDLFixed name sz -> do
                  name `shouldBe` "MD5"
                  sz `shouldBe` 16
                other -> expectationFailure $ "expected fixed, got " ++ show other
      , it "parse error type" $ do
          let input =
                t
                  [ "protocol Err {"
                  , "  error InvalidInput {"
                  , "    string message;"
                  , "    int code;"
                  , "  }"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              case V.head (aidlDeclarations idl) of
                IDLError name fields _ -> do
                  name `shouldBe` "InvalidInput"
                  V.length fields `shouldBe` 2
                  ifdName (fields V.! 0) `shouldBe` "message"
                  ifdName (fields V.! 1) `shouldBe` "code"
                other -> expectationFailure $ "expected error, got " ++ show other
      , it "parse methods (normal and oneway)" $ do
          let input =
                t
                  [ "protocol Svc {"
                  , "  string greet(string name);"
                  , "  void sendMessage(string to, string body) oneway;"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              V.length (aidlMessages idl) `shouldBe` 2
              let msg0 = aidlMessages idl V.! 0
              imName msg0 `shouldBe` "greet"
              imReturn msg0 `shouldBe` ITString
              imOneway msg0 `shouldBe` False
              V.length (imParams msg0) `shouldBe` 1
              fst (V.head (imParams msg0)) `shouldBe` ITString
              snd (V.head (imParams msg0)) `shouldBe` "name"

              let msg1 = aidlMessages idl V.! 1
              imName msg1 `shouldBe` "sendMessage"
              imReturn msg1 `shouldBe` ITNamed "void"
              imOneway msg1 `shouldBe` True
              V.length (imParams msg1) `shouldBe` 2
      , it "parse with namespace annotation" $ do
          let input = "@namespace(\"com.example\") protocol NS { }" :: Text
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              aidlNamespace idl `shouldBe` Just "com.example"
              aidlProtocolName idl `shouldBe` "NS"
      , it "parse imports (idl, protocol, schema)" $ do
          let input =
                t
                  [ "protocol Imp {"
                  , "  import idl \"other.avdl\";"
                  , "  import protocol \"other.avpr\";"
                  , "  import schema \"other.avsc\";"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              V.length (aidlImports idl) `shouldBe` 3
              aidlImports idl V.! 0 `shouldBe` ImportIDL "other.avdl"
              aidlImports idl V.! 1 `shouldBe` ImportProtocol "other.avpr"
              aidlImports idl V.! 2 `shouldBe` ImportSchema "other.avsc"
      , it "parse field defaults (null, numbers, strings, empty array, empty map)" $ do
          let input =
                t
                  [ "protocol Dflt {"
                  , "  record Defaults {"
                  , "    union { null, string } opt = null;"
                  , "    int count = 42;"
                  , "    string label = \"hello\";"
                  , "    array<string> tags = [];"
                  , "    map<int> scores = {};"
                  , "    double rate = 3.14;"
                  , "  }"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              case V.head (aidlDeclarations idl) of
                IDLRecord _ fields _ _ -> do
                  V.length fields `shouldBe` 6
                  ifdDefault (fields V.! 0) `shouldBe` Just "null"
                  ifdDefault (fields V.! 1) `shouldBe` Just "42"
                  ifdDefault (fields V.! 2) `shouldBe` Just "\"hello\""
                  ifdDefault (fields V.! 3) `shouldBe` Just "[]"
                  ifdDefault (fields V.! 4) `shouldBe` Just "{}"
                  ifdDefault (fields V.! 5) `shouldBe` Just "3.14"
                other -> expectationFailure $ "expected record, got " ++ show other
      , it "parse doc comments" $ do
          let input =
                t
                  [ "protocol Doc {"
                  , "  /** A person record */"
                  , "  record Person {"
                  , "    /** The name */"
                  , "    string name;"
                  , "  }"
                  , "  /** Get greeting */"
                  , "  string greet(string name);"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              case V.head (aidlDeclarations idl) of
                IDLRecord _ fields doc _ -> do
                  doc `shouldBe` Just "A person record"
                  ifdDoc (V.head fields) `shouldBe` Just "The name"
                other -> expectationFailure $ "expected record, got " ++ show other
              let msg = V.head (aidlMessages idl)
              imDoc msg `shouldBe` Just "Get greeting"
      , it "parse field annotations (@order, @logicalType)" $ do
          let input =
                t
                  [ "protocol Ann {"
                  , "  record Annotated {"
                  , "    @order(\"ascending\") string name;"
                  , "    @logicalType(\"timestamp-millis\") long created_at;"
                  , "  }"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              case V.head (aidlDeclarations idl) of
                IDLRecord _ fields _ _ -> do
                  ifdOrder (fields V.! 0) `shouldBe` Just "ascending"
                  let anns = ifdAnnotations (fields V.! 1)
                  V.length anns `shouldBe` 1
                  V.head anns `shouldBe` ("logicalType", "timestamp-millis")
                other -> expectationFailure $ "expected record, got " ++ show other
      , it "parse record with @aliases annotation" $ do
          let input =
                t
                  [ "protocol A {"
                  , "  @aliases([\"OldPerson\", \"LegacyPerson\"])"
                  , "  record Person {"
                  , "    string name;"
                  , "  }"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              case V.head (aidlDeclarations idl) of
                IDLRecord _ _ _ aliases -> do
                  V.length aliases `shouldBe` 2
                  aliases V.! 0 `shouldBe` "OldPerson"
                  aliases V.! 1 `shouldBe` "LegacyPerson"
                other -> expectationFailure $ "expected record, got " ++ show other
      , it "convert IDL record to AvroType" $ do
          let input =
                t
                  [ "protocol Conv {"
                  , "  record User {"
                  , "    string name;"
                  , "    int age;"
                  , "  }"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              let decl = V.head (aidlDeclarations idl)
                  ty = idlToType decl
              case ty of
                AvroRecord {avroRecordName = n, avroRecordFields = fs} -> do
                  n `shouldBe` "User"
                  V.length fs `shouldBe` 2
                  avroFieldName (fs V.! 0) `shouldBe` "name"
                  avroFieldType (fs V.! 0) `shouldBe` AvroPrimitive AvroString
                  avroFieldName (fs V.! 1) `shouldBe` "age"
                  avroFieldType (fs V.! 1) `shouldBe` AvroPrimitive AvroInt
                _ -> expectationFailure "expected AvroRecord"
      , it "convert IDL protocol to AvroProtocol" $ do
          let input =
                t
                  [ "@namespace(\"com.example\")"
                  , "protocol MyProto {"
                  , "  record Person {"
                  , "    string name;"
                  , "  }"
                  , "  Person getPerson(string name);"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              let proto = idlToProtocol idl
              protoName proto `shouldBe` "MyProto"
              protoNamespace proto `shouldBe` Just "com.example"
              length (protoTypes proto) `shouldBe` 1
              length (protoMessages proto) `shouldBe` 1
              let (msgName, msg) = head (protoMessages proto)
              msgName `shouldBe` "getPerson"
              msgOneWay msg `shouldBe` False
              length (msgRequest msg) `shouldBe` 1
              paramName (head (msgRequest msg)) `shouldBe` "name"
              paramType (head (msgRequest msg)) `shouldBe` AvroPrimitive AvroString
      , it "parse then convert produces valid structure" $ do
          let input =
                t
                  [ "@namespace(\"com.example\")"
                  , "protocol Full {"
                  , "  record Person {"
                  , "    string name;"
                  , "    int age;"
                  , "    union { null, string } email = null;"
                  , "    array<string> tags = [];"
                  , "    map<int> scores = {};"
                  , "  }"
                  , "  enum Color { RED, GREEN, BLUE }"
                  , "  fixed MD5(16);"
                  , "  Person getPerson(string name);"
                  , "  void sendMessage(string to, string body) oneway;"
                  , "  error InvalidInput {"
                  , "    string message;"
                  , "    int code;"
                  , "  }"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              aidlNamespace idl `shouldBe` Just "com.example"
              aidlProtocolName idl `shouldBe` "Full"
              V.length (aidlDeclarations idl) `shouldBe` 4
              V.length (aidlMessages idl) `shouldBe` 2

              let proto = idlToProtocol idl
              protoName proto `shouldBe` "Full"
              protoNamespace proto `shouldBe` Just "com.example"
              length (protoTypes proto) `shouldBe` 4
              length (protoMessages proto) `shouldBe` 2

              case protoTypes proto !! 0 of
                AvroRecord {avroRecordName = n, avroRecordFields = fs} -> do
                  n `shouldBe` "Person"
                  V.length fs `shouldBe` 5
                _ -> expectationFailure "expected record Person"

              case protoTypes proto !! 1 of
                AvroEnum {avroEnumName = n, avroEnumSymbols = ss} -> do
                  n `shouldBe` "Color"
                  ss `shouldBe` V.fromList ["RED", "GREEN", "BLUE"]
                _ -> expectationFailure "expected enum Color"

              case protoTypes proto !! 2 of
                AvroFixed {avroFixedName = n, avroFixedSize = sz} -> do
                  n `shouldBe` "MD5"
                  sz `shouldBe` 16
                _ -> expectationFailure "expected fixed MD5"
      , it "convert IDL enum to AvroType" $ do
          let decl = IDLEnum "Status" (V.fromList ["ACTIVE", "INACTIVE"]) (Just "Status enum")
              ty = idlToType decl
          case ty of
            AvroEnum {avroEnumName = n, avroEnumSymbols = ss, avroEnumDoc = d} -> do
              n `shouldBe` "Status"
              ss `shouldBe` V.fromList ["ACTIVE", "INACTIVE"]
              d `shouldBe` Just "Status enum"
            _ -> expectationFailure "expected AvroEnum"
      , it "convert IDL fixed to AvroType" $ do
          let decl = IDLFixed "Hash" 32
              ty = idlToType decl
          case ty of
            AvroFixed {avroFixedName = n, avroFixedSize = sz} -> do
              n `shouldBe` "Hash"
              sz `shouldBe` 32
            _ -> expectationFailure "expected AvroFixed"
      , it "convert IDL error to AvroType with error prop" $ do
          let decl =
                IDLError
                  "MyError"
                  (V.singleton (AvroIDLField ITString "msg" Nothing V.empty Nothing Nothing))
                  Nothing
              ty = idlToType decl
          case ty of
            AvroRecord {avroRecordName = n, avroRecordProps = ps} -> do
              n `shouldBe` "MyError"
              Map.lookup "error" ps `shouldBe` Just "true"
            _ -> expectationFailure "expected AvroRecord with error prop"
      , it "parse decimal type" $ do
          let input =
                t
                  [ "protocol D {"
                  , "  record Money {"
                  , "    decimal(10, 2) amount;"
                  , "  }"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              case V.head (aidlDeclarations idl) of
                IDLRecord _ fields _ _ -> do
                  ifdType (V.head fields) `shouldBe` ITDecimal 10 2
                other -> expectationFailure $ "expected record, got " ++ show other
      , it "convert decimal type" $ do
          let decl =
                IDLRecord
                  "R"
                  (V.singleton (AvroIDLField (ITDecimal 10 2) "amount" Nothing V.empty Nothing Nothing))
                  Nothing
                  V.empty
              ty = idlToType decl
          case ty of
            AvroRecord {avroRecordFields = fs} ->
              case avroFieldType (V.head fs) of
                AvroLogical {avroLogicalType = DecimalLogical p s} -> do
                  p `shouldBe` 10
                  s `shouldBe` 2
                _ -> expectationFailure "expected AvroLogical decimal"
            _ -> expectationFailure "expected AvroRecord"
      , it "parse negative default" $ do
          let input =
                t
                  [ "protocol N {"
                  , "  record R { int x = -1; }"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              case V.head (aidlDeclarations idl) of
                IDLRecord _ fields _ _ ->
                  ifdDefault (V.head fields) `shouldBe` Just "-1"
                other -> expectationFailure $ "expected record, got " ++ show other
      , it "parse method with throws" $ do
          let input =
                t
                  [ "protocol T {"
                  , "  string doWork(int x) throws MyError;"
                  , "}"
                  ]
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              let msg = V.head (aidlMessages idl)
              imErrors msg `shouldBe` V.fromList ["MyError"]
      , it "empty protocol" $ do
          let input = "protocol Empty { }" :: Text
          case parseAvroIDL input of
            Left err -> expectationFailure err
            Right idl -> do
              aidlProtocolName idl `shouldBe` "Empty"
              V.length (aidlDeclarations idl) `shouldBe` 0
              V.length (aidlMessages idl) `shouldBe` 0
              V.length (aidlImports idl) `shouldBe` 0
      ]
