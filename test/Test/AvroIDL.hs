module Test.AvroIDL (avroIDLTests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Avro.IDL
import Avro.IDLConvert
import Avro.Protocol (AvroProtocol(..), AvroMessage(..), AvroParam(..))
import Avro.Schema

t :: [Text] -> Text
t = T.unlines

avroIDLTests :: TestTree
avroIDLTests = testGroup "Avro IDL"
  [ testCase "parse simple protocol with one record" $ do
      let input = "protocol Simple { record Msg { string body; } }" :: Text
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          aidlProtocolName idl @?= "Simple"
          V.length (aidlDeclarations idl) @?= 1
          case V.head (aidlDeclarations idl) of
            IDLRecord name fields _ _ -> do
              name @?= "Msg"
              V.length fields @?= 1
              ifdName (V.head fields) @?= "body"
              ifdType (V.head fields) @?= ITString
            other -> assertFailure $ "expected record, got " ++ show other

  , testCase "parse record with all field types" $ do
      let input = t
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
        Left err -> assertFailure err
        Right idl -> do
          case V.head (aidlDeclarations idl) of
            IDLRecord _ fields _ _ -> do
              V.length fields @?= 12
              ifdType (fields V.! 0) @?= ITNull
              ifdType (fields V.! 1) @?= ITBoolean
              ifdType (fields V.! 2) @?= ITInt
              ifdType (fields V.! 3) @?= ITLong
              ifdType (fields V.! 4) @?= ITFloat
              ifdType (fields V.! 5) @?= ITDouble
              ifdType (fields V.! 6) @?= ITBytes
              ifdType (fields V.! 7) @?= ITString
              ifdType (fields V.! 8) @?= ITArray ITInt
              ifdType (fields V.! 9) @?= ITMap ITString
              ifdType (fields V.! 10) @?= ITUnion (V.fromList [ITNull, ITString])
              ifdType (fields V.! 11) @?= ITNamed "OtherRecord"
            other -> assertFailure $ "expected record, got " ++ show other

  , testCase "parse enum" $ do
      let input = "protocol E { enum Color { RED, GREEN, BLUE } }" :: Text
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          case V.head (aidlDeclarations idl) of
            IDLEnum name syms _ -> do
              name @?= "Color"
              syms @?= V.fromList ["RED", "GREEN", "BLUE"]
            other -> assertFailure $ "expected enum, got " ++ show other

  , testCase "parse fixed" $ do
      let input = "protocol F { fixed MD5(16); }" :: Text
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          case V.head (aidlDeclarations idl) of
            IDLFixed name sz -> do
              name @?= "MD5"
              sz @?= 16
            other -> assertFailure $ "expected fixed, got " ++ show other

  , testCase "parse error type" $ do
      let input = t
            [ "protocol Err {"
            , "  error InvalidInput {"
            , "    string message;"
            , "    int code;"
            , "  }"
            , "}"
            ]
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          case V.head (aidlDeclarations idl) of
            IDLError name fields _ -> do
              name @?= "InvalidInput"
              V.length fields @?= 2
              ifdName (fields V.! 0) @?= "message"
              ifdName (fields V.! 1) @?= "code"
            other -> assertFailure $ "expected error, got " ++ show other

  , testCase "parse methods (normal and oneway)" $ do
      let input = t
            [ "protocol Svc {"
            , "  string greet(string name);"
            , "  void sendMessage(string to, string body) oneway;"
            , "}"
            ]
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          V.length (aidlMessages idl) @?= 2
          let msg0 = aidlMessages idl V.! 0
          imName msg0 @?= "greet"
          imReturn msg0 @?= ITString
          imOneway msg0 @?= False
          V.length (imParams msg0) @?= 1
          fst (V.head (imParams msg0)) @?= ITString
          snd (V.head (imParams msg0)) @?= "name"

          let msg1 = aidlMessages idl V.! 1
          imName msg1 @?= "sendMessage"
          imReturn msg1 @?= ITNamed "void"
          imOneway msg1 @?= True
          V.length (imParams msg1) @?= 2

  , testCase "parse with namespace annotation" $ do
      let input = "@namespace(\"com.example\") protocol NS { }" :: Text
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          aidlNamespace idl @?= Just "com.example"
          aidlProtocolName idl @?= "NS"

  , testCase "parse imports (idl, protocol, schema)" $ do
      let input = t
            [ "protocol Imp {"
            , "  import idl \"other.avdl\";"
            , "  import protocol \"other.avpr\";"
            , "  import schema \"other.avsc\";"
            , "}"
            ]
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          V.length (aidlImports idl) @?= 3
          aidlImports idl V.! 0 @?= ImportIDL "other.avdl"
          aidlImports idl V.! 1 @?= ImportProtocol "other.avpr"
          aidlImports idl V.! 2 @?= ImportSchema "other.avsc"

  , testCase "parse field defaults (null, numbers, strings, empty array, empty map)" $ do
      let input = t
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
        Left err -> assertFailure err
        Right idl -> do
          case V.head (aidlDeclarations idl) of
            IDLRecord _ fields _ _ -> do
              V.length fields @?= 6
              ifdDefault (fields V.! 0) @?= Just "null"
              ifdDefault (fields V.! 1) @?= Just "42"
              ifdDefault (fields V.! 2) @?= Just "\"hello\""
              ifdDefault (fields V.! 3) @?= Just "[]"
              ifdDefault (fields V.! 4) @?= Just "{}"
              ifdDefault (fields V.! 5) @?= Just "3.14"
            other -> assertFailure $ "expected record, got " ++ show other

  , testCase "parse doc comments" $ do
      let input = t
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
        Left err -> assertFailure err
        Right idl -> do
          case V.head (aidlDeclarations idl) of
            IDLRecord _ fields doc _ -> do
              doc @?= Just "A person record"
              ifdDoc (V.head fields) @?= Just "The name"
            other -> assertFailure $ "expected record, got " ++ show other
          let msg = V.head (aidlMessages idl)
          imDoc msg @?= Just "Get greeting"

  , testCase "parse field annotations (@order, @logicalType)" $ do
      let input = t
            [ "protocol Ann {"
            , "  record Annotated {"
            , "    @order(\"ascending\") string name;"
            , "    @logicalType(\"timestamp-millis\") long created_at;"
            , "  }"
            , "}"
            ]
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          case V.head (aidlDeclarations idl) of
            IDLRecord _ fields _ _ -> do
              ifdOrder (fields V.! 0) @?= Just "ascending"
              let anns = ifdAnnotations (fields V.! 1)
              V.length anns @?= 1
              V.head anns @?= ("logicalType", "timestamp-millis")
            other -> assertFailure $ "expected record, got " ++ show other

  , testCase "parse record with @aliases annotation" $ do
      let input = t
            [ "protocol A {"
            , "  @aliases([\"OldPerson\", \"LegacyPerson\"])"
            , "  record Person {"
            , "    string name;"
            , "  }"
            , "}"
            ]
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          case V.head (aidlDeclarations idl) of
            IDLRecord _ _ _ aliases -> do
              V.length aliases @?= 2
              aliases V.! 0 @?= "OldPerson"
              aliases V.! 1 @?= "LegacyPerson"
            other -> assertFailure $ "expected record, got " ++ show other

  , testCase "convert IDL record to AvroType" $ do
      let input = t
            [ "protocol Conv {"
            , "  record User {"
            , "    string name;"
            , "    int age;"
            , "  }"
            , "}"
            ]
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          let decl = V.head (aidlDeclarations idl)
              ty = idlToType decl
          case ty of
            AvroRecord{avroRecordName = n, avroRecordFields = fs} -> do
              n @?= "User"
              V.length fs @?= 2
              avroFieldName (fs V.! 0) @?= "name"
              avroFieldType (fs V.! 0) @?= AvroPrimitive AvroString
              avroFieldName (fs V.! 1) @?= "age"
              avroFieldType (fs V.! 1) @?= AvroPrimitive AvroInt
            _ -> assertFailure "expected AvroRecord"

  , testCase "convert IDL protocol to AvroProtocol" $ do
      let input = t
            [ "@namespace(\"com.example\")"
            , "protocol MyProto {"
            , "  record Person {"
            , "    string name;"
            , "  }"
            , "  Person getPerson(string name);"
            , "}"
            ]
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          let proto = idlToProtocol idl
          protoName proto @?= "MyProto"
          protoNamespace proto @?= Just "com.example"
          length (protoTypes proto) @?= 1
          length (protoMessages proto) @?= 1
          let (msgName, msg) = head (protoMessages proto)
          msgName @?= "getPerson"
          msgOneWay msg @?= False
          length (msgRequest msg) @?= 1
          paramName (head (msgRequest msg)) @?= "name"
          paramType (head (msgRequest msg)) @?= AvroPrimitive AvroString

  , testCase "parse then convert produces valid structure" $ do
      let input = t
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
        Left err -> assertFailure err
        Right idl -> do
          aidlNamespace idl @?= Just "com.example"
          aidlProtocolName idl @?= "Full"
          V.length (aidlDeclarations idl) @?= 4
          V.length (aidlMessages idl) @?= 2

          let proto = idlToProtocol idl
          protoName proto @?= "Full"
          protoNamespace proto @?= Just "com.example"
          length (protoTypes proto) @?= 4
          length (protoMessages proto) @?= 2

          case protoTypes proto !! 0 of
            AvroRecord{avroRecordName = n, avroRecordFields = fs} -> do
              n @?= "Person"
              V.length fs @?= 5
            _ -> assertFailure "expected record Person"

          case protoTypes proto !! 1 of
            AvroEnum{avroEnumName = n, avroEnumSymbols = ss} -> do
              n @?= "Color"
              ss @?= V.fromList ["RED", "GREEN", "BLUE"]
            _ -> assertFailure "expected enum Color"

          case protoTypes proto !! 2 of
            AvroFixed{avroFixedName = n, avroFixedSize = sz} -> do
              n @?= "MD5"
              sz @?= 16
            _ -> assertFailure "expected fixed MD5"

  , testCase "convert IDL enum to AvroType" $ do
      let decl = IDLEnum "Status" (V.fromList ["ACTIVE", "INACTIVE"]) (Just "Status enum")
          ty = idlToType decl
      case ty of
        AvroEnum{avroEnumName = n, avroEnumSymbols = ss, avroEnumDoc = d} -> do
          n @?= "Status"
          ss @?= V.fromList ["ACTIVE", "INACTIVE"]
          d @?= Just "Status enum"
        _ -> assertFailure "expected AvroEnum"

  , testCase "convert IDL fixed to AvroType" $ do
      let decl = IDLFixed "Hash" 32
          ty = idlToType decl
      case ty of
        AvroFixed{avroFixedName = n, avroFixedSize = sz} -> do
          n @?= "Hash"
          sz @?= 32
        _ -> assertFailure "expected AvroFixed"

  , testCase "convert IDL error to AvroType with error prop" $ do
      let decl = IDLError "MyError"
                   (V.singleton (AvroIDLField ITString "msg" Nothing V.empty Nothing Nothing))
                   Nothing
          ty = idlToType decl
      case ty of
        AvroRecord{avroRecordName = n, avroRecordProps = ps} -> do
          n @?= "MyError"
          Map.lookup "error" ps @?= Just "true"
        _ -> assertFailure "expected AvroRecord with error prop"

  , testCase "parse decimal type" $ do
      let input = t
            [ "protocol D {"
            , "  record Money {"
            , "    decimal(10, 2) amount;"
            , "  }"
            , "}"
            ]
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          case V.head (aidlDeclarations idl) of
            IDLRecord _ fields _ _ -> do
              ifdType (V.head fields) @?= ITDecimal 10 2
            other -> assertFailure $ "expected record, got " ++ show other

  , testCase "convert decimal type" $ do
      let decl = IDLRecord "R"
                   (V.singleton (AvroIDLField (ITDecimal 10 2) "amount" Nothing V.empty Nothing Nothing))
                   Nothing V.empty
          ty = idlToType decl
      case ty of
        AvroRecord{avroRecordFields = fs} ->
          case avroFieldType (V.head fs) of
            AvroLogical{avroLogicalType = DecimalLogical p s} -> do
              p @?= 10
              s @?= 2
            _ -> assertFailure "expected AvroLogical decimal"
        _ -> assertFailure "expected AvroRecord"

  , testCase "parse negative default" $ do
      let input = t
            [ "protocol N {"
            , "  record R { int x = -1; }"
            , "}"
            ]
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          case V.head (aidlDeclarations idl) of
            IDLRecord _ fields _ _ ->
              ifdDefault (V.head fields) @?= Just "-1"
            other -> assertFailure $ "expected record, got " ++ show other

  , testCase "parse method with throws" $ do
      let input = t
            [ "protocol T {"
            , "  string doWork(int x) throws MyError;"
            , "}"
            ]
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          let msg = V.head (aidlMessages idl)
          imErrors msg @?= V.fromList ["MyError"]

  , testCase "empty protocol" $ do
      let input = "protocol Empty { }" :: Text
      case parseAvroIDL input of
        Left err -> assertFailure err
        Right idl -> do
          aidlProtocolName idl @?= "Empty"
          V.length (aidlDeclarations idl) @?= 0
          V.length (aidlMessages idl) @?= 0
          V.length (aidlImports idl) @?= 0
  ]
