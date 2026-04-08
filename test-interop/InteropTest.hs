module Main (main) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Exit (ExitCode(..))
import System.IO (hClose, hSetBinaryMode)
import System.Process
  ( StdStream(..), CreateProcess(..), createProcess, proc, waitForProcess )
import Test.Tasty
import Test.Tasty.HUnit

import qualified MsgPack.Value as MP
import qualified MsgPack.Encode as MPE
import qualified MsgPack.Decode as MPD

import qualified CBOR.Value as C
import qualified CBOR.Encode as CE
import qualified CBOR.Decode as CD

import qualified BSON.Value as B
import qualified BSON.Encode as BE
import qualified BSON.Decode as BD

import qualified Ion.Value as I
import qualified Ion.Encode as IE
import qualified Ion.Decode as ID

import qualified XML.Value as X
import qualified XML.Encode as XE
import qualified XML.Decode as XD

import qualified Avro.Value as AV
import qualified Avro.Schema as AS
import qualified Avro.Encode as AE

import qualified Thrift.Value as TV
import qualified Thrift.Encode as TE

main :: IO ()
main = defaultMain $ testGroup "Cross-Language Interop"
  [ testGroup "MsgPack ↔ Python msgpack" msgpackTests
  , testGroup "CBOR ↔ Python cbor2" cborTests
  , testGroup "XML ↔ Python xml.etree" xmlTests
  , testGroup "BSON ↔ Python bson" bsonTests
  , testGroup "Ion ↔ Python amazon.ion" ionTests
  , testGroup "Avro ↔ Python avro" avroTests
  , testGroup "Thrift ↔ Python thrift" thriftTests
  ]

--------------------------------------------------------------------------------
-- Process helpers
--------------------------------------------------------------------------------

runPythonBinary :: FilePath -> [String] -> BS.ByteString -> IO (Either String BS.ByteString)
runPythonBinary script args input = do
  let cp = (proc "python3" (script : args))
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  result <- try $ createProcess cp
  case result of
    Left (e :: SomeException) ->
      pure $ Left ("Failed to start python3: " ++ show e)
    Right (Just hin, Just hout, Just herr, ph) -> do
      hSetBinaryMode hin True
      hSetBinaryMode hout True
      hSetBinaryMode herr True
      BS.hPut hin input
      hClose hin
      output <- BS.hGetContents hout
      errOutput <- BS.hGetContents herr
      exitCode <- waitForProcess ph
      case exitCode of
        ExitSuccess -> pure (Right output)
        ExitFailure n ->
          pure $ Left $ "Python exited with code " ++ show n
            ++ "\nstderr: " ++ take 500 (show errOutput)
    _ -> pure $ Left "createProcess didn't return expected handles"

checkPythonLib :: String -> IO Bool
checkPythonLib modName = do
  let cp = (proc "python3" ["-c", "import " ++ modName])
        { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  result <- try $ createProcess cp
  case result of
    Left (_ :: SomeException) -> pure False
    Right (Just hin, _, _, ph) -> do
      hClose hin
      exitCode <- waitForProcess ph
      pure (exitCode == ExitSuccess)
    _ -> pure False

withPythonLib :: String -> Assertion -> Assertion
withPythonLib modName action = do
  avail <- checkPythonLib modName
  if avail then action
  else pure ()  -- silently skip if lib not available

--------------------------------------------------------------------------------
-- MsgPack tests
--------------------------------------------------------------------------------

msgpackTests :: [TestTree]
msgpackTests =
  [ testCase "roundtrip map {name: Alice, age: 30}" $ withPythonLib "msgpack" $ do
      let val = MP.Map $ V.fromList
            [ (MP.String "name", MP.String "Alice")
            , (MP.String "age", MP.Int 30)
            ]
      let encoded = MPE.encode val
      result <- runPythonBinary "test-interop/test_msgpack.py" [] encoded
      case result of
        Right pythonEncoded -> case MPD.decode pythonEncoded of
          Right decoded -> assertMsgPackEquiv val decoded
          Left err -> assertFailure $ "wireform decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip array [1, \"hello\", true, nil]" $ withPythonLib "msgpack" $ do
      let val = MP.Array $ V.fromList
            [ MP.Int 1, MP.String "hello", MP.Bool True, MP.Nil ]
      let encoded = MPE.encode val
      result <- runPythonBinary "test-interop/test_msgpack.py" [] encoded
      case result of
        Right pythonEncoded -> case MPD.decode pythonEncoded of
          Right decoded -> assertMsgPackEquiv val decoded
          Left err -> assertFailure $ "wireform decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip nested map" $ withPythonLib "msgpack" $ do
      let val = MP.Map $ V.fromList
            [ (MP.String "outer", MP.Map $ V.fromList
                [ (MP.String "inner", MP.Int 42) ])
            ]
      let encoded = MPE.encode val
      result <- runPythonBinary "test-interop/test_msgpack.py" [] encoded
      case result of
        Right pythonEncoded -> case MPD.decode pythonEncoded of
          Right decoded -> assertMsgPackEquiv val decoded
          Left err -> assertFailure $ "wireform decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip binary data" $ withPythonLib "msgpack" $ do
      let val = MP.Binary (BS.pack [0x00, 0xFF, 0xDE, 0xAD])
      let encoded = MPE.encode val
      result <- runPythonBinary "test-interop/test_msgpack.py" [] encoded
      case result of
        Right pythonEncoded -> case MPD.decode pythonEncoded of
          Right decoded -> decoded @?= val
          Left err -> assertFailure $ "wireform decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip negative int" $ withPythonLib "msgpack" $ do
      let val = MP.Int (-12345)
      let encoded = MPE.encode val
      result <- runPythonBinary "test-interop/test_msgpack.py" [] encoded
      case result of
        Right pythonEncoded -> case MPD.decode pythonEncoded of
          Right decoded -> assertMsgPackEquiv val decoded
          Left err -> assertFailure $ "wireform decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip double" $ withPythonLib "msgpack" $ do
      let val = MP.Double 3.14159
      let encoded = MPE.encode val
      result <- runPythonBinary "test-interop/test_msgpack.py" [] encoded
      case result of
        Right pythonEncoded -> case MPD.decode pythonEncoded of
          Right decoded -> assertMsgPackEquiv val decoded
          Left err -> assertFailure $ "wireform decode failed: " ++ err
        Left err -> assertFailure err
  ]

assertMsgPackEquiv :: MP.Value -> MP.Value -> Assertion
assertMsgPackEquiv expected actual = case (expected, actual) of
  (MP.Int a, MP.Word b) | fromIntegral a == b -> pure ()
  (MP.Word a, MP.Int b) | a == fromIntegral b -> pure ()
  (MP.Int a, MP.Int b)  -> a @?= b
  (MP.Word a, MP.Word b) -> a @?= b
  (MP.Float a, MP.Double b) -> abs (realToFrac a - b) < 1e-5 @? "float mismatch"
  (MP.Double a, MP.Float b) -> abs (a - realToFrac b) < 1e-5 @? "float mismatch"
  (MP.Array as, MP.Array bs) -> do
    V.length as @?= V.length bs
    V.zipWithM_ assertMsgPackEquiv as bs
  (MP.Map as, MP.Map bs) -> do
    V.length as @?= V.length bs
    V.zipWithM_ (\(k1,v1) (k2,v2) -> assertMsgPackEquiv k1 k2 >> assertMsgPackEquiv v1 v2) as bs
  _ -> expected @?= actual

--------------------------------------------------------------------------------
-- CBOR tests
--------------------------------------------------------------------------------

cborTests :: [TestTree]
cborTests =
  [ testCase "roundtrip map {key: 42}" $ withPythonLib "cbor2" $ do
      let val = C.Map $ V.fromList [(C.TextString "key", C.UInt 42)]
      let encoded = CE.encode val
      result <- runPythonBinary "test-interop/test_cbor.py" [] encoded
      case result of
        Right pythonEncoded -> case CD.decode pythonEncoded of
          Right decoded -> assertCBOREquiv val decoded
          Left err -> assertFailure $ "wireform CBOR decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip array [true, false, null]" $ withPythonLib "cbor2" $ do
      let val = C.Array $ V.fromList [C.Bool True, C.Bool False, C.Null]
      let encoded = CE.encode val
      result <- runPythonBinary "test-interop/test_cbor.py" [] encoded
      case result of
        Right pythonEncoded -> case CD.decode pythonEncoded of
          Right decoded -> assertCBOREquiv val decoded
          Left err -> assertFailure $ "wireform CBOR decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip text string" $ withPythonLib "cbor2" $ do
      let val = C.TextString "Hello, CBOR world! 🌍"
      let encoded = CE.encode val
      result <- runPythonBinary "test-interop/test_cbor.py" [] encoded
      case result of
        Right pythonEncoded -> case CD.decode pythonEncoded of
          Right decoded -> decoded @?= val
          Left err -> assertFailure $ "wireform CBOR decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip bytestring" $ withPythonLib "cbor2" $ do
      let val = C.ByteString (BS.pack [0xDE, 0xAD, 0xBE, 0xEF])
      let encoded = CE.encode val
      result <- runPythonBinary "test-interop/test_cbor.py" [] encoded
      case result of
        Right pythonEncoded -> case CD.decode pythonEncoded of
          Right decoded -> decoded @?= val
          Left err -> assertFailure $ "wireform CBOR decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip negative int" $ withPythonLib "cbor2" $ do
      let val = C.NInt 99  -- represents -100
      let encoded = CE.encode val
      result <- runPythonBinary "test-interop/test_cbor.py" [] encoded
      case result of
        Right pythonEncoded -> case CD.decode pythonEncoded of
          Right decoded -> assertCBOREquiv val decoded
          Left err -> assertFailure $ "wireform CBOR decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip float64" $ withPythonLib "cbor2" $ do
      let val = C.Float64 2.71828
      let encoded = CE.encode val
      result <- runPythonBinary "test-interop/test_cbor.py" [] encoded
      case result of
        Right pythonEncoded -> case CD.decode pythonEncoded of
          Right decoded -> assertCBOREquiv val decoded
          Left err -> assertFailure $ "wireform CBOR decode failed: " ++ err
        Left err -> assertFailure err
  ]

assertCBOREquiv :: C.Value -> C.Value -> Assertion
assertCBOREquiv expected actual = case (expected, actual) of
  (C.Float16 a, C.Float64 b) -> abs (realToFrac a - b) < 1e-3 @? "float mismatch"
  (C.Float32 a, C.Float64 b) -> abs (realToFrac a - b) < 1e-5 @? "float mismatch"
  (C.Float64 a, C.Float64 b) -> abs (a - b) < 1e-10 @? "float mismatch"
  (C.Array as, C.Array bs) -> do
    V.length as @?= V.length bs
    V.zipWithM_ assertCBOREquiv as bs
  (C.Map as, C.Map bs) -> do
    V.length as @?= V.length bs
    V.zipWithM_ (\(k1,v1) (k2,v2) -> assertCBOREquiv k1 k2 >> assertCBOREquiv v1 v2) as bs
  _ -> expected @?= actual

--------------------------------------------------------------------------------
-- XML tests (standard library — always available)
--------------------------------------------------------------------------------

xmlTests :: [TestTree]
xmlTests =
  [ testCase "roundtrip simple element" $ do
      let doc = X.Document Nothing
            (X.Element (X.simpleName "root") V.empty
              (V.singleton (X.Text "hello")))
      let encoded = XE.encode doc
      result <- runPythonBinary "test-interop/test_json_xml.py" [] encoded
      case result of
        Right pythonOutput -> case XD.decode pythonOutput of
          Right decoded ->
            extractRootText (X.docRoot decoded) @?= Just "hello"
          Left err -> assertFailure $ "wireform XML decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip element with attributes" $ do
      let attrs = V.fromList [X.Attribute (X.simpleName "id") "123"]
      let doc = X.Document Nothing
            (X.Element (X.simpleName "item") attrs
              (V.singleton (X.Text "content")))
      let encoded = XE.encode doc
      result <- runPythonBinary "test-interop/test_json_xml.py" [] encoded
      case result of
        Right pythonOutput -> case XD.decode pythonOutput of
          Right decoded -> do
            let root = X.docRoot decoded
            X.elementName root @?= Just (X.simpleName "item")
            hasAttrValue root "id" "123" @? "attribute id=123 expected"
          Left err -> assertFailure $ "wireform XML decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip nested elements" $ do
      let child = X.Element (X.simpleName "child") V.empty
                    (V.singleton (X.Text "inner"))
      let doc = X.Document Nothing
            (X.Element (X.simpleName "parent") V.empty (V.singleton child))
      let encoded = XE.encode doc
      result <- runPythonBinary "test-interop/test_json_xml.py" [] encoded
      case result of
        Right pythonOutput -> case XD.decode pythonOutput of
          Right decoded -> do
            let root = X.docRoot decoded
            X.elementName root @?= Just (X.simpleName "parent")
            let children = X.elementChildren root
            V.length children > 0 @? "expected at least one child"
          Left err -> assertFailure $ "wireform XML decode failed: " ++ err
        Left err -> assertFailure err
  ]

extractRootText :: X.Node -> Maybe T.Text
extractRootText (X.Element _ _ cs) = case V.find isText cs of
  Just (X.Text t) -> Just (T.strip t)
  _ -> Nothing
  where isText (X.Text _) = True
        isText _           = False
extractRootText _ = Nothing

hasAttrValue :: X.Node -> T.Text -> T.Text -> Bool
hasAttrValue (X.Element _ attrs _) name val =
  V.any (\(X.Attribute n v) -> X.nameLocal n == name && v == val) attrs
hasAttrValue _ _ _ = False

--------------------------------------------------------------------------------
-- BSON tests
--------------------------------------------------------------------------------

bsonTests :: [TestTree]
bsonTests =
  [ testCase "roundtrip document {name: Alice, age: 30}" $ withPythonLib "bson" $ do
      let val = B.Document $ V.fromList
            [ ("name", B.String "Alice")
            , ("age", B.Int32 30)
            ]
      let encoded = BE.encode val
      result <- runPythonBinary "test-interop/test_bson.py" [] encoded
      case result of
        Right pythonEncoded -> case BD.decode pythonEncoded of
          Right decoded -> assertBSONEquiv val decoded
          Left err -> assertFailure $ "wireform BSON decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip nested document" $ withPythonLib "bson" $ do
      let val = B.Document $ V.fromList
            [ ("outer", B.Document $ V.fromList
                [ ("inner", B.Int32 42) ])
            ]
      let encoded = BE.encode val
      result <- runPythonBinary "test-interop/test_bson.py" [] encoded
      case result of
        Right pythonEncoded -> case BD.decode pythonEncoded of
          Right decoded -> assertBSONEquiv val decoded
          Left err -> assertFailure $ "wireform BSON decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip with boolean and null" $ withPythonLib "bson" $ do
      let val = B.Document $ V.fromList
            [ ("flag", B.Bool True)
            , ("nothing", B.Null)
            ]
      let encoded = BE.encode val
      result <- runPythonBinary "test-interop/test_bson.py" [] encoded
      case result of
        Right pythonEncoded -> case BD.decode pythonEncoded of
          Right decoded -> assertBSONEquiv val decoded
          Left err -> assertFailure $ "wireform BSON decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip with double" $ withPythonLib "bson" $ do
      let val = B.Document $ V.fromList [("pi", B.Double 3.14159)]
      let encoded = BE.encode val
      result <- runPythonBinary "test-interop/test_bson.py" [] encoded
      case result of
        Right pythonEncoded -> case BD.decode pythonEncoded of
          Right decoded -> assertBSONEquiv val decoded
          Left err -> assertFailure $ "wireform BSON decode failed: " ++ err
        Left err -> assertFailure err
  ]

assertBSONEquiv :: B.Value -> B.Value -> Assertion
assertBSONEquiv expected actual = case (expected, actual) of
  (B.Document as, B.Document bs) -> do
    V.length as @?= V.length bs
    V.zipWithM_ (\(k1,v1) (k2,v2) -> do k1 @?= k2; assertBSONEquiv v1 v2) as bs
  (B.Array as, B.Array bs) -> do
    V.length as @?= V.length bs
    V.zipWithM_ assertBSONEquiv as bs
  (B.Double a, B.Double b) -> abs (a - b) < 1e-10 @? "double mismatch"
  (B.Int32 a, B.Int64 b) -> fromIntegral a @?= b
  (B.Int64 a, B.Int32 b) -> a @?= fromIntegral b
  _ -> expected @?= actual

--------------------------------------------------------------------------------
-- Ion tests
--------------------------------------------------------------------------------

ionTests :: [TestTree]
ionTests =
  [ testCase "roundtrip struct {name: Alice, age: 30}" $ withPythonLib "amazon.ion" $ do
      let val = I.Struct $ V.fromList
            [ ("name", I.String "Alice")
            , ("age", I.Int 30)
            ]
      let encoded = IE.encode val
      result <- runPythonBinary "test-interop/test_ion.py" [] encoded
      case result of
        Right pythonEncoded -> case ID.decode pythonEncoded of
          Right decoded -> assertIonEquiv val decoded
          Left err -> assertFailure $ "wireform Ion decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip string" $ withPythonLib "amazon.ion" $ do
      let val = I.String "Hello Ion!"
      let encoded = IE.encode val
      result <- runPythonBinary "test-interop/test_ion.py" [] encoded
      case result of
        Right pythonEncoded -> case ID.decode pythonEncoded of
          Right decoded -> assertIonEquiv val decoded
          Left err -> assertFailure $ "wireform Ion decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip list" $ withPythonLib "amazon.ion" $ do
      let val = I.List $ V.fromList [I.Int 1, I.Int 2, I.Int 3]
      let encoded = IE.encode val
      result <- runPythonBinary "test-interop/test_ion.py" [] encoded
      case result of
        Right pythonEncoded -> case ID.decode pythonEncoded of
          Right decoded -> assertIonEquiv val decoded
          Left err -> assertFailure $ "wireform Ion decode failed: " ++ err
        Left err -> assertFailure err

  , testCase "roundtrip bool" $ withPythonLib "amazon.ion" $ do
      let val = I.Bool True
      let encoded = IE.encode val
      result <- runPythonBinary "test-interop/test_ion.py" [] encoded
      case result of
        Right pythonEncoded -> case ID.decode pythonEncoded of
          Right decoded -> assertIonEquiv val decoded
          Left err -> assertFailure $ "wireform Ion decode failed: " ++ err
        Left err -> assertFailure err
  ]

assertIonEquiv :: I.Value -> I.Value -> Assertion
assertIonEquiv expected actual = case (expected, actual) of
  (I.Struct as, I.Struct bs) -> do
    V.length as @?= V.length bs
    V.zipWithM_ (\(k1,v1) (k2,v2) -> do k1 @?= k2; assertIonEquiv v1 v2) as bs
  (I.List as, I.List bs) -> do
    V.length as @?= V.length bs
    V.zipWithM_ assertIonEquiv as bs
  (I.Float a, I.Float b) -> abs (a - b) < 1e-10 @? "float mismatch"
  _ -> expected @?= actual

--------------------------------------------------------------------------------
-- Avro tests
--------------------------------------------------------------------------------

avroTests :: [TestTree]
avroTests =
  [ testCase "roundtrip record via Python avro" $ withPythonLib "avro" $ do
      let schema = AS.AvroRecord
            { AS.avroRecordName = "Person"
            , AS.avroRecordNamespace = Nothing
            , AS.avroRecordDoc = Nothing
            , AS.avroRecordAliases = V.empty
            , AS.avroRecordFields = V.fromList
                [ AS.AvroField "name" (AS.AvroPrimitive AS.AvroString) Nothing Nothing V.empty Nothing Map.empty
                , AS.AvroField "age" (AS.AvroPrimitive AS.AvroInt) Nothing Nothing V.empty Nothing Map.empty
                ]
            , AS.avroRecordProps = Map.empty
            }
      let val = AV.Record $ V.fromList [AV.String "Alice", AV.Int 30]
      let encoded = AE.encodeAvro schema val
      let schemaJson = "{\"type\":\"record\",\"name\":\"Person\",\"fields\":[{\"name\":\"name\",\"type\":\"string\"},{\"name\":\"age\",\"type\":\"int\"}]}"
      result <- runPythonBinary "test-interop/test_avro.py" [schemaJson] encoded
      case result of
        Right pythonJson -> do
          let jsonStr = T.unpack (T.strip (decodeUtf8Lenient pythonJson))
          assertBool ("Python parsed Avro and produced JSON: " ++ jsonStr)
            (not (null jsonStr))
          assertBool "JSON contains Alice"
            ("Alice" `T.isInfixOf` decodeUtf8Lenient pythonJson)
        Left err -> assertFailure err
  ]

decodeUtf8Lenient :: BS.ByteString -> T.Text
decodeUtf8Lenient = T.pack . map (toEnum . fromEnum) . BS.unpack

--------------------------------------------------------------------------------
-- Thrift tests
--------------------------------------------------------------------------------

thriftTests :: [TestTree]
thriftTests =
  [ testCase "binary protocol struct parseable by Python" $ withPythonLib "thrift" $ do
      let val = TV.Struct $ V.fromList
            [ (1, TV.String "Alice")
            , (2, TV.I32 30)
            ]
      let encoded = TE.encodeBinary val
      result <- runPythonBinary "test-interop/test_thrift.py" [] encoded
      case result of
        Right output ->
          assertBool "Python parsed the Thrift struct"
            (BS.length output > 0)
        Left err -> assertFailure err
  ]
