module Test.Avro (avroTests) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Avro.Schema
import Avro.Value
import Avro.Encode (encodeAvro)
import Avro.Decode (decodeAvro)
import Avro.JSON (avroToJSON, avroFromJSON, avroSchemaToJSON, avroSchemaFromJSON)
import Avro.Protocol
import Avro.Resolution (ResolvedSchema(..), FieldResolution(..), resolveSchema, resolveValue)

avroTests :: TestTree
avroTests = testGroup "Avro Encode/Decode"
  [ protocolTests
  , testGroup "Primitive roundtrips (property)"
      [ testProperty "null roundtrip" $ property $ do
          let ty = AvroPrimitive AvroNull
          roundtrip ty AvNull

      , testProperty "bool roundtrip" $ property $ do
          b <- forAll Gen.bool
          roundtrip (AvroPrimitive AvroBool) (AvBool b)

      , testProperty "int roundtrip" $ property $ do
          n <- forAll $ Gen.int32 Range.linearBounded
          roundtrip (AvroPrimitive AvroInt) (AvInt n)

      , testProperty "long roundtrip" $ property $ do
          n <- forAll $ Gen.int64 Range.linearBounded
          roundtrip (AvroPrimitive AvroLong) (AvLong n)

      , testProperty "float roundtrip" $ property $ do
          f <- forAll $ Gen.float (Range.linearFrac (-1e6) 1e6)
          roundtrip (AvroPrimitive AvroFloat) (AvFloat f)

      , testProperty "double roundtrip" $ property $ do
          d <- forAll $ Gen.double (Range.linearFrac (-1e12) 1e12)
          roundtrip (AvroPrimitive AvroDouble) (AvDouble d)

      , testProperty "bytes roundtrip" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 256)
          roundtrip (AvroPrimitive AvroBytes) (AvBytes bs)

      , testProperty "string roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 128) Gen.unicode
          roundtrip (AvroPrimitive AvroString) (AvString t)
      ]

  , testGroup "Edge cases (unit)"
      [ testCase "null encodes to empty" $ do
          let bs = encodeAvro (AvroPrimitive AvroNull) AvNull
          bs @?= BS.empty

      , testCase "empty string" $ do
          let ty = AvroPrimitive AvroString
              val = AvString ""
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "empty bytes" $ do
          let ty = AvroPrimitive AvroBytes
              val = AvBytes ""
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "empty array" $ do
          let ty = AvroArray (AvroPrimitive AvroInt)
              val = AvArray []
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "empty map" $ do
          let ty = AvroMap (AvroPrimitive AvroString)
              val = AvMap []
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "union index 0 (null)" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
              val = AvUnion 0 AvNull
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "union index 1 (string)" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
              val = AvUnion 1 (AvString "hello")
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "bool true" $ do
          let ty = AvroPrimitive AvroBool
          decodeAvro ty (encodeAvro ty (AvBool True)) @?= Right (AvBool True)

      , testCase "bool false" $ do
          let ty = AvroPrimitive AvroBool
          decodeAvro ty (encodeAvro ty (AvBool False)) @?= Right (AvBool False)

      , testCase "int 0" $ do
          let ty = AvroPrimitive AvroInt
          decodeAvro ty (encodeAvro ty (AvInt 0)) @?= Right (AvInt 0)

      , testCase "int min" $ do
          let ty = AvroPrimitive AvroInt
          decodeAvro ty (encodeAvro ty (AvInt minBound)) @?= Right (AvInt minBound)

      , testCase "int max" $ do
          let ty = AvroPrimitive AvroInt
          decodeAvro ty (encodeAvro ty (AvInt maxBound)) @?= Right (AvInt maxBound)

      , testCase "long min" $ do
          let ty = AvroPrimitive AvroLong
          decodeAvro ty (encodeAvro ty (AvLong minBound)) @?= Right (AvLong minBound)

      , testCase "long max" $ do
          let ty = AvroPrimitive AvroLong
          decodeAvro ty (encodeAvro ty (AvLong maxBound)) @?= Right (AvLong maxBound)
      ]

  , testGroup "Record roundtrip"
      [ testProperty "mixed-field record" $ property $ do
          n <- forAll $ Gen.int32 Range.linearBounded
          t <- forAll $ Gen.text (Range.linear 0 64) Gen.unicode
          b <- forAll Gen.bool
          d <- forAll $ Gen.double (Range.linearFrac (-1e6) 1e6)
          let ty = mkRecordType "TestRecord"
                     [ ("intField",    AvroPrimitive AvroInt)
                     , ("stringField", AvroPrimitive AvroString)
                     , ("boolField",   AvroPrimitive AvroBool)
                     , ("doubleField", AvroPrimitive AvroDouble)
                     ]
              val = AvRecord [AvInt n, AvString t, AvBool b, AvDouble d]
          roundtrip ty val
      ]

  , testGroup "Array roundtrip"
      [ testProperty "array of ints" $ property $ do
          ns <- forAll $ Gen.list (Range.linear 0 50) (Gen.int32 Range.linearBounded)
          let ty = AvroArray (AvroPrimitive AvroInt)
              val = AvArray (map AvInt ns)
          roundtrip ty val

      , testProperty "array of records" $ property $ do
          items <- forAll $ Gen.list (Range.linear 0 20) $ do
            i <- Gen.int32 Range.linearBounded
            s <- Gen.text (Range.linear 0 32) Gen.unicode
            pure (i, s)
          let recTy = mkRecordType "Item"
                        [ ("id",   AvroPrimitive AvroInt)
                        , ("name", AvroPrimitive AvroString)
                        ]
              ty = AvroArray recTy
              val = AvArray [AvRecord [AvInt i, AvString s] | (i, s) <- items]
          roundtrip ty val
      ]

  , testGroup "Map roundtrip"
      [ testProperty "map of longs" $ property $ do
          entries <- forAll $ Gen.list (Range.linear 0 30) $ do
            k <- Gen.text (Range.linear 1 32) Gen.alphaNum
            v <- Gen.int64 Range.linearBounded
            pure (k, v)
          let ty = AvroMap (AvroPrimitive AvroLong)
              val = AvMap [(k, AvLong v) | (k, v) <- entries]
          roundtrip ty val
      ]

  , testGroup "Union roundtrip"
      [ testProperty "null|string union" $ property $ do
          useNull <- forAll Gen.bool
          let branches = V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString]
              ty = AvroUnion branches
          val <- if useNull
                 then pure (AvUnion 0 AvNull)
                 else do
                   t <- forAll $ Gen.text (Range.linear 0 64) Gen.unicode
                   pure (AvUnion 1 (AvString t))
          roundtrip ty val

      , testProperty "int|long|string union" $ property $ do
          branch <- forAll $ Gen.int (Range.linear 0 2)
          let branches = V.fromList
                [ AvroPrimitive AvroInt
                , AvroPrimitive AvroLong
                , AvroPrimitive AvroString
                ]
              ty = AvroUnion branches
          val <- case branch of
            0 -> AvUnion 0 . AvInt <$> forAll (Gen.int32 Range.linearBounded)
            1 -> AvUnion 1 . AvLong <$> forAll (Gen.int64 Range.linearBounded)
            _ -> AvUnion 2 . AvString <$> forAll (Gen.text (Range.linear 0 32) Gen.unicode)
          roundtrip ty val
      ]

  , testGroup "Fixed roundtrip"
      [ testProperty "fixed bytes" $ property $ do
          sz <- forAll $ Gen.int (Range.linear 0 64)
          bs <- forAll $ Gen.bytes (Range.singleton sz)
          let ty = AvroFixed "TestFixed" Nothing sz V.empty
              val = AvFixed bs
          roundtrip ty val

      , testCase "fixed empty" $ do
          let ty = AvroFixed "Empty" Nothing 0 V.empty
              val = AvFixed ""
          decodeAvro ty (encodeAvro ty val) @?= Right val
      ]

  , testGroup "Enum roundtrip"
      [ testProperty "enum ordinal" $ property $ do
          idx <- forAll $ Gen.int (Range.linear 0 9)
          let syms = V.fromList (map (T.pack . ("S" ++) . show) [0..9 :: Int])
              ty = AvroEnum "TestEnum" Nothing Nothing V.empty syms Nothing
              val = AvEnum idx
          roundtrip ty val
      ]

  , testGroup "JSON roundtrip — primitives"
      [ testCase "null JSON roundtrip" $ do
          let ty = AvroPrimitive AvroNull
              val = AvNull
          avroFromJSON ty (avroToJSON ty val) @?= Right val

      , testCase "bool JSON roundtrip" $ do
          let ty = AvroPrimitive AvroBool
          avroFromJSON ty (avroToJSON ty (AvBool True)) @?= Right (AvBool True)
          avroFromJSON ty (avroToJSON ty (AvBool False)) @?= Right (AvBool False)

      , testCase "int JSON roundtrip" $ do
          let ty = AvroPrimitive AvroInt
              val = AvInt 42
          avroFromJSON ty (avroToJSON ty val) @?= Right val

      , testCase "long JSON roundtrip" $ do
          let ty = AvroPrimitive AvroLong
              val = AvLong 123456789
          avroFromJSON ty (avroToJSON ty val) @?= Right val

      , testCase "float JSON roundtrip" $ do
          let ty = AvroPrimitive AvroFloat
              val = AvFloat 3.14
              Right (AvFloat result) = avroFromJSON ty (avroToJSON ty val)
          abs (result - 3.14) < 0.001 @?= True

      , testCase "double JSON roundtrip" $ do
          let ty = AvroPrimitive AvroDouble
              val = AvDouble 2.71828
          avroFromJSON ty (avroToJSON ty val) @?= Right val

      , testCase "bytes JSON roundtrip" $ do
          let ty = AvroPrimitive AvroBytes
              val = AvBytes (BS.pack [0, 1, 127, 255])
          avroFromJSON ty (avroToJSON ty val) @?= Right val

      , testCase "string JSON roundtrip" $ do
          let ty = AvroPrimitive AvroString
              val = AvString "hello world"
          avroFromJSON ty (avroToJSON ty val) @?= Right val

      , testCase "float NaN JSON" $ do
          let ty = AvroPrimitive AvroFloat
          avroToJSON ty (AvFloat (0/0)) @?= Aeson.String "NaN"

      , testCase "double Infinity JSON" $ do
          let ty = AvroPrimitive AvroDouble
          avroToJSON ty (AvDouble (1/0)) @?= Aeson.String "Infinity"
          avroToJSON ty (AvDouble (negate (1/0))) @?= Aeson.String "-Infinity"
      ]

  , testGroup "JSON record encode/decode"
      [ testCase "simple record" $ do
          let ty = mkRecordType "Person"
                     [ ("name", AvroPrimitive AvroString)
                     , ("age",  AvroPrimitive AvroInt)
                     ]
              val = AvRecord [AvString "Alice", AvInt 30]
              json = avroToJSON ty val
          avroFromJSON ty json @?= Right val

      , testCase "record JSON structure" $ do
          let ty = mkRecordType "Pair"
                     [ ("first",  AvroPrimitive AvroInt)
                     , ("second", AvroPrimitive AvroString)
                     ]
              val = AvRecord [AvInt 1, AvString "x"]
              json = avroToJSON ty val
          case json of
            Aeson.Object _ -> pure ()
            _              -> assertFailure "expected JSON object"
          avroFromJSON ty json @?= Right val
      ]

  , testGroup "JSON union encode/decode"
      [ testCase "null branch in union" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
              val = AvUnion 0 AvNull
          avroToJSON ty val @?= Aeson.Null
          avroFromJSON ty Aeson.Null @?= Right val

      , testCase "string branch in union" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
              val = AvUnion 1 (AvString "hello")
              json = avroToJSON ty val
          avroFromJSON ty json @?= Right val
      ]

  , testGroup "Schema to/from JSON roundtrip"
      [ testCase "primitive schemas" $ do
          let prims = [ AvroPrimitive AvroNull, AvroPrimitive AvroBool
                       , AvroPrimitive AvroInt, AvroPrimitive AvroLong
                       , AvroPrimitive AvroFloat, AvroPrimitive AvroDouble
                       , AvroPrimitive AvroBytes, AvroPrimitive AvroString ]
          mapM_ (\ty -> avroSchemaFromJSON (avroSchemaToJSON ty) @?= Right ty) prims

      , testCase "record schema roundtrip" $ do
          let ty = mkRecordType "MyRec"
                     [ ("x", AvroPrimitive AvroInt)
                     , ("y", AvroPrimitive AvroString)
                     ]
              Right parsed = avroSchemaFromJSON (avroSchemaToJSON ty)
          avroRecordName parsed @?= "MyRec"
          V.length (avroRecordFields parsed) @?= 2

      , testCase "enum schema roundtrip" $ do
          let ty = AvroEnum "Color" Nothing Nothing V.empty
                     (V.fromList ["RED","GREEN","BLUE"]) Nothing
              Right parsed = avroSchemaFromJSON (avroSchemaToJSON ty)
          avroEnumName parsed @?= "Color"
          avroEnumSymbols parsed @?= V.fromList ["RED","GREEN","BLUE"]

      , testCase "array schema roundtrip" $ do
          let ty = AvroArray (AvroPrimitive AvroInt)
          avroSchemaFromJSON (avroSchemaToJSON ty) @?= Right ty

      , testCase "map schema roundtrip" $ do
          let ty = AvroMap (AvroPrimitive AvroString)
          avroSchemaFromJSON (avroSchemaToJSON ty) @?= Right ty

      , testCase "union schema roundtrip" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroInt])
          avroSchemaFromJSON (avroSchemaToJSON ty) @?= Right ty

      , testCase "fixed schema roundtrip" $ do
          let ty = AvroFixed "Hash" Nothing 16 V.empty
              Right parsed = avroSchemaFromJSON (avroSchemaToJSON ty)
          avroFixedName parsed @?= "Hash"
          avroFixedSize parsed @?= 16
      ]

  , testGroup "Resolution — compatible schemas"
      [ testCase "add field with default" $ do
          let writerTy = mkRecordType "Rec"
                           [ ("a", AvroPrimitive AvroInt) ]
              readerTy = mkRecordTypeWithDefaults "Rec"
                           [ ("a", AvroPrimitive AvroInt, Nothing)
                           , ("b", AvroPrimitive AvroString, Just AvroString)
                           ]
              Right res = resolveSchema writerTy readerTy
              writerVal = AvRecord [AvInt 42]
          resolveValue res writerVal @?= Right (AvRecord [AvInt 42, AvString ""])

      , testCase "remove field" $ do
          let writerTy = mkRecordType "Rec"
                           [ ("a", AvroPrimitive AvroInt)
                           , ("b", AvroPrimitive AvroString)
                           ]
              readerTy = mkRecordType "Rec"
                           [ ("a", AvroPrimitive AvroInt) ]
              Right res = resolveSchema writerTy readerTy
              writerVal = AvRecord [AvInt 99, AvString "dropped"]
          resolveValue res writerVal @?= Right (AvRecord [AvInt 99])

      , testCase "promote int -> long" $ do
          let writerTy = AvroPrimitive AvroInt
              readerTy = AvroPrimitive AvroLong
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AvInt 42) @?= Right (AvLong 42)

      , testCase "promote int -> float" $ do
          let writerTy = AvroPrimitive AvroInt
              readerTy = AvroPrimitive AvroFloat
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AvInt 7) @?= Right (AvFloat 7.0)

      , testCase "promote int -> double" $ do
          let writerTy = AvroPrimitive AvroInt
              readerTy = AvroPrimitive AvroDouble
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AvInt 7) @?= Right (AvDouble 7.0)

      , testCase "promote long -> float" $ do
          let writerTy = AvroPrimitive AvroLong
              readerTy = AvroPrimitive AvroFloat
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AvLong 100) @?= Right (AvFloat 100.0)

      , testCase "promote long -> double" $ do
          let writerTy = AvroPrimitive AvroLong
              readerTy = AvroPrimitive AvroDouble
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AvLong 100) @?= Right (AvDouble 100.0)

      , testCase "promote float -> double" $ do
          let writerTy = AvroPrimitive AvroFloat
              readerTy = AvroPrimitive AvroDouble
              Right res = resolveSchema writerTy readerTy
              Right (AvDouble d) = resolveValue res (AvFloat 1.5)
          abs (d - 1.5) < 0.001 @?= True

      , testCase "same schema resolves trivially" $ do
          let ty = AvroPrimitive AvroInt
          resolveSchema ty ty @?= Right ResolvedSame

      , testCase "array resolution" $ do
          let writerTy = AvroArray (AvroPrimitive AvroInt)
              readerTy = AvroArray (AvroPrimitive AvroLong)
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AvArray [AvInt 1, AvInt 2])
            @?= Right (AvArray [AvLong 1, AvLong 2])

      , testCase "map resolution" $ do
          let writerTy = AvroMap (AvroPrimitive AvroInt)
              readerTy = AvroMap (AvroPrimitive AvroLong)
              Right res = resolveSchema writerTy readerTy
          resolveValue res (AvMap [("k", AvInt 5)])
            @?= Right (AvMap [("k", AvLong 5)])
      ]

  , testGroup "Resolution — incompatible schemas"
      [ testCase "type mismatch int vs string" $ do
          let res = resolveSchema (AvroPrimitive AvroInt) (AvroPrimitive AvroString)
          case res of
            Left _ -> pure ()
            Right _ -> assertFailure "expected incompatibility error"

      , testCase "missing required field" $ do
          let writerTy = mkRecordType "Rec"
                           [ ("a", AvroPrimitive AvroInt) ]
              readerTy = mkRecordType "Rec"
                           [ ("a", AvroPrimitive AvroInt)
                           , ("b", AvroPrimitive AvroString)
                           ]
          case resolveSchema writerTy readerTy of
            Left _ -> pure ()
            Right _ -> assertFailure "expected missing field error"

      , testCase "fixed size mismatch" $ do
          let w = AvroFixed "Hash" Nothing 16 V.empty
              r = AvroFixed "Hash" Nothing 32 V.empty
          case resolveSchema w r of
            Left _ -> pure ()
            Right _ -> assertFailure "expected fixed size mismatch error"

      , testCase "enum symbol mismatch" $ do
          let w = AvroEnum "E" Nothing Nothing V.empty (V.fromList ["A","B","C"]) Nothing
              r = AvroEnum "E" Nothing Nothing V.empty (V.fromList ["A","B"]) Nothing
          case resolveSchema w r of
            Left _ -> pure ()
            Right _ -> assertFailure "expected enum symbol mismatch"

      , testCase "string vs int incompatible" $ do
          let res = resolveSchema (AvroPrimitive AvroString) (AvroPrimitive AvroInt)
          case res of
            Left _ -> pure ()
            Right _ -> assertFailure "expected incompatibility error"
      ]
  ]

--------------------------------------------------------------------------------
-- Avro Protocol tests
--------------------------------------------------------------------------------

protocolTests :: TestTree
protocolTests = testGroup "Avro Protocol"
  [ protocolJsonRoundtrip
  , protocolFingerprintTest
  , handshakeTests
  , minimalProtocol
  ]

protocolJsonRoundtrip :: TestTree
protocolJsonRoundtrip = testCase "Protocol JSON roundtrip" $ do
  let proto = AvroProtocol
        { protoName      = "HelloService"
        , protoNamespace = Just "com.example"
        , protoDoc       = Just "A simple service"
        , protoTypes     =
            [ mkRecordType "Greeting"
                [ ("message", AvroPrimitive AvroString) ]
            , AvroEnum
                { avroEnumName      = "Tone"
                , avroEnumNamespace = Nothing
                , avroEnumDoc       = Nothing
                , avroEnumAliases   = V.empty
                , avroEnumSymbols   = V.fromList ["FRIENDLY", "FORMAL"]
                , avroEnumDefault   = Nothing
                }
            ]
        , protoMessages  =
            [ ("hello", AvroMessage
                { msgRequest  =
                    [ AvroParam "name" (AvroPrimitive AvroString)
                    , AvroParam "tone" (AvroPrimitive (AvroSchemaRef "Tone"))
                    ]
                , msgResponse = AvroPrimitive (AvroSchemaRef "Greeting")
                , msgErrors   = Nothing
                , msgOneWay   = False
                })
            , ("ping", AvroMessage
                { msgRequest  = []
                , msgResponse = AvroPrimitive AvroNull
                , msgErrors   = Nothing
                , msgOneWay   = True
                })
            ]
        }
      json = protocolToJSON proto
  case protocolFromJSON json of
    Left err -> assertFailure $ "Failed to parse protocol JSON: " ++ err
    Right parsed -> do
      protoName parsed @?= "HelloService"
      protoNamespace parsed @?= Just "com.example"
      protoDoc parsed @?= Just "A simple service"
      length (protoTypes parsed) @?= 2
      length (protoMessages parsed) @?= 2

protocolFingerprintTest :: TestTree
protocolFingerprintTest = testCase "Protocol fingerprint is 16 bytes (MD5)" $ do
  let proto = AvroProtocol
        { protoName      = "TestService"
        , protoNamespace = Nothing
        , protoDoc       = Nothing
        , protoTypes     = []
        , protoMessages  =
            [ ("echo", AvroMessage
                { msgRequest  = [AvroParam "msg" (AvroPrimitive AvroString)]
                , msgResponse = AvroPrimitive AvroString
                , msgErrors   = Nothing
                , msgOneWay   = False
                })
            ]
        }
      fp = avroProtocolFingerprint proto
  BS.length fp @?= 16
  let fp2 = avroProtocolFingerprint proto
  fp @?= fp2

handshakeTests :: TestTree
handshakeTests = testGroup "Handshake types"
  [ testCase "HandshakeRequest roundtrip" $ do
      let req = HandshakeRequest
            { hsReqClientHash     = BS.replicate 16 0xAA
            , hsReqClientProtocol = Just "{\"protocol\":\"Test\"}"
            , hsReqServerHash     = BS.replicate 16 0xBB
            , hsReqMeta           = Nothing
            }
          json = handshakeRequestToJSON req
      case handshakeRequestFromJSON json of
        Left err -> assertFailure $ "Failed to parse handshake request: " ++ err
        Right parsed -> do
          hsReqClientHash parsed @?= hsReqClientHash req
          hsReqClientProtocol parsed @?= hsReqClientProtocol req
          hsReqServerHash parsed @?= hsReqServerHash req

  , testCase "HandshakeResponse roundtrip" $ do
      let resp = HandshakeResponse
            { hsRespMatch          = MatchBoth
            , hsRespServerProtocol = Nothing
            , hsRespServerHash     = Nothing
            , hsRespMeta           = Nothing
            }
          json = handshakeResponseToJSON resp
      case handshakeResponseFromJSON json of
        Left err -> assertFailure $ "Failed to parse handshake response: " ++ err
        Right parsed -> do
          hsRespMatch parsed @?= MatchBoth
          hsRespServerProtocol parsed @?= Nothing

  , testCase "HandshakeResponse CLIENT match" $ do
      let resp = HandshakeResponse
            { hsRespMatch          = MatchClient
            , hsRespServerProtocol = Just "{\"protocol\":\"Test\"}"
            , hsRespServerHash     = Just (BS.replicate 16 0xCC)
            , hsRespMeta           = Nothing
            }
          json = handshakeResponseToJSON resp
      case handshakeResponseFromJSON json of
        Left err -> assertFailure $ "Failed to parse handshake response: " ++ err
        Right parsed -> do
          hsRespMatch parsed @?= MatchClient
          hsRespServerProtocol parsed @?= Just "{\"protocol\":\"Test\"}"

  , testCase "HandshakeResponse NONE match" $ do
      let resp = HandshakeResponse
            { hsRespMatch          = MatchNone
            , hsRespServerProtocol = Just "{}"
            , hsRespServerHash     = Just (BS.replicate 16 0x00)
            , hsRespMeta           = Just [("key", BS.pack [1,2,3])]
            }
          json = handshakeResponseToJSON resp
      case handshakeResponseFromJSON json of
        Left err -> assertFailure $ "Failed to parse handshake response: " ++ err
        Right parsed ->
          hsRespMatch parsed @?= MatchNone
  ]

minimalProtocol :: TestTree
minimalProtocol = testCase "Minimal protocol (no types, no messages)" $ do
  let proto = AvroProtocol
        { protoName      = "Empty"
        , protoNamespace = Nothing
        , protoDoc       = Nothing
        , protoTypes     = []
        , protoMessages  = []
        }
      json = protocolToJSON proto
  case protocolFromJSON json of
    Left err -> assertFailure $ "Failed to parse minimal protocol: " ++ err
    Right parsed -> do
      protoName parsed @?= "Empty"
      null (protoTypes parsed) @?= True
      null (protoMessages parsed) @?= True
  let fp1 = avroProtocolFingerprint proto
      fp2 = avroProtocolFingerprint proto
  fp1 @?= fp2

-- Helpers

roundtrip :: (MonadTest m) => AvroType -> AvroValue -> m ()
roundtrip ty val =
  decodeAvro ty (encodeAvro ty val) === Right val

mkRecordType :: Text -> [(Text, AvroType)] -> AvroType
mkRecordType name fields = AvroRecord
  { avroRecordName      = name
  , avroRecordNamespace = Nothing
  , avroRecordDoc       = Nothing
  , avroRecordAliases   = V.empty
  , avroRecordFields    = V.fromList
      [ AvroField
          { avroFieldName    = fname
          , avroFieldType    = ftype
          , avroFieldDefault = Nothing
          , avroFieldOrder   = Nothing
          , avroFieldAliases = V.empty
          , avroFieldDoc     = Nothing
          }
      | (fname, ftype) <- fields
      ]
  }

mkRecordTypeWithDefaults :: Text -> [(Text, AvroType, Maybe AvroSchema)] -> AvroType
mkRecordTypeWithDefaults name fields = AvroRecord
  { avroRecordName      = name
  , avroRecordNamespace = Nothing
  , avroRecordDoc       = Nothing
  , avroRecordAliases   = V.empty
  , avroRecordFields    = V.fromList
      [ AvroField
          { avroFieldName    = fname
          , avroFieldType    = ftype
          , avroFieldDefault = dflt
          , avroFieldOrder   = Nothing
          , avroFieldAliases = V.empty
          , avroFieldDoc     = Nothing
          }
      | (fname, ftype, dflt) <- fields
      ]
  }
