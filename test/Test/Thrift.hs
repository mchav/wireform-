module Test.Thrift (thriftTests) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Vector as V

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Thrift.Encode (encodeBinary, encodeCompact)
import Thrift.Decode (decodeBinary, decodeCompact)
import Thrift.JSON (thriftToJSON, thriftFromJSON, thriftToTypedJSON, thriftFromTypedJSON)
import Thrift.Message
import Thrift.Transport (frameMessage, unframeMessage, unframeMessages)
import qualified Thrift.Value as TV
import Thrift.Wire (ThriftType (..))

thriftTests :: TestTree
thriftTests = testGroup "Thrift Encode/Decode"
  [ propertyBinaryRoundtrip
  , propertyCompactRoundtrip
  , unitMixedStruct
  , unitNestedStructs
  , unitContainers
  , unitEmptyStructAndContainers
  , unitProtocolsDiffer
  , jsonTests
  , messageTests
  , transportTests
  ]

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genText :: Gen Text
genText = Gen.text (Range.linear 0 32) Gen.unicode

wrapStruct :: TV.Value -> TV.Value
wrapStruct v = TV.Struct (V.fromList [(1, v)])

--------------------------------------------------------------------------------
-- Property: Binary roundtrip for each primitive
--------------------------------------------------------------------------------

propertyBinaryRoundtrip :: TestTree
propertyBinaryRoundtrip = testGroup "Binary roundtrip (property)"
  [ testProperty "Bool" $ property $ do
      b <- forAll Gen.bool
      let v = wrapStruct (TV.Bool b)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "Byte" $ property $ do
      x <- forAll $ Gen.int8 Range.linearBounded
      let v = wrapStruct (TV.Byte x)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "I16" $ property $ do
      x <- forAll $ Gen.int16 Range.linearBounded
      let v = wrapStruct (TV.I16 x)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "I32" $ property $ do
      x <- forAll $ Gen.int32 Range.linearBounded
      let v = wrapStruct (TV.I32 x)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "I64" $ property $ do
      x <- forAll $ Gen.int64 Range.linearBounded
      let v = wrapStruct (TV.I64 x)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "Double" $ property $ do
      x <- forAll $ Gen.double (Range.linearFrac (-1e15) 1e15)
      let v = wrapStruct (TV.Double x)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "String" $ property $ do
      t <- forAll genText
      let v = wrapStruct (TV.String t)
      decodeBinary (encodeBinary v) === Right v

  , testProperty "Binary" $ property $ do
      b <- forAll $ Gen.bytes (Range.linear 0 128)
      let v = wrapStruct (TV.Binary b)
          encoded = encodeBinary v
          decoded = decodeBinary encoded
      case decoded of
        Right (TV.Struct fields) | V.length fields == 1 ->
          case snd (V.head fields) of
            TV.Binary b' -> b' === b
            TV.String _  -> success
            other -> do
              annotate (show other)
              failure
        other -> do
          annotate (show other)
          failure

  , testProperty "UUID" $ property $ do
      u <- forAll $ Gen.bytes (Range.singleton 16)
      let v = wrapStruct (TV.UUID u)
      decodeBinary (encodeBinary v) === Right v
  ]

--------------------------------------------------------------------------------
-- Property: Compact roundtrip for each primitive
--------------------------------------------------------------------------------

propertyCompactRoundtrip :: TestTree
propertyCompactRoundtrip = testGroup "Compact roundtrip (property)"
  [ testProperty "Bool" $ property $ do
      b <- forAll Gen.bool
      let v = wrapStruct (TV.Bool b)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "Byte" $ property $ do
      x <- forAll $ Gen.int8 Range.linearBounded
      let v = wrapStruct (TV.Byte x)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "I16" $ property $ do
      x <- forAll $ Gen.int16 Range.linearBounded
      let v = wrapStruct (TV.I16 x)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "I32" $ property $ do
      x <- forAll $ Gen.int32 Range.linearBounded
      let v = wrapStruct (TV.I32 x)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "I64" $ property $ do
      x <- forAll $ Gen.int64 Range.linearBounded
      let v = wrapStruct (TV.I64 x)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "Double" $ property $ do
      x <- forAll $ Gen.double (Range.linearFrac (-1e15) 1e15)
      let v = wrapStruct (TV.Double x)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "String" $ property $ do
      t <- forAll genText
      let v = wrapStruct (TV.String t)
      decodeCompact (encodeCompact v) === Right v

  , testProperty "Binary" $ property $ do
      b <- forAll $ Gen.bytes (Range.linear 0 128)
      let v = wrapStruct (TV.Binary b)
          encoded = encodeCompact v
          decoded = decodeCompact encoded
      case decoded of
        Right (TV.Struct fields) | V.length fields == 1 ->
          case snd (V.head fields) of
            TV.Binary b' -> b' === b
            TV.String _  -> success
            other -> do
              annotate (show other)
              failure
        other -> do
          annotate (show other)
          failure

  , testProperty "UUID" $ property $ do
      u <- forAll $ Gen.bytes (Range.singleton 16)
      let v = wrapStruct (TV.UUID u)
      decodeCompact (encodeCompact v) === Right v
  ]

--------------------------------------------------------------------------------
-- Unit: struct with mixed field types
--------------------------------------------------------------------------------

unitMixedStruct :: TestTree
unitMixedStruct = testGroup "Mixed struct roundtrip"
  [ testCase "Binary" $ do
      let v = mixedStruct
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Compact" $ do
      let v = mixedStruct
      decodeCompact (encodeCompact v) @?= Right v
  ]

mixedStruct :: TV.Value
mixedStruct = TV.Struct $ V.fromList
  [ (1,  TV.Bool True)
  , (2,  TV.Byte 42)
  , (3,  TV.I16 1000)
  , (4,  TV.I32 100000)
  , (5,  TV.I64 9999999999)
  , (6,  TV.Double 3.14)
  , (7,  TV.String "hello world")
  , (8,  TV.Binary (BS.pack [0xDE, 0xAD, 0xBE, 0xEF]))
  , (10, TV.UUID (BS.pack [0..15]))
  ]

--------------------------------------------------------------------------------
-- Unit: nested structs
--------------------------------------------------------------------------------

unitNestedStructs :: TestTree
unitNestedStructs = testGroup "Nested structs"
  [ testCase "Binary" $ do
      let v = nestedStruct
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Compact" $ do
      let v = nestedStruct
      decodeCompact (encodeCompact v) @?= Right v
  ]

nestedStruct :: TV.Value
nestedStruct = TV.Struct $ V.fromList
  [ (1, TV.String "outer")
  , (2, TV.Struct $ V.fromList
      [ (1, TV.String "inner")
      , (2, TV.I32 42)
      , (3, TV.Struct $ V.fromList
          [ (1, TV.Bool False)
          , (2, TV.I64 (-999))
          ])
      ])
  , (3, TV.I32 7)
  ]

--------------------------------------------------------------------------------
-- Unit: lists, sets, maps
--------------------------------------------------------------------------------

unitContainers :: TestTree
unitContainers = testGroup "Container roundtrip"
  [ testCase "List of i32 (Binary)" $ do
      let v = TV.Struct (V.fromList [(1, TV.List TT_I32 (V.fromList [TV.I32 1, TV.I32 2, TV.I32 3]))])
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "List of i32 (Compact)" $ do
      let v = TV.Struct (V.fromList [(1, TV.List TT_I32 (V.fromList [TV.I32 1, TV.I32 2, TV.I32 3]))])
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "Set of strings (Binary)" $ do
      let v = TV.Struct (V.fromList [(1, TV.Set TT_STRING (V.fromList [TV.String "a", TV.String "b"]))])
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Set of strings (Compact)" $ do
      let v = TV.Struct (V.fromList [(1, TV.Set TT_STRING (V.fromList [TV.String "a", TV.String "b"]))])
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "Map i32->string (Binary)" $ do
      let v = TV.Struct (V.fromList [(1, TV.Map TT_I32 TT_STRING
                  (V.fromList [ (TV.I32 1, TV.String "one")
                              , (TV.I32 2, TV.String "two")
                              ]))])
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Map i32->string (Compact)" $ do
      let v = TV.Struct (V.fromList [(1, TV.Map TT_I32 TT_STRING
                  (V.fromList [ (TV.I32 1, TV.String "one")
                              , (TV.I32 2, TV.String "two")
                              ]))])
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "List of structs (Binary)" $ do
      let v = TV.Struct
                (V.fromList [(1, TV.List TT_STRUCT
                  (V.fromList [ TV.Struct (V.fromList [(1, TV.I32 10)])
                              , TV.Struct (V.fromList [(1, TV.I32 20)])
                              ]))])
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "List of structs (Compact)" $ do
      let v = TV.Struct
                (V.fromList [(1, TV.List TT_STRUCT
                  (V.fromList [ TV.Struct (V.fromList [(1, TV.I32 10)])
                              , TV.Struct (V.fromList [(1, TV.I32 20)])
                              ]))])
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "Map with struct values (Binary)" $ do
      let v = TV.Struct
                (V.fromList [(1, TV.Map TT_STRING TT_STRUCT
                  (V.fromList [ (TV.String "x", TV.Struct (V.fromList [(1, TV.Bool True)]))
                              ]))])
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Map with struct values (Compact)" $ do
      let v = TV.Struct
                (V.fromList [(1, TV.Map TT_STRING TT_STRUCT
                  (V.fromList [ (TV.String "x", TV.Struct (V.fromList [(1, TV.Bool True)]))
                              ]))])
      decodeCompact (encodeCompact v) @?= Right v
  ]

--------------------------------------------------------------------------------
-- Unit: empty struct and empty containers
--------------------------------------------------------------------------------

unitEmptyStructAndContainers :: TestTree
unitEmptyStructAndContainers = testGroup "Empty struct and containers"
  [ testCase "Empty struct (Binary)" $ do
      let v = TV.Struct V.empty
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Empty struct (Compact)" $ do
      let v = TV.Struct V.empty
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "Empty list (Binary)" $ do
      let v = TV.Struct (V.fromList [(1, TV.List TT_I32 V.empty)])
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Empty list (Compact)" $ do
      let v = TV.Struct (V.fromList [(1, TV.List TT_I32 V.empty)])
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "Empty set (Binary)" $ do
      let v = TV.Struct (V.fromList [(1, TV.Set TT_STRING V.empty)])
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Empty set (Compact)" $ do
      let v = TV.Struct (V.fromList [(1, TV.Set TT_STRING V.empty)])
      decodeCompact (encodeCompact v) @?= Right v

  , testCase "Empty map (Binary)" $ do
      let v = TV.Struct (V.fromList [(1, TV.Map TT_I32 TT_STRING V.empty)])
      decodeBinary (encodeBinary v) @?= Right v

  , testCase "Empty map (Compact)" $ do
      let v = TV.Struct (V.fromList [(1, TV.Map TT_I32 TT_STRING V.empty)])
          decoded = decodeCompact (encodeCompact v)
      case decoded of
        Right (TV.Struct fields) | V.length fields == 1 ->
          case snd (V.head fields) of
            TV.Map _ _ entries | V.null entries -> return ()
            other -> assertFailure $ "Expected empty map, got: " ++ show other
        other -> assertFailure $ "Expected empty map struct, got: " ++ show other
  ]

--------------------------------------------------------------------------------
-- Unit: Binary and Compact produce different bytes but decode to same value
--------------------------------------------------------------------------------

unitProtocolsDiffer :: TestTree
unitProtocolsDiffer = testGroup "Protocols differ in bytes, agree on values"
  [ testCase "Simple struct" $ do
      let v = TV.Struct $ V.fromList
                [ (1, TV.Bool True)
                , (2, TV.I32 42)
                , (3, TV.String "test")
                ]
          binBytes  = encodeBinary v
          compBytes = encodeCompact v
      assertBool "Binary and Compact should produce different bytes"
                 (binBytes /= compBytes)
      decodeBinary binBytes   @?= Right v
      decodeCompact compBytes @?= Right v

  , testCase "Struct with containers" $ do
      let v = TV.Struct $ V.fromList
                [ (1, TV.List TT_I64 (V.fromList [TV.I64 100, TV.I64 200]))
                , (2, TV.Map TT_STRING TT_I32
                     (V.fromList [(TV.String "a", TV.I32 1)]))
                ]
          binBytes  = encodeBinary v
          compBytes = encodeCompact v
      assertBool "Binary and Compact should produce different bytes"
                 (binBytes /= compBytes)
      decodeBinary binBytes   @?= Right v
      decodeCompact compBytes @?= Right v

  , testCase "Nested struct" $ do
      let v = TV.Struct $ V.fromList
                [ (1, TV.Struct (V.fromList [(1, TV.I32 (-1)), (2, TV.Bool False)]))
                , (2, TV.Double 2.718)
                ]
          binBytes  = encodeBinary v
          compBytes = encodeCompact v
      assertBool "Binary and Compact should produce different bytes"
                 (binBytes /= compBytes)
      decodeBinary binBytes   @?= Right v
      decodeCompact compBytes @?= Right v
  ]

--------------------------------------------------------------------------------
-- JSON protocol tests
--------------------------------------------------------------------------------

jsonTests :: TestTree
jsonTests = testGroup "Thrift JSON"
  [ jsonPrimitiveRoundtrip
  , jsonStructEncodeDecode
  , jsonNestedStruct
  , jsonContainers
  , jsonEmptyContainers
  , typedJsonMixedStruct
  ]

jsonPrimitiveRoundtrip :: TestTree
jsonPrimitiveRoundtrip = testGroup "JSON primitive roundtrip"
  [ testCase "Bool" $ do
      let v = TV.Bool True
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Byte" $ do
      let v = TV.Byte 42
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "I16" $ do
      let v = TV.I16 (-1000)
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "I32" $ do
      let v = TV.I32 100000
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "I64" $ do
      let v = TV.I64 9999999999
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Double" $ do
      let v = TV.Double 3.14
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "String" $ do
      let v = TV.String "hello"
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Binary" $ do
      let v = TV.Binary (BS.pack [0xDE, 0xAD])
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "UUID" $ do
      let v = TV.UUID (BS.pack [0..15])
      thriftFromJSON v (thriftToJSON v) @?= Right v
  ]

jsonStructEncodeDecode :: TestTree
jsonStructEncodeDecode = testCase "JSON struct encode/decode" $ do
  let v = TV.Struct $ V.fromList
            [ (1, TV.Bool True)
            , (2, TV.I32 42)
            , (3, TV.String "test")
            ]
  thriftFromJSON v (thriftToJSON v) @?= Right v

jsonNestedStruct :: TestTree
jsonNestedStruct = testCase "JSON nested struct" $ do
  let v = TV.Struct $ V.fromList
            [ (1, TV.String "outer")
            , (2, TV.Struct $ V.fromList
                [ (1, TV.String "inner")
                , (2, TV.I32 42)
                ])
            ]
  thriftFromJSON v (thriftToJSON v) @?= Right v

jsonContainers :: TestTree
jsonContainers = testGroup "JSON list/set/map"
  [ testCase "List of i32" $ do
      let v = TV.List TT_I32 (V.fromList [TV.I32 1, TV.I32 2, TV.I32 3])
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Set of strings" $ do
      let v = TV.Set TT_STRING (V.fromList [TV.String "a", TV.String "b"])
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Map i32->string" $ do
      let v = TV.Map TT_I32 TT_STRING
                (V.fromList [ (TV.I32 1, TV.String "one")
                            , (TV.I32 2, TV.String "two")
                            ])
      thriftFromJSON v (thriftToJSON v) @?= Right v
  ]

jsonEmptyContainers :: TestTree
jsonEmptyContainers = testGroup "JSON empty containers"
  [ testCase "Empty list" $ do
      let v = TV.List TT_I32 V.empty
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Empty set" $ do
      let v = TV.Set TT_STRING V.empty
      thriftFromJSON v (thriftToJSON v) @?= Right v

  , testCase "Empty map" $ do
      let v = TV.Map TT_I32 TT_STRING V.empty
      thriftFromJSON v (thriftToJSON v) @?= Right v
  ]

typedJsonMixedStruct :: TestTree
typedJsonMixedStruct = testCase "Typed JSON roundtrip (mixed struct)" $ do
  let v = TV.Struct $ V.fromList
            [ (1,  TV.Bool True)
            , (2,  TV.Byte 42)
            , (3,  TV.I16 1000)
            , (4,  TV.I32 100000)
            , (5,  TV.I64 9999999999)
            , (6,  TV.Double 3.14)
            , (7,  TV.String "hello world")
            , (8,  TV.Binary (BS.pack [0xDE, 0xAD, 0xBE, 0xEF]))
            , (9,  TV.List TT_I32 (V.fromList [TV.I32 1, TV.I32 2]))
            , (10, TV.Set TT_STRING (V.fromList [TV.String "x", TV.String "y"]))
            ]
      encoded = thriftToTypedJSON v
      decoded = thriftFromTypedJSON encoded
  decoded @?= Right v

--------------------------------------------------------------------------------
-- Thrift RPC message header roundtrip tests
--------------------------------------------------------------------------------

messageTests :: TestTree
messageTests = testGroup "Thrift Message Headers"
  [ binaryMessageTests
  , compactMessageTests
  ]

binaryMessageTests :: TestTree
binaryMessageTests = testGroup "Binary Protocol messages"
  [ testCase "Call roundtrip" $ do
      let msg = ThriftMessage "getUser" TMsgCall 1 simplePayload
      decodeMessageBinary (encodeMessageBinary msg) @?= Right msg

  , testCase "Reply roundtrip" $ do
      let msg = ThriftMessage "getUser" TMsgReply 1 simplePayload
      decodeMessageBinary (encodeMessageBinary msg) @?= Right msg

  , testCase "Exception roundtrip" $ do
      let msg = ThriftMessage "getUser" TMsgException 42 simplePayload
      decodeMessageBinary (encodeMessageBinary msg) @?= Right msg

  , testCase "Oneway roundtrip" $ do
      let msg = ThriftMessage "fireAndForget" TMsgOneway 99 (TV.Struct V.empty)
      decodeMessageBinary (encodeMessageBinary msg) @?= Right msg

  , testCase "Complex payload roundtrip" $ do
      let payload = TV.Struct $ V.fromList
            [ (1, TV.String "hello")
            , (2, TV.I32 42)
            , (3, TV.List TT_I64 (V.fromList [TV.I64 100, TV.I64 200]))
            ]
          msg = ThriftMessage "complexMethod" TMsgCall 7 payload
      decodeMessageBinary (encodeMessageBinary msg) @?= Right msg

  , testCase "Empty method name" $ do
      let msg = ThriftMessage "" TMsgCall 0 (TV.Struct V.empty)
      decodeMessageBinary (encodeMessageBinary msg) @?= Right msg

  , testCase "Negative seqid" $ do
      let msg = ThriftMessage "test" TMsgReply (-1) simplePayload
      decodeMessageBinary (encodeMessageBinary msg) @?= Right msg
  ]

compactMessageTests :: TestTree
compactMessageTests = testGroup "Compact Protocol messages"
  [ testCase "Call roundtrip" $ do
      let msg = ThriftMessage "getUser" TMsgCall 1 simplePayload
      decodeMessageCompact (encodeMessageCompact msg) @?= Right msg

  , testCase "Reply roundtrip" $ do
      let msg = ThriftMessage "getUser" TMsgReply 1 simplePayload
      decodeMessageCompact (encodeMessageCompact msg) @?= Right msg

  , testCase "Exception roundtrip" $ do
      let msg = ThriftMessage "getUser" TMsgException 42 simplePayload
      decodeMessageCompact (encodeMessageCompact msg) @?= Right msg

  , testCase "Oneway roundtrip" $ do
      let msg = ThriftMessage "fireAndForget" TMsgOneway 99 (TV.Struct V.empty)
      decodeMessageCompact (encodeMessageCompact msg) @?= Right msg

  , testCase "Complex payload roundtrip" $ do
      let payload = TV.Struct $ V.fromList
            [ (1, TV.String "hello")
            , (2, TV.I32 42)
            , (3, TV.List TT_I64 (V.fromList [TV.I64 100, TV.I64 200]))
            ]
          msg = ThriftMessage "complexMethod" TMsgCall 7 payload
      decodeMessageCompact (encodeMessageCompact msg) @?= Right msg

  , testCase "Empty method name" $ do
      let msg = ThriftMessage "" TMsgCall 0 (TV.Struct V.empty)
      decodeMessageCompact (encodeMessageCompact msg) @?= Right msg

  , testCase "Large seqid" $ do
      let msg = ThriftMessage "test" TMsgReply 100000 simplePayload
      decodeMessageCompact (encodeMessageCompact msg) @?= Right msg

  , testCase "Binary and Compact produce different bytes" $ do
      let msg = ThriftMessage "method" TMsgCall 1 simplePayload
          binBytes  = encodeMessageBinary msg
          compBytes = encodeMessageCompact msg
      assertBool "Binary and Compact should produce different bytes"
                 (binBytes /= compBytes)
      decodeMessageBinary binBytes @?= Right msg
      decodeMessageCompact compBytes @?= Right msg
  ]

simplePayload :: TV.Value
simplePayload = TV.Struct (V.fromList [(1, TV.I32 42)])

--------------------------------------------------------------------------------
-- Thrift framed transport tests
--------------------------------------------------------------------------------

transportTests :: TestTree
transportTests = testGroup "Thrift Framed Transport"
  [ testCase "frame/unframe roundtrip" $ do
      let payload = BS.pack [1,2,3,4,5]
      unframeMessage (frameMessage payload) @?= Right payload

  , testCase "frame/unframe empty payload" $ do
      let payload = BS.empty
      unframeMessage (frameMessage payload) @?= Right payload

  , testCase "frame adds 4-byte big-endian length prefix" $ do
      let payload = BS.pack [0xDE, 0xAD]
          framed = frameMessage payload
      BS.length framed @?= 6
      BS.take 4 framed @?= BS.pack [0, 0, 0, 2]

  , testCase "unframe rejects too-short input" $ do
      case unframeMessage (BS.pack [0, 0]) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on short input"

  , testCase "unframe rejects truncated payload" $ do
      case unframeMessage (BS.pack [0, 0, 0, 10, 1, 2]) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on truncated payload"

  , testCase "unframeMessages empty" $ do
      unframeMessages BS.empty @?= Right []

  , testCase "unframeMessages multiple" $ do
      let p1 = BS.pack [1,2]
          p2 = BS.pack [3,4,5]
          stream = frameMessage p1 <> frameMessage p2
      unframeMessages stream @?= Right [p1, p2]

  , testCase "unframeMessages with framed thrift message" $ do
      let msg = ThriftMessage "test" TMsgCall 1 simplePayload
          payload = encodeMessageBinary msg
          framed = frameMessage payload
      case unframeMessage framed of
        Right unframed -> decodeMessageBinary unframed @?= Right msg
        Left err -> assertFailure err
  ]
