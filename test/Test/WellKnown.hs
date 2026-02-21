module Test.WellKnown (wellKnownTests) where

import qualified Data.ByteString as BS
import Data.Proxy (Proxy(..))
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Proto.Encode
import Proto.Decode
import Proto.Google.Protobuf.Timestamp
import Proto.Google.Protobuf.Duration
import Proto.Google.Protobuf.Any
import Proto.Google.Protobuf.Any.Util
import Proto.Google.Protobuf.Empty
import Proto.Google.Protobuf.Wrappers
import Proto.Google.Protobuf.FieldMask
import Proto.Google.Protobuf.SourceContext
import Proto.Google.Protobuf.Struct
import Proto.Message (IsMessage(..))
import Proto.Google.Protobuf.WellKnownInstances ()

wellKnownTests :: TestTree
wellKnownTests = testGroup "Well-Known Types"
  [ testGroup "Timestamp"
      [ testProperty "roundtrip" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear (-1000000000) 1000000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999999)
          let msg = defaultTimestamp { timestampSeconds = s, timestampNanos = n }
              encoded = encodeMessage msg
          decodeMessage encoded === Right msg

      , testCase "default is zero-length" $ do
          let encoded = encodeMessage defaultTimestamp
          BS.length encoded @?= 0

      , testCase "sized encoding matches" $ do
          let msg = defaultTimestamp { timestampSeconds = 1234567890, timestampNanos = 123456789 }
          BS.length (encodeMessage msg) @?= messageSize msg
      ]

  , testGroup "Duration"
      [ testProperty "roundtrip" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear (-315576000000) 315576000000)
          n <- forAll $ Gen.int32 (Range.linear (-999999999) 999999999)
          let msg = defaultDuration { durationSeconds = s, durationNanos = n }
              encoded = encodeMessage msg
          decodeMessage encoded === Right msg
      ]

  , testGroup "Any"
      [ testProperty "raw Any roundtrip" $ property $ do
          tu <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
          v <- forAll $ Gen.bytes (Range.linear 0 200)
          let msg = defaultAny { anyTypeurl = tu, anyValue = v }
              encoded = encodeMessage msg
          decodeMessage encoded === Right msg

      , testProperty "packAny Timestamp roundtrip" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear 0 2000000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999999)
          let ts = defaultTimestamp { timestampSeconds = s, timestampNanos = n }
              packed = packAny ts
          anyTypeurl packed === "type.googleapis.com/google.protobuf.Timestamp"
          case unpackAny packed of
            Just (Right decoded) -> decoded === ts
            Just (Left err) -> do annotate (show err); failure
            Nothing -> do annotate "type mismatch"; failure

      , testProperty "packAny Duration roundtrip" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear 0 1000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999999)
          let dur = defaultDuration { durationSeconds = s, durationNanos = n }
              packed = packAny dur
          case unpackAny packed of
            Just (Right decoded) -> decoded === dur
            _ -> failure

      , testCase "packAny Empty" $ do
          let packed = packAny Empty
          anyTypeurl packed @?= "type.googleapis.com/google.protobuf.Empty"
          case unpackAny packed of
            Just (Right Empty) -> pure ()
            other -> assertFailure ("Expected Just (Right Empty), got " <> show other)

      , testCase "unpackAny type mismatch returns Nothing" $ do
          let packed = packAny (defaultTimestamp { timestampSeconds = 100 })
          case unpackAny packed :: Maybe (Either DecodeError Duration) of
            Nothing -> pure ()
            Just _  -> assertFailure "Should not match Duration"

      , testCase "isMessageType" $ do
          let packed = packAny (defaultDuration { durationSeconds = 60 })
          isMessageType (Proxy :: Proxy Duration) packed @?= True
          isMessageType (Proxy :: Proxy Timestamp) packed @?= False

      , testCase "typeNameFromUrl strips prefix" $ do
          typeNameFromUrl "type.googleapis.com/google.protobuf.Timestamp"
            @?= "google.protobuf.Timestamp"
          typeNameFromUrl "mycompany.com/types/example.Foo"
            @?= "example.Foo"
          typeNameFromUrl "google.protobuf.Timestamp"
            @?= "google.protobuf.Timestamp"

      , testCase "packAnyWithPrefix custom prefix" $ do
          let packed = packAnyWithPrefix "myhost/" (defaultTimestamp { timestampSeconds = 1 })
          anyTypeurl packed @?= "myhost/google.protobuf.Timestamp"
          case unpackAny packed of
            Just (Right ts) -> ts @?= defaultTimestamp { timestampSeconds = 1 }
            _ -> assertFailure "Should unpack with any prefix"

      , testCase "Any wire roundtrip preserves content" $ do
          let ts = defaultTimestamp { timestampSeconds = 1234567890, timestampNanos = 500000000 }
              packed = packAny ts
              encodedAny = encodeMessage packed
          case decodeMessage encodedAny of
            Left err -> assertFailure (show err)
            Right decodedAny -> case unpackAny decodedAny of
              Just (Right (decoded :: Timestamp)) -> decoded @?= ts
              _ -> assertFailure "Should unpack after wire roundtrip"

      , testCase "TypeRegistry dynamic dispatch" $ do
          let registry = registerType (Proxy :: Proxy Timestamp)
                       . registerType (Proxy :: Proxy Duration)
                       . registerType (Proxy :: Proxy Empty)
                       $ emptyRegistry
          case unpackAnyDynamic registry (packAny (defaultTimestamp { timestampSeconds = 42 })) of
            Just (Right (DynamicMessage msg)) ->
              show msg @?= "Timestamp {timestampSeconds = 42, timestampNanos = 0}"
            _ -> assertFailure "Should unpack Timestamp dynamically"
          case unpackAnyDynamic registry (packAny (defaultDuration { durationSeconds = 60 })) of
            Just (Right (DynamicMessage msg)) ->
              show msg @?= "Duration {durationSeconds = 60, durationNanos = 0}"
            _ -> assertFailure "Should unpack Duration dynamically"

      , testCase "TypeRegistry unknown type returns Nothing" $ do
          let registry = registerType (Proxy :: Proxy Timestamp) emptyRegistry
              unknownAny = defaultAny { anyTypeurl = "type.googleapis.com/unknown.Type" }
          case unpackAnyDynamic registry unknownAny of
            Nothing -> pure ()
            Just _  -> assertFailure "Should return Nothing for unknown type"
      ]

  , testGroup "Empty"
      [ testCase "empty roundtrip" $ do
          let encoded = encodeMessage Empty
          BS.length encoded @?= 0
          decodeMessage encoded @?= Right Empty
      ]

  , testGroup "Wrappers"
      [ testProperty "Int64Value roundtrip" $ property $ do
          v <- forAll $ Gen.int64 Range.linearBounded
          let msg = defaultInt64Value { int64ValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "UInt64Value roundtrip" $ property $ do
          v <- forAll $ Gen.word64 (Range.linear 0 maxBound)
          let msg = defaultUInt64Value { uInt64ValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "Int32Value roundtrip" $ property $ do
          v <- forAll $ Gen.int32 Range.linearBounded
          let msg = defaultInt32Value { int32ValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "BoolValue roundtrip" $ property $ do
          v <- forAll Gen.bool
          let msg = defaultBoolValue { boolValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "StringValue roundtrip" $ property $ do
          v <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
          let msg = defaultStringValue { stringValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "BytesValue roundtrip" $ property $ do
          v <- forAll $ Gen.bytes (Range.linear 0 200)
          let msg = defaultBytesValue { bytesValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "DoubleValue roundtrip" $ property $ do
          v <- forAll $ Gen.double (Range.linearFrac (-1e100) 1e100)
          let msg = defaultDoubleValue { doubleValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg

      , testProperty "FloatValue roundtrip" $ property $ do
          v <- forAll $ Gen.float (Range.linearFrac (-1e30) 1e30)
          let msg = defaultFloatValue { floatValueValue = v }
          decodeMessage (encodeMessage msg) === Right msg
      ]

  , testGroup "FieldMask"
      [ testCase "roundtrip" $ do
          let msg = defaultFieldMask { fieldMaskPaths = V.fromList ["foo.bar", "baz"] }
              encoded = encodeMessage msg
          decodeMessage encoded @?= Right msg
      ]

  , testGroup "SourceContext"
      [ testProperty "roundtrip" $ property $ do
          fn <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
          let msg = defaultSourceContext { sourceContextFilename = fn }
          decodeMessage (encodeMessage msg) === Right msg
      ]

  , testGroup "Struct"
      [ testCase "empty struct roundtrip" $ do
          let msg = defaultStruct
          decodeMessage (encodeMessage msg) @?= Right msg

      , testCase "value null roundtrip" $ do
          let v = defaultValue { valueKind = Just (Value'Kind'NullValue NullValue'NullValue) }
          decodeMessage (encodeMessage v) @?= Right v

      , testCase "value number roundtrip" $ do
          let v = defaultValue { valueKind = Just (Value'Kind'NumberValue 3.14) }
          decodeMessage (encodeMessage v) @?= Right v

      , testCase "value string roundtrip" $ do
          let v = defaultValue { valueKind = Just (Value'Kind'StringValue "hello") }
          decodeMessage (encodeMessage v) @?= Right v

      , testCase "value bool roundtrip" $ do
          let v = defaultValue { valueKind = Just (Value'Kind'BoolValue True) }
          decodeMessage (encodeMessage v) @?= Right v

      , testCase "list value roundtrip" $ do
          let lv = defaultListValue { listValueValues = V.fromList
                [ defaultValue { valueKind = Just (Value'Kind'NumberValue 1) }
                , defaultValue { valueKind = Just (Value'Kind'StringValue "two") }
                , defaultValue { valueKind = Just (Value'Kind'BoolValue False) }
                ] }
          decodeMessage (encodeMessage lv) @?= Right lv
      ]

  , testGroup "Exact-size encoding"
      [ testProperty "encodeMessageSized matches encodeMessage for Timestamp" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear 0 1000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999)
          let msg = defaultTimestamp { timestampSeconds = s, timestampNanos = n }
          encodeMessageSized msg === encodeMessage msg

      , testProperty "encodeMessageSized matches encodeMessage for Duration" $ property $ do
          s <- forAll $ Gen.int64 (Range.linear 0 1000000)
          n <- forAll $ Gen.int32 (Range.linear 0 999999)
          let msg = defaultDuration { durationSeconds = s, durationNanos = n }
          encodeMessageSized msg === encodeMessage msg
      ]
  ]
