{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the new direct-poke Wire codec
-- (`Kafka.Protocol.Wire` + `Kafka.Protocol.Wire.Primitives`).
--
-- Two layers of confidence:
--
--   1. Pure round-trip: encode @x@ via 'runWirePut', decode the
--      bytes via 'runWireGet', assert @decoded == x@.
--
--   2. Cross-codec equivalence: encode @x@ via the legacy
--      `Data.Bytes.Serial`-based path and via 'runWirePut'; the
--      bytes must be identical (the wire format is fixed by the
--      Kafka spec, so two correct encoders cannot diverge).
module Protocol.WireSpec (tests) where

import qualified Data.ByteString as BS
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (serialize)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP

tests :: TestTree
tests = testGroup "Wire codec"
  [ testGroup "fixed-width primitives — round trip"
      [ testProperty "Int8"   prop_int8
      , testProperty "Int16"  prop_int16
      , testProperty "Int32"  prop_int32
      , testProperty "Int64"  prop_int64
      , testProperty "Word16" prop_word16
      , testProperty "Word32" prop_word32
      , testProperty "Bool"   prop_bool
      ]
  , testGroup "fixed-width — cross-codec equivalence"
      [ testProperty "Int16 == Serial Int16"
          prop_int16_eq_serial
      , testProperty "Int32 == Serial Int32"
          prop_int32_eq_serial
      , testProperty "Int64 == Serial Int64"
          prop_int64_eq_serial
      ]
  , testGroup "variable-length integers — round trip"
      [ testProperty "VarInt"  prop_varint
      , testProperty "VarLong" prop_varlong
      , testProperty "UVarInt" prop_uvarint
      ]
  , testGroup "variable-length — cross-codec equivalence"
      [ testProperty "VarInt  == Serial VarInt"  prop_varint_eq_serial
      , testProperty "VarLong == Serial VarLong" prop_varlong_eq_serial
      , testProperty "UVarInt == Serial UVarInt" prop_uvarint_eq_serial
      ]
  , testGroup "Kafka strings / bytes / arrays — round trip"
      [ testProperty "KafkaString"   prop_kafka_string
      , testProperty "CompactString" prop_compact_string
      , testProperty "KafkaBytes"    prop_kafka_bytes
      , testProperty "CompactBytes"  prop_compact_bytes
      ]
  , testGroup "Kafka strings / bytes — cross-codec equivalence"
      [ testProperty "KafkaString  == Serial" prop_kafka_string_eq
      , testProperty "CompactString == Serial" prop_compact_string_eq
      , testProperty "KafkaBytes   == Serial" prop_kafka_bytes_eq
      , testProperty "CompactBytes  == Serial" prop_compact_bytes_eq
      ]
  , testGroup "edge cases"
      [ testCase "runWireGet on truncated input returns Left"
          truncated_left
      , testCase "VarInt longer than 5 bytes is rejected"
          oversized_varint
      ]
  ]

----------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------

genText :: Gen P.KafkaString
genText = do
  isNull <- Gen.bool
  if isNull
    then pure (P.KafkaString P.Null)
    else P.mkKafkaString <$> Gen.text (Range.linear 0 256) Gen.alphaNum

genCompactText :: Gen P.CompactString
genCompactText = P.toCompactString <$> genText

genBytes :: Gen P.KafkaBytes
genBytes = do
  isNull <- Gen.bool
  if isNull
    then pure (P.KafkaBytes P.Null)
    else P.mkKafkaBytes <$> Gen.bytes (Range.linear 0 256)

genCompactBytes :: Gen P.CompactBytes
genCompactBytes = P.toCompactBytes <$> genBytes

----------------------------------------------------------------------
-- Round-trip properties
----------------------------------------------------------------------

prop_int8 :: Property
prop_int8 = property $ do
  x <- forAll (Gen.int8 Range.linearBounded)
  W.runWireGet (W.runWirePut x) === Right x

prop_int16 :: Property
prop_int16 = property $ do
  x <- forAll (Gen.int16 Range.linearBounded)
  W.runWireGet (W.runWirePut x) === Right x

prop_int32 :: Property
prop_int32 = property $ do
  x <- forAll (Gen.int32 Range.linearBounded)
  W.runWireGet (W.runWirePut x) === Right x

prop_int64 :: Property
prop_int64 = property $ do
  x <- forAll (Gen.int64 Range.linearBounded)
  W.runWireGet (W.runWirePut x) === Right x

prop_word16 :: Property
prop_word16 = property $ do
  x <- forAll (Gen.word16 Range.linearBounded)
  W.runWireGet (W.runWirePut x) === Right x

prop_word32 :: Property
prop_word32 = property $ do
  x <- forAll (Gen.word32 Range.linearBounded)
  W.runWireGet (W.runWirePut x) === Right x

prop_bool :: Property
prop_bool = property $ do
  x <- forAll Gen.bool
  W.runWireGet (W.runWirePut x) === Right x

prop_varint :: Property
prop_varint = property $ do
  x <- forAll (Gen.int32 Range.linearBounded)
  let v = P.VarInt x
  W.runWireGet (W.runWirePut v) === Right v

prop_varlong :: Property
prop_varlong = property $ do
  x <- forAll (Gen.int64 Range.linearBounded)
  let v = P.VarLong x
  W.runWireGet (W.runWirePut v) === Right v

prop_uvarint :: Property
prop_uvarint = property $ do
  x <- forAll (Gen.word32 Range.linearBounded)
  let v = P.UVarInt x
  W.runWireGet (W.runWirePut v) === Right v

prop_kafka_string :: Property
prop_kafka_string = property $ do
  x <- forAll genText
  W.runWireGet (W.runWirePut x) === Right x

prop_compact_string :: Property
prop_compact_string = property $ do
  x <- forAll genCompactText
  W.runWireGet (W.runWirePut x) === Right x

prop_kafka_bytes :: Property
prop_kafka_bytes = property $ do
  x <- forAll genBytes
  W.runWireGet (W.runWirePut x) === Right x

prop_compact_bytes :: Property
prop_compact_bytes = property $ do
  x <- forAll genCompactBytes
  W.runWireGet (W.runWirePut x) === Right x

----------------------------------------------------------------------
-- Cross-codec equivalence
----------------------------------------------------------------------

prop_int16_eq_serial :: Property
prop_int16_eq_serial = property $ do
  x <- forAll (Gen.int16 Range.linearBounded)
  W.runWirePut x === runPutS (serialize x)

prop_int32_eq_serial :: Property
prop_int32_eq_serial = property $ do
  x <- forAll (Gen.int32 Range.linearBounded)
  W.runWirePut x === runPutS (serialize x)

prop_int64_eq_serial :: Property
prop_int64_eq_serial = property $ do
  x <- forAll (Gen.int64 Range.linearBounded)
  W.runWirePut x === runPutS (serialize x)

prop_varint_eq_serial :: Property
prop_varint_eq_serial = property $ do
  x <- forAll (Gen.int32 Range.linearBounded)
  let v = P.VarInt x
  W.runWirePut v === runPutS (serialize v)

prop_varlong_eq_serial :: Property
prop_varlong_eq_serial = property $ do
  x <- forAll (Gen.int64 Range.linearBounded)
  let v = P.VarLong x
  W.runWirePut v === runPutS (serialize v)

prop_uvarint_eq_serial :: Property
prop_uvarint_eq_serial = property $ do
  x <- forAll (Gen.word32 Range.linearBounded)
  let v = P.UVarInt x
  W.runWirePut v === runPutS (serialize v)

prop_kafka_string_eq :: Property
prop_kafka_string_eq = property $ do
  x <- forAll genText
  W.runWirePut x === runPutS (serialize x)

prop_compact_string_eq :: Property
prop_compact_string_eq = property $ do
  x <- forAll genCompactText
  W.runWirePut x === runPutS (serialize x)

prop_kafka_bytes_eq :: Property
prop_kafka_bytes_eq = property $ do
  x <- forAll genBytes
  W.runWirePut x === runPutS (serialize x)

prop_compact_bytes_eq :: Property
prop_compact_bytes_eq = property $ do
  x <- forAll genCompactBytes
  W.runWirePut x === runPutS (serialize x)

----------------------------------------------------------------------
-- Edge cases
----------------------------------------------------------------------

truncated_left :: IO ()
truncated_left = do
  -- An Int32 needs 4 bytes; give it 3.
  case W.runWireGet (BS.pack [0, 0, 0]) :: Either String Int32 of
    Left _  -> pure ()
    Right _ -> error "expected truncated input to be Left"

oversized_varint :: IO ()
oversized_varint = do
  -- 6 bytes all with the continuation bit set.
  case W.runWireGet (BS.replicate 6 0xFF) :: Either String P.UVarInt of
    Left _  -> pure ()
    Right _ -> error "expected oversized UVarInt to be Left"
