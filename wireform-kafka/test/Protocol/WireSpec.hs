{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Round-trip tests for the direct-poke Wire codec
-- (`Kafka.Protocol.Wire` + `Kafka.Protocol.Wire.Primitives`).
--
-- Pre no-Serial migration this spec also held cross-codec
-- equivalence properties (Wire == Serial). The Serial codec is
-- gone now; the equivalent guarantee on the runtime path is the
-- per-message Wire codec round-trip in 'Protocol.WireCodecParitySpec'
-- and the per-vector exact-byte tests in 'Protocol.Generated.*Spec'.
module Protocol.WireSpec (tests) where

import qualified Data.ByteString as BS
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
  , testGroup "variable-length integers — round trip"
      [ testProperty "VarInt"  prop_varint
      , testProperty "VarLong" prop_varlong
      , testProperty "UVarInt" prop_uvarint
      ]
  , testGroup "Kafka strings / bytes / arrays — round trip"
      [ testProperty "KafkaString"   prop_kafka_string
      , testProperty "CompactString" prop_compact_string
      , testProperty "KafkaBytes"    prop_kafka_bytes
      , testProperty "CompactBytes"  prop_compact_bytes
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
