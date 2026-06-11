{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Round-trip tests for the direct-poke Wire codec
(`Kafka.Protocol.Wire` + `Kafka.Protocol.Wire.Primitives`).

The per-message Wire codec round-trip lives in
'Protocol.WireCodecParitySpec' and the per-vector exact-byte
tests in 'Protocol.Generated.*Spec'.
-}
module Protocol.WireSpec (tests) where

import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()
import "wireform-kafka-protocol" Kafka.Protocol.Primitives qualified as P
import "wireform-kafka-protocol" Kafka.Protocol.Wire qualified as W
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Primitives qualified as WP


tests :: Spec
tests =
  describe "Wire codec" $
    sequence_
      [ describe "fixed-width primitives — round trip" $
          sequence_
            [ it "Int8" prop_int8
            , it "Int16" prop_int16
            , it "Int32" prop_int32
            , it "Int64" prop_int64
            , it "Word16" prop_word16
            , it "Word32" prop_word32
            , it "Bool" prop_bool
            ]
      , describe "variable-length integers — round trip" $
          sequence_
            [ it "VarInt" prop_varint
            , it "VarLong" prop_varlong
            , it "UVarInt" prop_uvarint
            ]
      , describe "Kafka strings / bytes / arrays — round trip" $
          sequence_
            [ it "KafkaString" prop_kafka_string
            , it "CompactString" prop_compact_string
            , it "KafkaBytes" prop_kafka_bytes
            , it "CompactBytes" prop_compact_bytes
            ]
      , describe "edge cases" $
          sequence_
            [ it
                "runWireGet on truncated input returns Left"
                truncated_left
            , it
                "VarInt longer than 5 bytes is rejected"
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
    Left _ -> pure ()
    Right _ -> error "expected truncated input to be Left"


oversized_varint :: IO ()
oversized_varint = do
  -- 6 bytes all with the continuation bit set.
  case W.runWireGet (BS.replicate 6 0xFF) :: Either String P.UVarInt of
    Left _ -> pure ()
    Right _ -> error "expected oversized UVarInt to be Left"
