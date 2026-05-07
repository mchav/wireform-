{-# LANGUAGE OverloadedStrings #-}

module Protocol.PrimitivesSpec (tests) where

import qualified Data.ByteString as BS
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Bytes.Serial
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Kafka.Protocol.Primitives
import Test.Tasty
import Test.Tasty.Hedgehog

tests :: TestTree
tests = testGroup "Primitives"
  [ testProperty "VarInt round-trip" prop_varIntRoundTrip
  , testProperty "VarLong round-trip" prop_varLongRoundTrip
  , testProperty "UVarInt round-trip" prop_uvarIntRoundTrip
  , testProperty "KafkaString round-trip" prop_kafkaStringRoundTrip
  , testProperty "CompactString round-trip" prop_compactStringRoundTrip
  ]

-- | Test VarInt encoding/decoding round-trip
prop_varIntRoundTrip :: Property
prop_varIntRoundTrip = property $ do
  value <- forAll $ Gen.int32 Range.constantBounded
  let encoded = runPutS $ serialize (VarInt value)
      decoded = runGetS deserialize encoded
  case decoded of
    Right (VarInt result) -> result === value
    Left err -> annotate err >> failure

-- | Test VarLong encoding/decoding round-trip
prop_varLongRoundTrip :: Property
prop_varLongRoundTrip = property $ do
  value <- forAll $ Gen.int64 Range.constantBounded
  let encoded = runPutS $ serialize (VarLong value)
      decoded = runGetS deserialize encoded
  case decoded of
    Right (VarLong result) -> result === value
    Left err -> annotate err >> failure

-- | Test UVarInt encoding/decoding round-trip
prop_uvarIntRoundTrip :: Property
prop_uvarIntRoundTrip = property $ do
  value <- forAll $ Gen.word32 Range.constantBounded
  let encoded = runPutS $ serialize (UVarInt value)
      decoded = runGetS deserialize encoded
  case decoded of
    Right (UVarInt result) -> result === value
    Left err -> annotate err >> failure

-- | Test KafkaString encoding/decoding round-trip
prop_kafkaStringRoundTrip :: Property
prop_kafkaStringRoundTrip = property $ do
  text <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
  let kafkaStr = mkKafkaString text
      encoded = runPutS $ serialize kafkaStr
      decoded = runGetS deserialize encoded
  case decoded of
    Right result -> result === kafkaStr
    Left err -> annotate err >> failure

-- | Test CompactString encoding/decoding round-trip
prop_compactStringRoundTrip :: Property
prop_compactStringRoundTrip = property $ do
  text <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
  let compactStr = mkCompactString text
      encoded = runPutS $ serialize compactStr
      decoded = runGetS deserialize encoded
  case decoded of
    Right result -> result === compactStr
    Left err -> annotate err >> failure

