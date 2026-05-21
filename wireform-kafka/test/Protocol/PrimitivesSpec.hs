{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | Round-trip tests for the protocol primitives via the 'Wire'
-- codec — exercises the per-primitive Wire helpers.
module Protocol.PrimitivesSpec (tests) where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import "wireform-kafka-protocol" Kafka.Protocol.Primitives
import qualified "wireform-kafka-protocol" Kafka.Protocol.Wire as W
import "wireform-kafka-protocol" Kafka.Protocol.Wire.Primitives ()
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

prop_varIntRoundTrip :: Property
prop_varIntRoundTrip = property $ do
  value <- forAll $ Gen.int32 Range.constantBounded
  let encoded = W.runWirePut (VarInt value)
  case W.runWireGet encoded of
    Right (VarInt result) -> result === value
    Left  err             -> annotate err >> failure

prop_varLongRoundTrip :: Property
prop_varLongRoundTrip = property $ do
  value <- forAll $ Gen.int64 Range.constantBounded
  let encoded = W.runWirePut (VarLong value)
  case W.runWireGet encoded of
    Right (VarLong result) -> result === value
    Left  err              -> annotate err >> failure

prop_uvarIntRoundTrip :: Property
prop_uvarIntRoundTrip = property $ do
  value <- forAll $ Gen.word32 Range.constantBounded
  let encoded = W.runWirePut (UVarInt value)
  case W.runWireGet encoded of
    Right (UVarInt result) -> result === value
    Left  err              -> annotate err >> failure

prop_kafkaStringRoundTrip :: Property
prop_kafkaStringRoundTrip = property $ do
  text <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
  let kafkaStr = mkKafkaString text
      encoded  = W.runWirePut kafkaStr
  case W.runWireGet encoded of
    Right result -> result === kafkaStr
    Left  err    -> annotate err >> failure

prop_compactStringRoundTrip :: Property
prop_compactStringRoundTrip = property $ do
  text <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
  let compactStr = mkCompactString text
      encoded    = W.runWirePut compactStr
  case W.runWireGet encoded of
    Right result -> result === compactStr
    Left  err    -> annotate err >> failure
