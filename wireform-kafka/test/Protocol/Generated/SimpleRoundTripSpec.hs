{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Round-trip tests for the protocol primitives that the codegen
-- composes into message codecs (KafkaString, KafkaArray, ...).
--
-- Before the no-Serial migration these tests went through
-- 'Data.Bytes.Serial' instances on the primitives. Those instances
-- are gone; the equivalent round-trip is now over the 'Wire'
-- instances + per-helper poke / peek pairs.
module Protocol.Generated.SimpleRoundTripSpec (tests) where

import Data.Int (Int32)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.Hedgehog

prop_roundtrip_kafka_string :: Property
prop_roundtrip_kafka_string = property $ do
  value :: P.KafkaString <- forAll genKafkaString
  let encoded = W.runWirePut value
  case W.runWireGet encoded of
    Right decoded -> decoded === value
    Left err      -> annotate err >> failure

prop_roundtrip_kafka_array_int32 :: Property
prop_roundtrip_kafka_array_int32 = property $ do
  value :: P.KafkaArray Int32 <- forAll genKafkaArrayInt32
  let !nElts = case P.unKafkaArray value of
        P.NotNull v -> V.length v
        P.Null      -> 0
      !ub      = 4 + 4 * nElts
      !encoded = W.runWirePokeWith ub $ \p ->
                   WP.pokeKafkaArray W.pokeInt32BE p value
      decoded  = W.runWireGetWith
                   (\_fp _bp p e -> WP.peekKafkaArray W.peekInt32BE p e)
                   encoded
  case decoded of
    Right d  -> d === value
    Left err -> annotate err >> failure

genKafkaString :: Gen P.KafkaString
genKafkaString = Gen.choice
  [ pure (P.KafkaString P.Null)
  , do str <- Gen.text (Range.linear 0 50) Gen.alphaNum
       pure (P.mkKafkaString str)
  ]

genKafkaArrayInt32 :: Gen (P.KafkaArray Int32)
genKafkaArrayInt32 = Gen.choice
  [ pure (P.mkKafkaArray V.empty)
  , do count  <- Gen.int (Range.linear 0 10)
       values <- Gen.list (Range.singleton count) (Gen.int32 Range.constantBounded)
       pure (P.mkKafkaArray (V.fromList values))
  ]

tests :: TestTree
tests = testGroup "Simple Round-Trip Tests"
  [ testProperty "KafkaString round-trips" prop_roundtrip_kafka_string
  , testProperty "KafkaArray Int32 round-trips" prop_roundtrip_kafka_array_int32
  ]
