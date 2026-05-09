{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocol.Generated.SimpleRoundTripSpec (tests) where

import qualified Data.ByteString as BS
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (Serial(..))
import Data.Int (Int32)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Kafka.Protocol.Primitives as P
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.Hedgehog

-- | Test that Nullable types round-trip correctly.
-- Note: This tests the Serial instance for Nullable which is used for nullable structs.
prop_roundtrip_nullable_int32 :: Property
prop_roundtrip_nullable_int32 = property $ do
  value :: P.Nullable Int32 <- forAll $ genNullableInt32
  
  let encoded = runPutS $ serialize value
      decoded = runGetS deserialize encoded
  
  case decoded of
    Right decodedValue -> decodedValue === value
    Left err -> do
      annotate $ "Decode error: " ++ err
      annotate $ "Encoded bytes: " ++ show (BS.length encoded)
      failure

-- | Test that KafkaString round-trips correctly.
prop_roundtrip_kafka_string :: Property
prop_roundtrip_kafka_string = property $ do
  value :: P.KafkaString <- forAll $ genKafkaString
  
  let encoded = runPutS $ serialize value
      decoded = runGetS deserialize encoded
  
  case decoded of
    Right decodedValue -> decodedValue === value
    Left err -> do
      annotate $ "Decode error: " ++ err
      annotate $ "Encoded bytes: " ++ show (BS.length encoded)
      failure

-- | Test that Kafka arrays round-trip correctly.
prop_roundtrip_kafka_array_int32 :: Property
prop_roundtrip_kafka_array_int32 = property $ do
  value :: P.KafkaArray Int32 <- forAll $ genKafkaArrayInt32
  
  let encoded = runPutS $ serialize value
      decoded = runGetS deserialize encoded
  
  case decoded of
    Right decodedValue -> decodedValue === value
    Left err -> do
      annotate $ "Decode error: " ++ err
      annotate $ "Encoded bytes: " ++ show (BS.length encoded)
      failure

-- Generators

genNullableInt32 :: Gen (P.Nullable Int32)
genNullableInt32 = Gen.choice
  [ return P.Null
  , P.NotNull <$> Gen.int32 Range.constantBounded
  ]

genKafkaString :: Gen P.KafkaString
genKafkaString = Gen.choice
  [ return $ P.mkKafkaString ""  -- Empty string for null-ish case
  , do str <- Gen.text (Range.linear 0 50) Gen.alphaNum
       return $ P.mkKafkaString str
  ]

genKafkaArrayInt32 :: Gen (P.KafkaArray Int32)
genKafkaArrayInt32 = Gen.choice
  [ return $ P.mkKafkaArray V.empty  -- Empty array
  , do count <- Gen.int (Range.linear 0 10)
       values <- Gen.list (Range.singleton count) (Gen.int32 Range.constantBounded)
       return $ P.mkKafkaArray (V.fromList values)
  ]

tests :: TestTree
tests = testGroup "Simple Round-Trip Tests"
  [ testProperty "Nullable Int32 round-trips" prop_roundtrip_nullable_int32
  , testProperty "KafkaString round-trips" prop_roundtrip_kafka_string
  , testProperty "KafkaArray Int32 round-trips" prop_roundtrip_kafka_array_int32
  ]
