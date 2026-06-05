{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Avro / JSON-Schema / Protobuf payload serdes
-- on top of 'Kafka.Streams.Serde.SchemaRegistry'.
module Streams.SchemaRegistryFormatsSpec (tests) where

import qualified Data.ByteString as BS
import Test.Syd

import qualified Kafka.Streams.Serde as Serde
import qualified Kafka.Streams.Serde.Avro as Avro
import qualified Kafka.Streams.Serde.JsonSchema as JS
import qualified Kafka.Streams.Serde.Protobuf as PB
import qualified Kafka.Streams.Serde.SchemaRegistry as SR

tests :: Spec
tests = describe "Schema-Registry payload serdes" $ sequence_
  [ it "Avro round-trip through the in-memory registry"
      avro_round_trip
  , it "JSON-Schema round-trip"
      json_round_trip
  , it "Protobuf round-trip with message-index 0"
      proto_round_trip_idx0
  , it "Protobuf round-trip with message-index 5"
      proto_round_trip_idxN
  , it "encodeMessageIndex(0) is the single byte 0x00"
      idx_zero
  , it "encodeMessageIndex / decodeMessageIndex round-trip"
      idx_round_trip
  ]

avro_round_trip :: IO ()
avro_round_trip = do
  reg <- SR.inMemoryRegistry
  s <- Avro.avroSerde Avro.AvroSerdeConfig
    { Avro.ascClient  = reg
    , Avro.ascSubject = SR.SchemaSubject "t-value"
    , Avro.ascSchema  = SR.SchemaPayload "{\"type\":\"string\"}"
    , Avro.ascEncoder = Avro.AvroEncoder id
    , Avro.ascDecoder = Avro.AvroDecoder Right
    }
  let !bs = Serde.serialize s "hello"
  Serde.deserialize s bs `shouldBe` Right "hello"

json_round_trip :: IO ()
json_round_trip = do
  reg <- SR.inMemoryRegistry
  s <- JS.jsonSchemaSerde JS.JsonSchemaSerdeConfig
    { JS.jssClient  = reg
    , JS.jssSubject = SR.SchemaSubject "t-value"
    , JS.jssSchema  = SR.SchemaPayload "{\"type\":\"object\"}"
    , JS.jssEncoder = JS.JsonSchemaEncoder id
    , JS.jssDecoder = JS.JsonSchemaDecoder Right
    }
  let !bs = Serde.serialize s "{\"a\":1}"
  Serde.deserialize s bs `shouldBe` Right "{\"a\":1}"

proto_round_trip_idx0 :: IO ()
proto_round_trip_idx0 = do
  reg <- SR.inMemoryRegistry
  s <- PB.protobufSerde PB.ProtobufSerdeConfig
    { PB.pscClient       = reg
    , PB.pscSubject      = SR.SchemaSubject "t-value"
    , PB.pscSchema       = SR.SchemaPayload "syntax = \"proto3\";"
    , PB.pscMessageIndex = 0
    , PB.pscEncoder      = PB.ProtobufEncoder id
    , PB.pscDecoder      = PB.ProtobufDecoder Right
    }
  let !bs = Serde.serialize s "payload"
  Serde.deserialize s bs `shouldBe` Right "payload"

proto_round_trip_idxN :: IO ()
proto_round_trip_idxN = do
  reg <- SR.inMemoryRegistry
  s <- PB.protobufSerde PB.ProtobufSerdeConfig
    { PB.pscClient       = reg
    , PB.pscSubject      = SR.SchemaSubject "t-value"
    , PB.pscSchema       = SR.SchemaPayload "syntax = \"proto3\";"
    , PB.pscMessageIndex = 5
    , PB.pscEncoder      = PB.ProtobufEncoder id
    , PB.pscDecoder      = PB.ProtobufDecoder Right
    }
  let !bs = Serde.serialize s "payload-5"
  Serde.deserialize s bs `shouldBe` Right "payload-5"

idx_zero :: IO ()
idx_zero =
  PB.encodeMessageIndex 0 `shouldBe` BS.singleton 0

idx_round_trip :: IO ()
idx_round_trip = do
  let bs = PB.encodeMessageIndex 7 <> "payload"
  case PB.decodeMessageIndex bs of
    Right (idx, rest) -> do
      idx  `shouldBe` 7
      rest `shouldBe` "payload"
    Left err -> error err
