{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Serde.Avro
Description : Apache Avro 'Kafka.Serde.Serde' built on top of @wireform-avro@.
Copyright   : (c) 2025
License     : BSD-3-Clause

A schema-driven 'Serde' value for any @wireform-avro@-typed
payload, suitable for plugging into 'Kafka.Topic.Topic' or
directly into 'Kafka.Client.Producer.publish' \/ a typed consume
helper.

= Recommended usage

@
import           Data.Text                  (Text)
import qualified Kafka
import qualified Kafka.Topic                as Topic
import qualified Kafka.Serde                as Serde
import qualified Kafka.Serde.Avro           as AvroSerde
import qualified Avro.Schema                as Avro
import qualified Avro.IDL                   as Avro

orderSchema :: Avro.AvroType
orderSchema = Avro.parseSchemaOrDie ...   -- your Avro schema

events :: Topic.'Topic.Topic' Text MyOrder
events =
  Topic.'Topic.topic' \"orders\" Serde.'Serde.textSerde'
    (AvroSerde.'avroSerde' orderSchema)
@

= Schema lifecycle

Avro is __schema-driven__ — both writes and reads need an
@AvroType@ in hand. Unlike Protocol Buffers, the wire bytes don't
carry the schema, so it's the caller's job to supply the same
schema on both sides (or to wrap the serde with a Schema-Registry
envelope; see "Kafka.Streams.Serde.SchemaRegistry").

= Variants

  * 'avroSerde'     — typed values via 'ToAvro' / 'FromAvro'.
    Encodes through 'Avro.Class.toAvro' :: a -> 'AV.Value', then
    'Avro.Encode.encodeAvro' over the schema. Decodes the inverse.
  * 'avroValueSerde' — untyped 'Avro.Value.Value' on both sides.
    Useful when the application is happy to work in the dynamic
    value representation (e.g. a generic streams operator).
-}
module Kafka.Serde.Avro (
  -- * Typed value serde
  avroSerde,

  -- * Dynamic-value serde
  avroValueSerde,

  -- * Lower-level access
  encodeAvroValue,
  decodeAvroValue,
) where

import Avro.Class qualified as Avro.Class
import Avro.Decode qualified as Avro.Decode
import Avro.Encode qualified as Avro.Encode
import Avro.Schema qualified as Avro.Schema
import Avro.Value qualified as Avro.Value
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Kafka.Serde (Serde (..))


{- | Schema-driven typed Avro serde. Threads the application
value through 'Avro.Class.toAvro' \/ 'Avro.Class.fromAvro' on
either side of the 'Avro.Encode.encodeAvro' \/
'Avro.Decode.decodeAvro' pair.

The caller must supply the writer schema; for Schema-Registry-
backed deployments use the @wireform-kafka-streams@
"Kafka.Streams.Serde.SchemaRegistry" wrapper, which fetches
the schema lazily by id.
-}
avroSerde
  :: (Avro.Class.ToAvro a, Avro.Class.FromAvro a)
  => Avro.Schema.AvroType
  -> Serde a
avroSerde schema =
  Serde
    { serialize = \a -> Avro.Encode.encodeAvro schema (Avro.Class.toAvro a)
    , deserialize = \b -> case Avro.Decode.decodeAvro schema b of
        Left e -> Left (T.pack e)
        Right val -> case Avro.Class.fromAvro val of
          Left e' -> Left (T.pack e')
          Right a' -> Right a'
    , serializeHeaders = const mempty
    }


{- | Dynamic-value serde. Operates directly on 'Avro.Value.Value'
— useful for generic stream operators that don't have a Haskell
type for the payload.
-}
avroValueSerde :: Avro.Schema.AvroType -> Serde Avro.Value.Value
avroValueSerde schema =
  Serde
    { serialize = Avro.Encode.encodeAvro schema
    , deserialize = \b -> case Avro.Decode.decodeAvro schema b of
        Left e -> Left (T.pack e)
        Right v -> Right v
    , serializeHeaders = const mempty
    }


{- | Standalone encode of an 'Avro.Value.Value'. Identical to
'Avro.Encode.encodeAvro'; re-exported for symmetry with
'Kafka.Serde.Proto.encodeProto'.
-}
encodeAvroValue :: Avro.Schema.AvroType -> Avro.Value.Value -> BS.ByteString
encodeAvroValue = Avro.Encode.encodeAvro


{- | Standalone decode. Identical to 'Avro.Decode.decodeAvro';
the schema must be supplied by the caller.
-}
decodeAvroValue
  :: Avro.Schema.AvroType
  -> BS.ByteString
  -> Either String Avro.Value.Value
decodeAvroValue = Avro.Decode.decodeAvro
