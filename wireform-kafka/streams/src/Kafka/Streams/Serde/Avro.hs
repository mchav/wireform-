{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Streams.Serde.Avro
Description : Avro-binary payload serde for Confluent Schema Registry

Combines 'Kafka.Streams.Serde.SchemaRegistry' (the wire-envelope
helper) with a payload-level Avro encoder / decoder. The shape
mirrors Confluent's @KafkaAvroSerializer@ +
@KafkaAvroDeserializer@:

  * On the producer side: register / fetch the schema id, render
    the value with the supplied 'AvroEncoder', stamp the
    Confluent envelope.
  * On the consumer side: peel the envelope, optionally re-fetch
    the schema, decode the payload with the supplied
    'AvroDecoder'.

The actual Avro codec is intentionally pluggable: callers pass
in 'AvroEncoder' / 'AvroDecoder' values that wrap whatever Avro
library their organisation already uses (the wireform-avro
package is one option, but this module has no hard dependency
on it).
-}
module Kafka.Streams.Serde.Avro
  ( AvroEncoder (..)
  , AvroDecoder (..)
  , AvroSerdeConfig (..)
  , avroSerde
  ) where

import Data.ByteString (ByteString)

import Kafka.Streams.Serde (Serde (..))
import qualified Kafka.Streams.Serde.SchemaRegistry as SR

-- | A pluggable Avro value encoder. Returns the binary payload
-- /without/ the Confluent envelope; this module wraps it.
newtype AvroEncoder a = AvroEncoder { runAvroEncoder :: a -> ByteString }

newtype AvroDecoder a = AvroDecoder
  { runAvroDecoder :: ByteString -> Either String a
  }

data AvroSerdeConfig a = AvroSerdeConfig
  { ascClient    :: !SR.SchemaRegistryClient
  , ascSubject   :: !SR.SchemaSubject
  , ascSchema    :: !SR.SchemaPayload
    -- ^ The Avro schema JSON document to register on first use.
  , ascEncoder   :: !(AvroEncoder a)
  , ascDecoder   :: !(AvroDecoder a)
  }

-- | Wire the Schema Registry envelope around an Avro payload
-- codec. Result is a 'Serde' the streams DSL accepts.
avroSerde :: AvroSerdeConfig a -> IO (Serde a)
avroSerde AvroSerdeConfig{..} =
  SR.registrySerde SR.SchemaRegistrySerdeConfig
    { SR.srscClient  = ascClient
    , SR.srscSubject = ascSubject
    , SR.srscSchema  = ascSchema
    , SR.srscPayload = Serde
        { serialize   = runAvroEncoder ascEncoder
        , deserialize = runAvroDecoder ascDecoder
        }
    }
