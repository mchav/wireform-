{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Streams.Serde.JsonSchema
Description : JSON-Schema payload serde for Confluent Schema Registry

Mirror of "Kafka.Streams.Serde.Avro" but for JSON-Schema-typed
payloads. The wire format is identical (Confluent envelope +
JSON body); the schema registered with the registry is a
JSON-Schema document rather than an Avro schema.
-}
module Kafka.Streams.Serde.JsonSchema
  ( JsonSchemaEncoder (..)
  , JsonSchemaDecoder (..)
  , JsonSchemaSerdeConfig (..)
  , jsonSchemaSerde
  ) where

import Data.ByteString (ByteString)

import Kafka.Streams.Serde (Serde (..))
import qualified Kafka.Streams.Serde.SchemaRegistry as SR

newtype JsonSchemaEncoder a = JsonSchemaEncoder
  { runJsonSchemaEncoder :: a -> ByteString
  }

newtype JsonSchemaDecoder a = JsonSchemaDecoder
  { runJsonSchemaDecoder :: ByteString -> Either String a
  }

data JsonSchemaSerdeConfig a = JsonSchemaSerdeConfig
  { jssClient   :: !SR.SchemaRegistryClient
  , jssSubject  :: !SR.SchemaSubject
  , jssSchema   :: !SR.SchemaPayload
  , jssEncoder  :: !(JsonSchemaEncoder a)
  , jssDecoder  :: !(JsonSchemaDecoder a)
  }

jsonSchemaSerde :: JsonSchemaSerdeConfig a -> IO (Serde a)
jsonSchemaSerde JsonSchemaSerdeConfig{..} =
  SR.registrySerde SR.SchemaRegistrySerdeConfig
    { SR.srscClient  = jssClient
    , SR.srscSubject = jssSubject
    , SR.srscSchema  = jssSchema
    , SR.srscPayload = Serde
        { serialize   = runJsonSchemaEncoder jssEncoder
        , deserialize = runJsonSchemaDecoder jssDecoder
        }
    }
