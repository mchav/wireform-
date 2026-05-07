{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.RequestHeader
Description : Kafka RequestHeader message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka header (no API key).



Valid versions: 1-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.RequestHeader
  (
    RequestHeader(..),
    encodeRequestHeader,
    decodeRequestHeader,
    maxRequestHeaderVersion
  ) where

import Control.Monad (when)
import Data.Bytes.Get (MonadGet)
import Data.Bytes.Put (MonadPut)
import Data.Bytes.Serial (Serial(..), serialize, deserialize)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( VarInt(..), VarLong(..), UVarInt(..)
  , KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , CompactString, CompactBytes, CompactArray
  , TaggedFields, emptyTaggedFields, Nullable(..)
  , toCompactString, toCompactBytes, toCompactArray
  )
import qualified Kafka.Protocol.Encoding as E




data RequestHeader = RequestHeader
  {

  -- | The API key of this request.

  -- Versions: 0+
  requestHeaderRequestApiKey :: !(Int16)
,

  -- | The API version of this request.

  -- Versions: 0+
  requestHeaderRequestApiVersion :: !(Int16)
,

  -- | The correlation ID of this request.

  -- Versions: 0+
  requestHeaderCorrelationId :: !(Int32)
,

  -- | The client ID string.

  -- Versions: 1+
  requestHeaderClientId :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for RequestHeader.
maxRequestHeaderVersion :: Int16
maxRequestHeaderVersion = 2

-- | Encode RequestHeader with the given API version.
encodeRequestHeader :: MonadPut m => E.ApiVersion -> RequestHeader -> m ()
encodeRequestHeader version msg
  | version == 1 =
    do
      serialize (requestHeaderRequestApiKey msg)
      serialize (requestHeaderRequestApiVersion msg)
      serialize (requestHeaderCorrelationId msg)
      serialize (requestHeaderClientId msg)


  | version == 2 =
    do
      serialize (requestHeaderRequestApiKey msg)
      serialize (requestHeaderRequestApiVersion msg)
      serialize (requestHeaderCorrelationId msg)
      serialize (toCompactString (requestHeaderClientId msg))
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode RequestHeader with the given API version.
decodeRequestHeader :: MonadGet m => E.ApiVersion -> m RequestHeader
decodeRequestHeader version
  | version == 1 =
    do
      fieldrequestapikey <- deserialize
      fieldrequestapiversion <- deserialize
      fieldcorrelationid <- deserialize
      fieldclientid <- deserialize
      pure RequestHeader
        {
        requestHeaderRequestApiKey = fieldrequestapikey
        ,
        requestHeaderRequestApiVersion = fieldrequestapiversion
        ,
        requestHeaderCorrelationId = fieldcorrelationid
        ,
        requestHeaderClientId = fieldclientid
        }

  | version == 2 =
    do
      fieldrequestapikey <- deserialize
      fieldrequestapiversion <- deserialize
      fieldcorrelationid <- deserialize
      fieldclientid <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure RequestHeader
        {
        requestHeaderRequestApiKey = fieldrequestapikey
        ,
        requestHeaderRequestApiVersion = fieldrequestapiversion
        ,
        requestHeaderCorrelationId = fieldcorrelationid
        ,
        requestHeaderClientId = fieldclientid
        }
  | otherwise = fail $ "Unsupported version: " ++ show version