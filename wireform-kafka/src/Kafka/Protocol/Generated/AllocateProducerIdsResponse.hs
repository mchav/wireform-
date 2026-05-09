{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AllocateProducerIdsResponse
Description : Kafka AllocateProducerIdsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 67.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AllocateProducerIdsResponse
  (
    AllocateProducerIdsResponse(..),
    encodeAllocateProducerIdsResponse,
    decodeAllocateProducerIdsResponse,
    maxAllocateProducerIdsResponseVersion
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




data AllocateProducerIdsResponse = AllocateProducerIdsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  allocateProducerIdsResponseThrottleTimeMs :: !(Int32)
,

  -- | The top level response error code.

  -- Versions: 0+
  allocateProducerIdsResponseErrorCode :: !(Int16)
,

  -- | The first producer ID in this range, inclusive.

  -- Versions: 0+
  allocateProducerIdsResponseProducerIdStart :: !(Int64)
,

  -- | The number of producer IDs in this range.

  -- Versions: 0+
  allocateProducerIdsResponseProducerIdLen :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AllocateProducerIdsResponse.
maxAllocateProducerIdsResponseVersion :: Int16
maxAllocateProducerIdsResponseVersion = 0

-- | Encode AllocateProducerIdsResponse with the given API version.
encodeAllocateProducerIdsResponse :: MonadPut m => E.ApiVersion -> AllocateProducerIdsResponse -> m ()
encodeAllocateProducerIdsResponse version msg
  | version == 0 =
    do
      serialize (allocateProducerIdsResponseThrottleTimeMs msg)
      serialize (allocateProducerIdsResponseErrorCode msg)
      serialize (allocateProducerIdsResponseProducerIdStart msg)
      serialize (allocateProducerIdsResponseProducerIdLen msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AllocateProducerIdsResponse with the given API version.
decodeAllocateProducerIdsResponse :: MonadGet m => E.ApiVersion -> m AllocateProducerIdsResponse
decodeAllocateProducerIdsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldproduceridstart <- deserialize
      fieldproduceridlen <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AllocateProducerIdsResponse
        {
        allocateProducerIdsResponseThrottleTimeMs = fieldthrottletimems
        ,
        allocateProducerIdsResponseErrorCode = fielderrorcode
        ,
        allocateProducerIdsResponseProducerIdStart = fieldproduceridstart
        ,
        allocateProducerIdsResponseProducerIdLen = fieldproduceridlen
        }
  | otherwise = fail $ "Unsupported version: " ++ show version