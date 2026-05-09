{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AllocateProducerIdsRequest
Description : Kafka AllocateProducerIdsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 67.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AllocateProducerIdsRequest
  (
    AllocateProducerIdsRequest(..),
    encodeAllocateProducerIdsRequest,
    decodeAllocateProducerIdsRequest,
    maxAllocateProducerIdsRequestVersion
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
import qualified Kafka.Protocol.Wire.Codec as WC




data AllocateProducerIdsRequest = AllocateProducerIdsRequest
  {

  -- | The ID of the requesting broker.

  -- Versions: 0+
  allocateProducerIdsRequestBrokerId :: !(Int32)
,

  -- | The epoch of the requesting broker.

  -- Versions: 0+
  allocateProducerIdsRequestBrokerEpoch :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AllocateProducerIdsRequest.
maxAllocateProducerIdsRequestVersion :: Int16
maxAllocateProducerIdsRequestVersion = 0

-- | Encode AllocateProducerIdsRequest with the given API version.
encodeAllocateProducerIdsRequest :: MonadPut m => E.ApiVersion -> AllocateProducerIdsRequest -> m ()
encodeAllocateProducerIdsRequest version msg
  | version == 0 =
    do
      serialize (allocateProducerIdsRequestBrokerId msg)
      serialize (allocateProducerIdsRequestBrokerEpoch msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AllocateProducerIdsRequest with the given API version.
decodeAllocateProducerIdsRequest :: MonadGet m => E.ApiVersion -> m AllocateProducerIdsRequest
decodeAllocateProducerIdsRequest version
  | version == 0 =
    do
      fieldbrokerid <- deserialize
      fieldbrokerepoch <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AllocateProducerIdsRequest
        {
        allocateProducerIdsRequestBrokerId = fieldbrokerid
        ,
        allocateProducerIdsRequestBrokerEpoch = fieldbrokerepoch
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeAllocateProducerIdsRequest' / 'decodeAllocateProducerIdsRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec AllocateProducerIdsRequest where
  wireCodec = Just (WC.serialShimCodec encodeAllocateProducerIdsRequest decodeAllocateProducerIdsRequest)
  {-# INLINE wireCodec #-}
