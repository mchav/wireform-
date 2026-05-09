{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ConsumerGroupDescribeRequest
Description : Kafka ConsumerGroupDescribeRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 69.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ConsumerGroupDescribeRequest
  (
    ConsumerGroupDescribeRequest(..),
    encodeConsumerGroupDescribeRequest,
    decodeConsumerGroupDescribeRequest,
    maxConsumerGroupDescribeRequestVersion
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




data ConsumerGroupDescribeRequest = ConsumerGroupDescribeRequest
  {

  -- | The ids of the groups to describe.

  -- Versions: 0+
  consumerGroupDescribeRequestGroupIds :: !(KafkaArray (KafkaString))
,

  -- | Whether to include authorized operations.

  -- Versions: 0+
  consumerGroupDescribeRequestIncludeAuthorizedOperations :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ConsumerGroupDescribeRequest.
maxConsumerGroupDescribeRequestVersion :: Int16
maxConsumerGroupDescribeRequestVersion = 1

-- | Encode ConsumerGroupDescribeRequest with the given API version.
encodeConsumerGroupDescribeRequest :: MonadPut m => E.ApiVersion -> ConsumerGroupDescribeRequest -> m ()
encodeConsumerGroupDescribeRequest version msg
  | version >= 0 && version <= 1 =
    do
      E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (consumerGroupDescribeRequestGroupIds msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (consumerGroupDescribeRequestIncludeAuthorizedOperations msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ConsumerGroupDescribeRequest with the given API version.
decodeConsumerGroupDescribeRequest :: MonadGet m => E.ApiVersion -> m ConsumerGroupDescribeRequest
decodeConsumerGroupDescribeRequest version
  | version >= 0 && version <= 1 =
    do
      fieldgroupids <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
      fieldincludeauthorizedoperations <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ConsumerGroupDescribeRequest
        {
        consumerGroupDescribeRequestGroupIds = fieldgroupids
        ,
        consumerGroupDescribeRequestIncludeAuthorizedOperations = fieldincludeauthorizedoperations
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeConsumerGroupDescribeRequest' / 'decodeConsumerGroupDescribeRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec ConsumerGroupDescribeRequest where
  wireCodec = Just (WC.serialShimCodec encodeConsumerGroupDescribeRequest decodeConsumerGroupDescribeRequest)
  {-# INLINE wireCodec #-}
