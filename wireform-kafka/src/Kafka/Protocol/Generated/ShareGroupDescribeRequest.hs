{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ShareGroupDescribeRequest
Description : Kafka ShareGroupDescribeRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 77.



Valid versions: 1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ShareGroupDescribeRequest
  (
    ShareGroupDescribeRequest(..),
    encodeShareGroupDescribeRequest,
    decodeShareGroupDescribeRequest,
    maxShareGroupDescribeRequestVersion
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




data ShareGroupDescribeRequest = ShareGroupDescribeRequest
  {

  -- | The ids of the groups to describe.

  -- Versions: 0+
  shareGroupDescribeRequestGroupIds :: !(KafkaArray (KafkaString))
,

  -- | Whether to include authorized operations.

  -- Versions: 0+
  shareGroupDescribeRequestIncludeAuthorizedOperations :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ShareGroupDescribeRequest.
maxShareGroupDescribeRequestVersion :: Int16
maxShareGroupDescribeRequestVersion = 1

-- | Encode ShareGroupDescribeRequest with the given API version.
encodeShareGroupDescribeRequest :: MonadPut m => E.ApiVersion -> ShareGroupDescribeRequest -> m ()
encodeShareGroupDescribeRequest version msg
  | version == 1 =
    do
      E.encodeVersionedArray version 0 (\v s -> if v >= 0 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (shareGroupDescribeRequestGroupIds msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (shareGroupDescribeRequestIncludeAuthorizedOperations msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ShareGroupDescribeRequest with the given API version.
decodeShareGroupDescribeRequest :: MonadGet m => E.ApiVersion -> m ShareGroupDescribeRequest
decodeShareGroupDescribeRequest version
  | version == 1 =
    do
      fieldgroupids <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\v -> if v >= 0 then P.fromCompactString <$> deserialize else deserialize)
      fieldincludeauthorizedoperations <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ShareGroupDescribeRequest
        {
        shareGroupDescribeRequestGroupIds = fieldgroupids
        ,
        shareGroupDescribeRequestIncludeAuthorizedOperations = fieldincludeauthorizedoperations
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeShareGroupDescribeRequest' / 'decodeShareGroupDescribeRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec ShareGroupDescribeRequest where
  wireCodec = Just (WC.serialShimCodec encodeShareGroupDescribeRequest decodeShareGroupDescribeRequest)
  {-# INLINE wireCodec #-}
