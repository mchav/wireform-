{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetForLeaderEpochResponse
Description : Kafka OffsetForLeaderEpochResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 23.



Valid versions: 2-4
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetForLeaderEpochResponse
  (
    OffsetForLeaderEpochResponse(..),
    OffsetForLeaderTopicResult(..),
    EpochEndOffset(..),
    encodeOffsetForLeaderEpochResponse,
    decodeOffsetForLeaderEpochResponse,
    maxOffsetForLeaderEpochResponseVersion
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


-- | Each partition in the topic we fetched offsets for.
data EpochEndOffset = EpochEndOffset
  {

  -- | The error code 0, or if there was no error.

  -- Versions: 0+
  epochEndOffsetErrorCode :: !(Int16)
,

  -- | The partition index.

  -- Versions: 0+
  epochEndOffsetPartition :: !(Int32)
,

  -- | The leader epoch of the partition.

  -- Versions: 1+
  epochEndOffsetLeaderEpoch :: !(Int32)
,

  -- | The end offset of the epoch.

  -- Versions: 0+
  epochEndOffsetEndOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode EpochEndOffset with version-aware field handling.
encodeEpochEndOffset :: MonadPut m => E.ApiVersion -> EpochEndOffset -> m ()
encodeEpochEndOffset version emsg =
  do
    serialize (epochEndOffsetErrorCode emsg)
    serialize (epochEndOffsetPartition emsg)
    when (version >= 1) $
      serialize (epochEndOffsetLeaderEpoch emsg)
    serialize (epochEndOffsetEndOffset emsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode EpochEndOffset with version-aware field handling.
decodeEpochEndOffset :: MonadGet m => E.ApiVersion -> m EpochEndOffset
decodeEpochEndOffset version =
  do
    fielderrorcode <- deserialize
    fieldpartition <- deserialize
    fieldleaderepoch <- if version >= 1
      then deserialize
      else pure ((-1))
    fieldendoffset <- deserialize
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure EpochEndOffset
      {
      epochEndOffsetErrorCode = fielderrorcode
      ,
      epochEndOffsetPartition = fieldpartition
      ,
      epochEndOffsetLeaderEpoch = fieldleaderepoch
      ,
      epochEndOffsetEndOffset = fieldendoffset
      }


-- | Each topic we fetched offsets for.
data OffsetForLeaderTopicResult = OffsetForLeaderTopicResult
  {

  -- | The topic name.

  -- Versions: 0+
  offsetForLeaderTopicResultTopic :: !(KafkaString)
,

  -- | Each partition in the topic we fetched offsets for.

  -- Versions: 0+
  offsetForLeaderTopicResultPartitions :: !(KafkaArray (EpochEndOffset))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetForLeaderTopicResult with version-aware field handling.
encodeOffsetForLeaderTopicResult :: MonadPut m => E.ApiVersion -> OffsetForLeaderTopicResult -> m ()
encodeOffsetForLeaderTopicResult version omsg =
  do
    if version >= 4 then serialize (toCompactString (offsetForLeaderTopicResultTopic omsg)) else serialize (offsetForLeaderTopicResultTopic omsg)
    E.encodeVersionedArray version 4 encodeEpochEndOffset (case P.unKafkaArray (offsetForLeaderTopicResultPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetForLeaderTopicResult with version-aware field handling.
decodeOffsetForLeaderTopicResult :: MonadGet m => E.ApiVersion -> m OffsetForLeaderTopicResult
decodeOffsetForLeaderTopicResult version =
  do
    fieldtopic <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeEpochEndOffset
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetForLeaderTopicResult
      {
      offsetForLeaderTopicResultTopic = fieldtopic
      ,
      offsetForLeaderTopicResultPartitions = fieldpartitions
      }



data OffsetForLeaderEpochResponse = OffsetForLeaderEpochResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 2+
  offsetForLeaderEpochResponseThrottleTimeMs :: !(Int32)
,

  -- | Each topic we fetched offsets for.

  -- Versions: 0+
  offsetForLeaderEpochResponseTopics :: !(KafkaArray (OffsetForLeaderTopicResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetForLeaderEpochResponse.
maxOffsetForLeaderEpochResponseVersion :: Int16
maxOffsetForLeaderEpochResponseVersion = 4

-- | Encode OffsetForLeaderEpochResponse with the given API version.
encodeOffsetForLeaderEpochResponse :: MonadPut m => E.ApiVersion -> OffsetForLeaderEpochResponse -> m ()
encodeOffsetForLeaderEpochResponse version msg
  | version == 4 =
    do
      serialize (offsetForLeaderEpochResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 4 encodeOffsetForLeaderTopicResult (case P.unKafkaArray (offsetForLeaderEpochResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 2 && version <= 3 =
    do
      serialize (offsetForLeaderEpochResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 4 encodeOffsetForLeaderTopicResult (case P.unKafkaArray (offsetForLeaderEpochResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetForLeaderEpochResponse with the given API version.
decodeOffsetForLeaderEpochResponse :: MonadGet m => E.ApiVersion -> m OffsetForLeaderEpochResponse
decodeOffsetForLeaderEpochResponse version
  | version == 4 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeOffsetForLeaderTopicResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetForLeaderEpochResponse
        {
        offsetForLeaderEpochResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetForLeaderEpochResponseTopics = fieldtopics
        }

  | version >= 2 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeOffsetForLeaderTopicResult
      pure OffsetForLeaderEpochResponse
        {
        offsetForLeaderEpochResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetForLeaderEpochResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeOffsetForLeaderEpochResponse' / 'decodeOffsetForLeaderEpochResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec OffsetForLeaderEpochResponse where
  wireCodec = Just (WC.serialShimCodec encodeOffsetForLeaderEpochResponse decodeOffsetForLeaderEpochResponse)
  {-# INLINE wireCodec #-}
