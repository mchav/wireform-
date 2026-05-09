{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeShareGroupOffsetsResponse
Description : Kafka DescribeShareGroupOffsetsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 90.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeShareGroupOffsetsResponse
  (
    DescribeShareGroupOffsetsResponse(..),
    DescribeShareGroupOffsetsResponseGroup(..),
    DescribeShareGroupOffsetsResponseTopic(..),
    DescribeShareGroupOffsetsResponsePartition(..),
    encodeDescribeShareGroupOffsetsResponse,
    decodeDescribeShareGroupOffsetsResponse,
    maxDescribeShareGroupOffsetsResponseVersion
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



data DescribeShareGroupOffsetsResponsePartition = DescribeShareGroupOffsetsResponsePartition
  {

  -- | The partition index.

  -- Versions: 0+
  describeShareGroupOffsetsResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The share-partition start offset.

  -- Versions: 0+
  describeShareGroupOffsetsResponsePartitionStartOffset :: !(Int64)
,

  -- | The leader epoch of the partition.

  -- Versions: 0+
  describeShareGroupOffsetsResponsePartitionLeaderEpoch :: !(Int32)
,

  -- | The partition-level error code, or 0 if there was no error.

  -- Versions: 0+
  describeShareGroupOffsetsResponsePartitionErrorCode :: !(Int16)
,

  -- | The partition-level error message, or null if there was no error.

  -- Versions: 0+
  describeShareGroupOffsetsResponsePartitionErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeShareGroupOffsetsResponsePartition with version-aware field handling.
encodeDescribeShareGroupOffsetsResponsePartition :: MonadPut m => E.ApiVersion -> DescribeShareGroupOffsetsResponsePartition -> m ()
encodeDescribeShareGroupOffsetsResponsePartition version dmsg =
  do
    serialize (describeShareGroupOffsetsResponsePartitionPartitionIndex dmsg)
    serialize (describeShareGroupOffsetsResponsePartitionStartOffset dmsg)
    serialize (describeShareGroupOffsetsResponsePartitionLeaderEpoch dmsg)
    serialize (describeShareGroupOffsetsResponsePartitionErrorCode dmsg)
    if version >= 0 then serialize (toCompactString (describeShareGroupOffsetsResponsePartitionErrorMessage dmsg)) else serialize (describeShareGroupOffsetsResponsePartitionErrorMessage dmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeShareGroupOffsetsResponsePartition with version-aware field handling.
decodeDescribeShareGroupOffsetsResponsePartition :: MonadGet m => E.ApiVersion -> m DescribeShareGroupOffsetsResponsePartition
decodeDescribeShareGroupOffsetsResponsePartition version =
  do
    fieldpartitionindex <- deserialize
    fieldstartoffset <- deserialize
    fieldleaderepoch <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeShareGroupOffsetsResponsePartition
      {
      describeShareGroupOffsetsResponsePartitionPartitionIndex = fieldpartitionindex
      ,
      describeShareGroupOffsetsResponsePartitionStartOffset = fieldstartoffset
      ,
      describeShareGroupOffsetsResponsePartitionLeaderEpoch = fieldleaderepoch
      ,
      describeShareGroupOffsetsResponsePartitionErrorCode = fielderrorcode
      ,
      describeShareGroupOffsetsResponsePartitionErrorMessage = fielderrormessage
      }


-- | The results for each topic.
data DescribeShareGroupOffsetsResponseTopic = DescribeShareGroupOffsetsResponseTopic
  {

  -- | The topic name.

  -- Versions: 0+
  describeShareGroupOffsetsResponseTopicTopicName :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 0+
  describeShareGroupOffsetsResponseTopicTopicId :: !(KafkaUuid)
,


  -- Versions: 0+
  describeShareGroupOffsetsResponseTopicPartitions :: !(KafkaArray (DescribeShareGroupOffsetsResponsePartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeShareGroupOffsetsResponseTopic with version-aware field handling.
encodeDescribeShareGroupOffsetsResponseTopic :: MonadPut m => E.ApiVersion -> DescribeShareGroupOffsetsResponseTopic -> m ()
encodeDescribeShareGroupOffsetsResponseTopic version dmsg =
  do
    if version >= 0 then serialize (toCompactString (describeShareGroupOffsetsResponseTopicTopicName dmsg)) else serialize (describeShareGroupOffsetsResponseTopicTopicName dmsg)
    serialize (describeShareGroupOffsetsResponseTopicTopicId dmsg)
    E.encodeVersionedArray version 0 encodeDescribeShareGroupOffsetsResponsePartition (case P.unKafkaArray (describeShareGroupOffsetsResponseTopicPartitions dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeShareGroupOffsetsResponseTopic with version-aware field handling.
decodeDescribeShareGroupOffsetsResponseTopic :: MonadGet m => E.ApiVersion -> m DescribeShareGroupOffsetsResponseTopic
decodeDescribeShareGroupOffsetsResponseTopic version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDescribeShareGroupOffsetsResponsePartition
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeShareGroupOffsetsResponseTopic
      {
      describeShareGroupOffsetsResponseTopicTopicName = fieldtopicname
      ,
      describeShareGroupOffsetsResponseTopicTopicId = fieldtopicid
      ,
      describeShareGroupOffsetsResponseTopicPartitions = fieldpartitions
      }


-- | The results for each group.
data DescribeShareGroupOffsetsResponseGroup = DescribeShareGroupOffsetsResponseGroup
  {

  -- | The group identifier.

  -- Versions: 0+
  describeShareGroupOffsetsResponseGroupGroupId :: !(KafkaString)
,

  -- | The results for each topic.

  -- Versions: 0+
  describeShareGroupOffsetsResponseGroupTopics :: !(KafkaArray (DescribeShareGroupOffsetsResponseTopic))
,

  -- | The group-level error code, or 0 if there was no error.

  -- Versions: 0+
  describeShareGroupOffsetsResponseGroupErrorCode :: !(Int16)
,

  -- | The group-level error message, or null if there was no error.

  -- Versions: 0+
  describeShareGroupOffsetsResponseGroupErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeShareGroupOffsetsResponseGroup with version-aware field handling.
encodeDescribeShareGroupOffsetsResponseGroup :: MonadPut m => E.ApiVersion -> DescribeShareGroupOffsetsResponseGroup -> m ()
encodeDescribeShareGroupOffsetsResponseGroup version dmsg =
  do
    if version >= 0 then serialize (toCompactString (describeShareGroupOffsetsResponseGroupGroupId dmsg)) else serialize (describeShareGroupOffsetsResponseGroupGroupId dmsg)
    E.encodeVersionedArray version 0 encodeDescribeShareGroupOffsetsResponseTopic (case P.unKafkaArray (describeShareGroupOffsetsResponseGroupTopics dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    serialize (describeShareGroupOffsetsResponseGroupErrorCode dmsg)
    if version >= 0 then serialize (toCompactString (describeShareGroupOffsetsResponseGroupErrorMessage dmsg)) else serialize (describeShareGroupOffsetsResponseGroupErrorMessage dmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeShareGroupOffsetsResponseGroup with version-aware field handling.
decodeDescribeShareGroupOffsetsResponseGroup :: MonadGet m => E.ApiVersion -> m DescribeShareGroupOffsetsResponseGroup
decodeDescribeShareGroupOffsetsResponseGroup version =
  do
    fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDescribeShareGroupOffsetsResponseTopic
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeShareGroupOffsetsResponseGroup
      {
      describeShareGroupOffsetsResponseGroupGroupId = fieldgroupid
      ,
      describeShareGroupOffsetsResponseGroupTopics = fieldtopics
      ,
      describeShareGroupOffsetsResponseGroupErrorCode = fielderrorcode
      ,
      describeShareGroupOffsetsResponseGroupErrorMessage = fielderrormessage
      }



data DescribeShareGroupOffsetsResponse = DescribeShareGroupOffsetsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeShareGroupOffsetsResponseThrottleTimeMs :: !(Int32)
,

  -- | The results for each group.

  -- Versions: 0+
  describeShareGroupOffsetsResponseGroups :: !(KafkaArray (DescribeShareGroupOffsetsResponseGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeShareGroupOffsetsResponse.
maxDescribeShareGroupOffsetsResponseVersion :: Int16
maxDescribeShareGroupOffsetsResponseVersion = 0

-- | Encode DescribeShareGroupOffsetsResponse with the given API version.
encodeDescribeShareGroupOffsetsResponse :: MonadPut m => E.ApiVersion -> DescribeShareGroupOffsetsResponse -> m ()
encodeDescribeShareGroupOffsetsResponse version msg
  | version == 0 =
    do
      serialize (describeShareGroupOffsetsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 0 encodeDescribeShareGroupOffsetsResponseGroup (case P.unKafkaArray (describeShareGroupOffsetsResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeShareGroupOffsetsResponse with the given API version.
decodeDescribeShareGroupOffsetsResponse :: MonadGet m => E.ApiVersion -> m DescribeShareGroupOffsetsResponse
decodeDescribeShareGroupOffsetsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDescribeShareGroupOffsetsResponseGroup
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeShareGroupOffsetsResponse
        {
        describeShareGroupOffsetsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeShareGroupOffsetsResponseGroups = fieldgroups
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeDescribeShareGroupOffsetsResponse' / 'decodeDescribeShareGroupOffsetsResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec DescribeShareGroupOffsetsResponse where
  wireCodec = Just (WC.serialShimCodec encodeDescribeShareGroupOffsetsResponse decodeDescribeShareGroupOffsetsResponse)
  {-# INLINE wireCodec #-}
