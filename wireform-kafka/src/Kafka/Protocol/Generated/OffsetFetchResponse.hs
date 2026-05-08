{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetFetchResponse
Description : Kafka OffsetFetchResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 9.



Valid versions: 1-10
Flexible versions: 6+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetFetchResponse
  (
    OffsetFetchResponse(..),
    OffsetFetchResponseTopic(..),
    OffsetFetchResponsePartition(..),
    OffsetFetchResponseGroup(..),
    OffsetFetchResponseTopics(..),
    OffsetFetchResponsePartitions(..),
    encodeOffsetFetchResponse,
    decodeOffsetFetchResponse,
    maxOffsetFetchResponseVersion
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


-- | The responses per partition.
data OffsetFetchResponsePartition = OffsetFetchResponsePartition
  {

  -- | The partition index.

  -- Versions: 0-7
  offsetFetchResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The committed message offset.

  -- Versions: 0-7
  offsetFetchResponsePartitionCommittedOffset :: !(Int64)
,

  -- | The leader epoch.

  -- Versions: 5-7
  offsetFetchResponsePartitionCommittedLeaderEpoch :: !(Int32)
,

  -- | The partition metadata.

  -- Versions: 0-7
  offsetFetchResponsePartitionMetadata :: !(KafkaString)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0-7
  offsetFetchResponsePartitionErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetFetchResponsePartition with version-aware field handling.
encodeOffsetFetchResponsePartition :: MonadPut m => E.ApiVersion -> OffsetFetchResponsePartition -> m ()
encodeOffsetFetchResponsePartition version omsg =
  do
    when (version >= 0 && version <= 7) $
      serialize (offsetFetchResponsePartitionPartitionIndex omsg)
    when (version >= 0 && version <= 7) $
      serialize (offsetFetchResponsePartitionCommittedOffset omsg)
    when (version >= 5 && version <= 7) $
      serialize (offsetFetchResponsePartitionCommittedLeaderEpoch omsg)
    when (version >= 0 && version <= 7) $
      if version >= 6 then serialize (toCompactString (offsetFetchResponsePartitionMetadata omsg)) else serialize (offsetFetchResponsePartitionMetadata omsg)
    when (version >= 0 && version <= 7) $
      serialize (offsetFetchResponsePartitionErrorCode omsg)
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchResponsePartition with version-aware field handling.
decodeOffsetFetchResponsePartition :: MonadGet m => E.ApiVersion -> m OffsetFetchResponsePartition
decodeOffsetFetchResponsePartition version =
  do
    fieldpartitionindex <- if version >= 0 && version <= 7
      then deserialize
      else pure (0)
    fieldcommittedoffset <- if version >= 0 && version <= 7
      then deserialize
      else pure (0)
    fieldcommittedleaderepoch <- if version >= 5 && version <= 7
      then deserialize
      else pure ((-1))
    fieldmetadata <- if version >= 0 && version <= 7
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fielderrorcode <- if version >= 0 && version <= 7
      then deserialize
      else pure (0)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchResponsePartition
      {
      offsetFetchResponsePartitionPartitionIndex = fieldpartitionindex
      ,
      offsetFetchResponsePartitionCommittedOffset = fieldcommittedoffset
      ,
      offsetFetchResponsePartitionCommittedLeaderEpoch = fieldcommittedleaderepoch
      ,
      offsetFetchResponsePartitionMetadata = fieldmetadata
      ,
      offsetFetchResponsePartitionErrorCode = fielderrorcode
      }


-- | The responses per topic.
data OffsetFetchResponseTopic = OffsetFetchResponseTopic
  {

  -- | The topic name.

  -- Versions: 0-7
  offsetFetchResponseTopicName :: !(KafkaString)
,

  -- | The responses per partition.

  -- Versions: 0-7
  offsetFetchResponseTopicPartitions :: !(KafkaArray (OffsetFetchResponsePartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetFetchResponseTopic with version-aware field handling.
encodeOffsetFetchResponseTopic :: MonadPut m => E.ApiVersion -> OffsetFetchResponseTopic -> m ()
encodeOffsetFetchResponseTopic version omsg =
  do
    when (version >= 0 && version <= 7) $
      if version >= 6 then serialize (toCompactString (offsetFetchResponseTopicName omsg)) else serialize (offsetFetchResponseTopicName omsg)
    when (version >= 0 && version <= 7) $
      E.encodeVersionedArray version 6 encodeOffsetFetchResponsePartition (case P.unKafkaArray (offsetFetchResponseTopicPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchResponseTopic with version-aware field handling.
decodeOffsetFetchResponseTopic :: MonadGet m => E.ApiVersion -> m OffsetFetchResponseTopic
decodeOffsetFetchResponseTopic version =
  do
    fieldname <- if version >= 0 && version <= 7
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldpartitions <- if version >= 0 && version <= 7
      then P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponsePartition
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchResponseTopic
      {
      offsetFetchResponseTopicName = fieldname
      ,
      offsetFetchResponseTopicPartitions = fieldpartitions
      }


-- | The responses per partition.
data OffsetFetchResponsePartitions = OffsetFetchResponsePartitions
  {

  -- | The partition index.

  -- Versions: 8+
  offsetFetchResponsePartitionsPartitionIndex :: !(Int32)
,

  -- | The committed message offset.

  -- Versions: 8+
  offsetFetchResponsePartitionsCommittedOffset :: !(Int64)
,

  -- | The leader epoch.

  -- Versions: 8+
  offsetFetchResponsePartitionsCommittedLeaderEpoch :: !(Int32)
,

  -- | The partition metadata.

  -- Versions: 8+
  offsetFetchResponsePartitionsMetadata :: !(KafkaString)
,

  -- | The partition-level error code, or 0 if there was no error.

  -- Versions: 8+
  offsetFetchResponsePartitionsErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetFetchResponsePartitions with version-aware field handling.
encodeOffsetFetchResponsePartitions :: MonadPut m => E.ApiVersion -> OffsetFetchResponsePartitions -> m ()
encodeOffsetFetchResponsePartitions version omsg =
  do
    when (version >= 8) $
      serialize (offsetFetchResponsePartitionsPartitionIndex omsg)
    when (version >= 8) $
      serialize (offsetFetchResponsePartitionsCommittedOffset omsg)
    when (version >= 8) $
      serialize (offsetFetchResponsePartitionsCommittedLeaderEpoch omsg)
    when (version >= 8) $
      if version >= 6 then serialize (toCompactString (offsetFetchResponsePartitionsMetadata omsg)) else serialize (offsetFetchResponsePartitionsMetadata omsg)
    when (version >= 8) $
      serialize (offsetFetchResponsePartitionsErrorCode omsg)
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchResponsePartitions with version-aware field handling.
decodeOffsetFetchResponsePartitions :: MonadGet m => E.ApiVersion -> m OffsetFetchResponsePartitions
decodeOffsetFetchResponsePartitions version =
  do
    fieldpartitionindex <- if version >= 8
      then deserialize
      else pure (0)
    fieldcommittedoffset <- if version >= 8
      then deserialize
      else pure (0)
    fieldcommittedleaderepoch <- if version >= 8
      then deserialize
      else pure ((-1))
    fieldmetadata <- if version >= 8
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fielderrorcode <- if version >= 8
      then deserialize
      else pure (0)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchResponsePartitions
      {
      offsetFetchResponsePartitionsPartitionIndex = fieldpartitionindex
      ,
      offsetFetchResponsePartitionsCommittedOffset = fieldcommittedoffset
      ,
      offsetFetchResponsePartitionsCommittedLeaderEpoch = fieldcommittedleaderepoch
      ,
      offsetFetchResponsePartitionsMetadata = fieldmetadata
      ,
      offsetFetchResponsePartitionsErrorCode = fielderrorcode
      }


-- | The responses per topic.
data OffsetFetchResponseTopics = OffsetFetchResponseTopics
  {

  -- | The topic name.

  -- Versions: 8-9
  offsetFetchResponseTopicsName :: !(KafkaString)
,

  -- | The topic ID.

  -- Versions: 10+
  offsetFetchResponseTopicsTopicId :: !(KafkaUuid)
,

  -- | The responses per partition.

  -- Versions: 8+
  offsetFetchResponseTopicsPartitions :: !(KafkaArray (OffsetFetchResponsePartitions))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetFetchResponseTopics with version-aware field handling.
encodeOffsetFetchResponseTopics :: MonadPut m => E.ApiVersion -> OffsetFetchResponseTopics -> m ()
encodeOffsetFetchResponseTopics version omsg =
  do
    when (version >= 8 && version <= 9) $
      if version >= 6 then serialize (toCompactString (offsetFetchResponseTopicsName omsg)) else serialize (offsetFetchResponseTopicsName omsg)
    when (version >= 10) $
      serialize (offsetFetchResponseTopicsTopicId omsg)
    when (version >= 8) $
      E.encodeVersionedArray version 6 encodeOffsetFetchResponsePartitions (case P.unKafkaArray (offsetFetchResponseTopicsPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchResponseTopics with version-aware field handling.
decodeOffsetFetchResponseTopics :: MonadGet m => E.ApiVersion -> m OffsetFetchResponseTopics
decodeOffsetFetchResponseTopics version =
  do
    fieldname <- if version >= 8 && version <= 9
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 10
      then deserialize
      else pure (P.nullUuid)
    fieldpartitions <- if version >= 8
      then P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponsePartitions
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchResponseTopics
      {
      offsetFetchResponseTopicsName = fieldname
      ,
      offsetFetchResponseTopicsTopicId = fieldtopicid
      ,
      offsetFetchResponseTopicsPartitions = fieldpartitions
      }


-- | The responses per group id.
data OffsetFetchResponseGroup = OffsetFetchResponseGroup
  {

  -- | The group ID.

  -- Versions: 8+
  offsetFetchResponseGroupGroupId :: !(KafkaString)
,

  -- | The responses per topic.

  -- Versions: 8+
  offsetFetchResponseGroupTopics :: !(KafkaArray (OffsetFetchResponseTopics))
,

  -- | The group-level error code, or 0 if there was no error.

  -- Versions: 8+
  offsetFetchResponseGroupErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetFetchResponseGroup with version-aware field handling.
encodeOffsetFetchResponseGroup :: MonadPut m => E.ApiVersion -> OffsetFetchResponseGroup -> m ()
encodeOffsetFetchResponseGroup version omsg =
  do
    when (version >= 8) $
      if version >= 6 then serialize (toCompactString (offsetFetchResponseGroupGroupId omsg)) else serialize (offsetFetchResponseGroupGroupId omsg)
    when (version >= 8) $
      E.encodeVersionedArray version 6 encodeOffsetFetchResponseTopics (case P.unKafkaArray (offsetFetchResponseGroupTopics omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 8) $
      serialize (offsetFetchResponseGroupErrorCode omsg)
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchResponseGroup with version-aware field handling.
decodeOffsetFetchResponseGroup :: MonadGet m => E.ApiVersion -> m OffsetFetchResponseGroup
decodeOffsetFetchResponseGroup version =
  do
    fieldgroupid <- if version >= 8
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopics <- if version >= 8
      then P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponseTopics
      else pure (P.mkKafkaArray V.empty)
    fielderrorcode <- if version >= 8
      then deserialize
      else pure (0)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchResponseGroup
      {
      offsetFetchResponseGroupGroupId = fieldgroupid
      ,
      offsetFetchResponseGroupTopics = fieldtopics
      ,
      offsetFetchResponseGroupErrorCode = fielderrorcode
      }



data OffsetFetchResponse = OffsetFetchResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 3+
  offsetFetchResponseThrottleTimeMs :: !(Int32)
,

  -- | The responses per topic.

  -- Versions: 0-7
  offsetFetchResponseTopics :: !(KafkaArray (OffsetFetchResponseTopic))
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 2-7
  offsetFetchResponseErrorCode :: !(Int16)
,

  -- | The responses per group id.

  -- Versions: 8+
  offsetFetchResponseGroups :: !(KafkaArray (OffsetFetchResponseGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetFetchResponse.
maxOffsetFetchResponseVersion :: Int16
maxOffsetFetchResponseVersion = 10

-- | Encode OffsetFetchResponse with the given API version.
encodeOffsetFetchResponse :: MonadPut m => E.ApiVersion -> OffsetFetchResponse -> m ()
encodeOffsetFetchResponse version msg
  | version == 1 =
    do
      E.encodeVersionedArray version 6 encodeOffsetFetchResponseTopic (case P.unKafkaArray (offsetFetchResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 2 =
    do
      E.encodeVersionedArray version 6 encodeOffsetFetchResponseTopic (case P.unKafkaArray (offsetFetchResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (offsetFetchResponseErrorCode msg)


  | version >= 6 && version <= 7 =
    do
      serialize (offsetFetchResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 6 encodeOffsetFetchResponseTopic (case P.unKafkaArray (offsetFetchResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (offsetFetchResponseErrorCode msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 3 && version <= 5 =
    do
      serialize (offsetFetchResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 6 encodeOffsetFetchResponseTopic (case P.unKafkaArray (offsetFetchResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (offsetFetchResponseErrorCode msg)


  | version >= 8 && version <= 10 =
    do
      serialize (offsetFetchResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 6 encodeOffsetFetchResponseGroup (case P.unKafkaArray (offsetFetchResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetFetchResponse with the given API version.
decodeOffsetFetchResponse :: MonadGet m => E.ApiVersion -> m OffsetFetchResponse
decodeOffsetFetchResponse version
  | version == 1 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponseTopic
      pure OffsetFetchResponse
        {
        offsetFetchResponseThrottleTimeMs = 0
        ,
        offsetFetchResponseTopics = fieldtopics
        ,
        offsetFetchResponseErrorCode = 0
        ,
        offsetFetchResponseGroups = P.mkKafkaArray V.empty
        }

  | version == 2 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponseTopic
      fielderrorcode <- deserialize
      pure OffsetFetchResponse
        {
        offsetFetchResponseThrottleTimeMs = 0
        ,
        offsetFetchResponseTopics = fieldtopics
        ,
        offsetFetchResponseErrorCode = fielderrorcode
        ,
        offsetFetchResponseGroups = P.mkKafkaArray V.empty
        }

  | version >= 6 && version <= 7 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponseTopic
      fielderrorcode <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetFetchResponse
        {
        offsetFetchResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetFetchResponseTopics = fieldtopics
        ,
        offsetFetchResponseErrorCode = fielderrorcode
        ,
        offsetFetchResponseGroups = P.mkKafkaArray V.empty
        }

  | version >= 3 && version <= 5 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponseTopic
      fielderrorcode <- deserialize
      pure OffsetFetchResponse
        {
        offsetFetchResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetFetchResponseTopics = fieldtopics
        ,
        offsetFetchResponseErrorCode = fielderrorcode
        ,
        offsetFetchResponseGroups = P.mkKafkaArray V.empty
        }

  | version >= 8 && version <= 10 =
    do
      fieldthrottletimems <- deserialize
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponseGroup
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetFetchResponse
        {
        offsetFetchResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetFetchResponseTopics = P.mkKafkaArray V.empty
        ,
        offsetFetchResponseErrorCode = 0
        ,
        offsetFetchResponseGroups = fieldgroups
        }
  | otherwise = fail $ "Unsupported version: " ++ show version