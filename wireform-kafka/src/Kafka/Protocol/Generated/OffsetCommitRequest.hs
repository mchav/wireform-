{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetCommitRequest
Description : Kafka OffsetCommitRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 8.



Valid versions: 2-9
Flexible versions: 8+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetCommitRequest
  (
    OffsetCommitRequest(..),
    OffsetCommitRequestTopic(..),
    OffsetCommitRequestPartition(..),
    encodeOffsetCommitRequest,
    decodeOffsetCommitRequest,
    maxOffsetCommitRequestVersion
  ) where

import Control.Monad (when)
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
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
import Kafka.Protocol.Message (KafkaMessage(..))


-- | Each partition to commit offsets for.
data OffsetCommitRequestPartition = OffsetCommitRequestPartition
  {

  -- | The partition index.

  -- Versions: 0+
  offsetCommitRequestPartitionPartitionIndex :: !(Int32)
,

  -- | The message offset to be committed.

  -- Versions: 0+
  offsetCommitRequestPartitionCommittedOffset :: !(Int64)
,

  -- | The leader epoch of this partition.

  -- Versions: 6+
  offsetCommitRequestPartitionCommittedLeaderEpoch :: !(Int32)
,

  -- | Any associated metadata the client wants to keep.

  -- Versions: 0+
  offsetCommitRequestPartitionCommittedMetadata :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetCommitRequestPartition with version-aware field handling.
encodeOffsetCommitRequestPartition :: MonadPut m => E.ApiVersion -> OffsetCommitRequestPartition -> m ()
encodeOffsetCommitRequestPartition version omsg =
  do
    serialize (offsetCommitRequestPartitionPartitionIndex omsg)
    serialize (offsetCommitRequestPartitionCommittedOffset omsg)
    when (version >= 6) $
      serialize (offsetCommitRequestPartitionCommittedLeaderEpoch omsg)
    if version >= 8 then serialize (toCompactString (offsetCommitRequestPartitionCommittedMetadata omsg)) else serialize (offsetCommitRequestPartitionCommittedMetadata omsg)
    when (version >= 8) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetCommitRequestPartition with version-aware field handling.
decodeOffsetCommitRequestPartition :: MonadGet m => E.ApiVersion -> m OffsetCommitRequestPartition
decodeOffsetCommitRequestPartition version =
  do
    fieldpartitionindex <- deserialize
    fieldcommittedoffset <- deserialize
    fieldcommittedleaderepoch <- if version >= 6
      then deserialize
      else pure ((-1))
    fieldcommittedmetadata <- if version >= 8 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 8 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetCommitRequestPartition
      {
      offsetCommitRequestPartitionPartitionIndex = fieldpartitionindex
      ,
      offsetCommitRequestPartitionCommittedOffset = fieldcommittedoffset
      ,
      offsetCommitRequestPartitionCommittedLeaderEpoch = fieldcommittedleaderepoch
      ,
      offsetCommitRequestPartitionCommittedMetadata = fieldcommittedmetadata
      }


-- | The topics to commit offsets for.
data OffsetCommitRequestTopic = OffsetCommitRequestTopic
  {

  -- | The topic name.

  -- Versions: 0+
  offsetCommitRequestTopicName :: !(KafkaString)
,

  -- | Each partition to commit offsets for.

  -- Versions: 0+
  offsetCommitRequestTopicPartitions :: !(KafkaArray (OffsetCommitRequestPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetCommitRequestTopic with version-aware field handling.
encodeOffsetCommitRequestTopic :: MonadPut m => E.ApiVersion -> OffsetCommitRequestTopic -> m ()
encodeOffsetCommitRequestTopic version omsg =
  do
    if version >= 8 then serialize (toCompactString (offsetCommitRequestTopicName omsg)) else serialize (offsetCommitRequestTopicName omsg)
    E.encodeVersionedArray version 8 encodeOffsetCommitRequestPartition (case P.unKafkaArray (offsetCommitRequestTopicPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 8) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetCommitRequestTopic with version-aware field handling.
decodeOffsetCommitRequestTopic :: MonadGet m => E.ApiVersion -> m OffsetCommitRequestTopic
decodeOffsetCommitRequestTopic version =
  do
    fieldname <- if version >= 8 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 8 decodeOffsetCommitRequestPartition
    _ <- if version >= 8 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetCommitRequestTopic
      {
      offsetCommitRequestTopicName = fieldname
      ,
      offsetCommitRequestTopicPartitions = fieldpartitions
      }



data OffsetCommitRequest = OffsetCommitRequest
  {

  -- | The unique group identifier.

  -- Versions: 0+
  offsetCommitRequestGroupId :: !(KafkaString)
,

  -- | The generation of the group if using the classic group protocol or the member epoch if using the con

  -- Versions: 1+
  offsetCommitRequestGenerationIdOrMemberEpoch :: !(Int32)
,

  -- | The member ID assigned by the group coordinator.

  -- Versions: 1+
  offsetCommitRequestMemberId :: !(KafkaString)
,

  -- | The unique identifier of the consumer instance provided by end user.

  -- Versions: 7+
  offsetCommitRequestGroupInstanceId :: !(KafkaString)
,

  -- | The time period in ms to retain the offset.

  -- Versions: 2-4
  offsetCommitRequestRetentionTimeMs :: !(Int64)
,

  -- | The topics to commit offsets for.

  -- Versions: 0+
  offsetCommitRequestTopics :: !(KafkaArray (OffsetCommitRequestTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetCommitRequest.
maxOffsetCommitRequestVersion :: Int16
maxOffsetCommitRequestVersion = 9

-- | KafkaMessage instance for OffsetCommitRequest.
instance KafkaMessage OffsetCommitRequest where
  messageApiKey = 8
  messageMinVersion = 2
  messageMaxVersion = 9
  messageFlexibleVersion = Just 8

-- | Encode OffsetCommitRequest with the given API version.
encodeOffsetCommitRequest :: MonadPut m => E.ApiVersion -> OffsetCommitRequest -> m ()
encodeOffsetCommitRequest version msg
  | version == 7 =
    do
      serialize (offsetCommitRequestGroupId msg)
      serialize (offsetCommitRequestGenerationIdOrMemberEpoch msg)
      serialize (offsetCommitRequestMemberId msg)
      serialize (offsetCommitRequestGroupInstanceId msg)
      E.encodeVersionedArray version 8 encodeOffsetCommitRequestTopic (case P.unKafkaArray (offsetCommitRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 5 && version <= 6 =
    do
      serialize (offsetCommitRequestGroupId msg)
      serialize (offsetCommitRequestGenerationIdOrMemberEpoch msg)
      serialize (offsetCommitRequestMemberId msg)
      E.encodeVersionedArray version 8 encodeOffsetCommitRequestTopic (case P.unKafkaArray (offsetCommitRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 8 && version <= 9 =
    do
      serialize (toCompactString (offsetCommitRequestGroupId msg))
      serialize (offsetCommitRequestGenerationIdOrMemberEpoch msg)
      serialize (toCompactString (offsetCommitRequestMemberId msg))
      serialize (toCompactString (offsetCommitRequestGroupInstanceId msg))
      E.encodeVersionedArray version 8 encodeOffsetCommitRequestTopic (case P.unKafkaArray (offsetCommitRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 2 && version <= 4 =
    do
      serialize (offsetCommitRequestGroupId msg)
      serialize (offsetCommitRequestGenerationIdOrMemberEpoch msg)
      serialize (offsetCommitRequestMemberId msg)
      serialize (offsetCommitRequestRetentionTimeMs msg)
      E.encodeVersionedArray version 8 encodeOffsetCommitRequestTopic (case P.unKafkaArray (offsetCommitRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetCommitRequest with the given API version.
decodeOffsetCommitRequest :: MonadGet m => E.ApiVersion -> m OffsetCommitRequest
decodeOffsetCommitRequest version
  | version == 7 =
    do
      fieldgroupid <- deserialize
      fieldgenerationidormemberepoch <- deserialize
      fieldmemberid <- deserialize
      fieldgroupinstanceid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 8 decodeOffsetCommitRequestTopic
      pure OffsetCommitRequest
        {
        offsetCommitRequestGroupId = fieldgroupid
        ,
        offsetCommitRequestGenerationIdOrMemberEpoch = fieldgenerationidormemberepoch
        ,
        offsetCommitRequestMemberId = fieldmemberid
        ,
        offsetCommitRequestGroupInstanceId = fieldgroupinstanceid
        ,
        offsetCommitRequestRetentionTimeMs = (-1)
        ,
        offsetCommitRequestTopics = fieldtopics
        }

  | version >= 5 && version <= 6 =
    do
      fieldgroupid <- deserialize
      fieldgenerationidormemberepoch <- deserialize
      fieldmemberid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 8 decodeOffsetCommitRequestTopic
      pure OffsetCommitRequest
        {
        offsetCommitRequestGroupId = fieldgroupid
        ,
        offsetCommitRequestGenerationIdOrMemberEpoch = fieldgenerationidormemberepoch
        ,
        offsetCommitRequestMemberId = fieldmemberid
        ,
        offsetCommitRequestGroupInstanceId = P.KafkaString Null
        ,
        offsetCommitRequestRetentionTimeMs = (-1)
        ,
        offsetCommitRequestTopics = fieldtopics
        }

  | version >= 8 && version <= 9 =
    do
      fieldgroupid <- if version >= 8 then P.fromCompactString <$> deserialize else deserialize
      fieldgenerationidormemberepoch <- deserialize
      fieldmemberid <- if version >= 8 then P.fromCompactString <$> deserialize else deserialize
      fieldgroupinstanceid <- if version >= 8 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 8 decodeOffsetCommitRequestTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetCommitRequest
        {
        offsetCommitRequestGroupId = fieldgroupid
        ,
        offsetCommitRequestGenerationIdOrMemberEpoch = fieldgenerationidormemberepoch
        ,
        offsetCommitRequestMemberId = fieldmemberid
        ,
        offsetCommitRequestGroupInstanceId = fieldgroupinstanceid
        ,
        offsetCommitRequestRetentionTimeMs = (-1)
        ,
        offsetCommitRequestTopics = fieldtopics
        }

  | version >= 2 && version <= 4 =
    do
      fieldgroupid <- deserialize
      fieldgenerationidormemberepoch <- deserialize
      fieldmemberid <- deserialize
      fieldretentiontimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 8 decodeOffsetCommitRequestTopic
      pure OffsetCommitRequest
        {
        offsetCommitRequestGroupId = fieldgroupid
        ,
        offsetCommitRequestGenerationIdOrMemberEpoch = fieldgenerationidormemberepoch
        ,
        offsetCommitRequestMemberId = fieldmemberid
        ,
        offsetCommitRequestGroupInstanceId = P.KafkaString Null
        ,
        offsetCommitRequestRetentionTimeMs = fieldretentiontimems
        ,
        offsetCommitRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version