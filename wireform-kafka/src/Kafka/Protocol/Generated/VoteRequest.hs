{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.VoteRequest
Description : Kafka VoteRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 52.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.VoteRequest
  (
    VoteRequest(..),
    TopicData(..),
    PartitionData(..),
    encodeVoteRequest,
    decodeVoteRequest,
    maxVoteRequestVersion
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


-- | The partition data.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The epoch of the voter sending the request

  -- Versions: 0+
  partitionDataReplicaEpoch :: !(Int32)
,

  -- | The replica id of the voter sending the request

  -- Versions: 0+
  partitionDataReplicaId :: !(Int32)
,

  -- | The directory id of the voter sending the request

  -- Versions: 1+
  partitionDataReplicaDirectoryId :: !(KafkaUuid)
,

  -- | The directory id of the voter receiving the request

  -- Versions: 1+
  partitionDataVoterDirectoryId :: !(KafkaUuid)
,

  -- | The epoch of the last record written to the metadata log.

  -- Versions: 0+
  partitionDataLastOffsetEpoch :: !(Int32)
,

  -- | The log end offset of the metadata log of the voter sending the request.

  -- Versions: 0+
  partitionDataLastOffset :: !(Int64)
,

  -- | Whether the request is a PreVote request (not persisted) or not.

  -- Versions: 2+
  partitionDataPreVote :: !(Bool)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataReplicaEpoch pmsg)
    serialize (partitionDataReplicaId pmsg)
    when (version >= 1) $
      serialize (partitionDataReplicaDirectoryId pmsg)
    when (version >= 1) $
      serialize (partitionDataVoterDirectoryId pmsg)
    serialize (partitionDataLastOffsetEpoch pmsg)
    serialize (partitionDataLastOffset pmsg)
    when (version >= 2) $
      serialize (partitionDataPreVote pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fieldreplicaepoch <- deserialize
    fieldreplicaid <- deserialize
    fieldreplicadirectoryid <- if version >= 1
      then deserialize
      else pure (P.nullUuid)
    fieldvoterdirectoryid <- if version >= 1
      then deserialize
      else pure (P.nullUuid)
    fieldlastoffsetepoch <- deserialize
    fieldlastoffset <- deserialize
    fieldprevote <- if version >= 2
      then deserialize
      else pure (False)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataReplicaEpoch = fieldreplicaepoch
      ,
      partitionDataReplicaId = fieldreplicaid
      ,
      partitionDataReplicaDirectoryId = fieldreplicadirectoryid
      ,
      partitionDataVoterDirectoryId = fieldvoterdirectoryid
      ,
      partitionDataLastOffsetEpoch = fieldlastoffsetepoch
      ,
      partitionDataLastOffset = fieldlastoffset
      ,
      partitionDataPreVote = fieldprevote
      }


-- | The topic data.
data TopicData = TopicData
  {

  -- | The topic name.

  -- Versions: 0+
  topicDataTopicName :: !(KafkaString)
,

  -- | The partition data.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicData with version-aware field handling.
encodeTopicData :: MonadPut m => E.ApiVersion -> TopicData -> m ()
encodeTopicData version tmsg =
  do
    if version >= 0 then serialize (toCompactString (topicDataTopicName tmsg)) else serialize (topicDataTopicName tmsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (topicDataPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicData with version-aware field handling.
decodeTopicData :: MonadGet m => E.ApiVersion -> m TopicData
decodeTopicData version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicData
      {
      topicDataTopicName = fieldtopicname
      ,
      topicDataPartitions = fieldpartitions
      }



data VoteRequest = VoteRequest
  {

  -- | The cluster id.

  -- Versions: 0+
  voteRequestClusterId :: !(KafkaString)
,

  -- | The replica id of the voter receiving the request.

  -- Versions: 1+
  voteRequestVoterId :: !(Int32)
,

  -- | The topic data.

  -- Versions: 0+
  voteRequestTopics :: !(KafkaArray (TopicData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for VoteRequest.
maxVoteRequestVersion :: Int16
maxVoteRequestVersion = 2

-- | Encode VoteRequest with the given API version.
encodeVoteRequest :: MonadPut m => E.ApiVersion -> VoteRequest -> m ()
encodeVoteRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (voteRequestClusterId msg))
      E.encodeVersionedArray version 0 encodeTopicData (case P.unKafkaArray (voteRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 2 =
    do
      serialize (toCompactString (voteRequestClusterId msg))
      serialize (voteRequestVoterId msg)
      E.encodeVersionedArray version 0 encodeTopicData (case P.unKafkaArray (voteRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode VoteRequest with the given API version.
decodeVoteRequest :: MonadGet m => E.ApiVersion -> m VoteRequest
decodeVoteRequest version
  | version == 0 =
    do
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure VoteRequest
        {
        voteRequestClusterId = fieldclusterid
        ,
        voteRequestVoterId = (-1)
        ,
        voteRequestTopics = fieldtopics
        }

  | version >= 1 && version <= 2 =
    do
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldvoterid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure VoteRequest
        {
        voteRequestClusterId = fieldclusterid
        ,
        voteRequestVoterId = fieldvoterid
        ,
        voteRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeVoteRequest' / 'decodeVoteRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec VoteRequest where
  wireCodec = Just (WC.serialShimCodec encodeVoteRequest decodeVoteRequest)
  {-# INLINE wireCodec #-}
