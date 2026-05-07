{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.EndQuorumEpochRequest
Description : Kafka EndQuorumEpochRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 54.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.EndQuorumEpochRequest
  (
    EndQuorumEpochRequest(..),
    TopicData(..),
    PartitionData(..),
    ReplicaInfo(..),
    LeaderEndpoint(..),
    encodeEndQuorumEpochRequest,
    decodeEndQuorumEpochRequest,
    maxEndQuorumEpochRequestVersion
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


-- | A sorted list of preferred candidates to start the election.
data ReplicaInfo = ReplicaInfo
  {

  -- | The ID of the candidate replica.

  -- Versions: 1+
  replicaInfoCandidateId :: !(Int32)
,

  -- | The directory ID of the candidate replica.

  -- Versions: 1+
  replicaInfoCandidateDirectoryId :: !(KafkaUuid)

  }
  deriving (Eq, Show, Generic)


-- | Encode ReplicaInfo with version-aware field handling.
encodeReplicaInfo :: MonadPut m => E.ApiVersion -> ReplicaInfo -> m ()
encodeReplicaInfo version rmsg =
  do
    when (version >= 1) $
      serialize (replicaInfoCandidateId rmsg)
    when (version >= 1) $
      serialize (replicaInfoCandidateDirectoryId rmsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReplicaInfo with version-aware field handling.
decodeReplicaInfo :: MonadGet m => E.ApiVersion -> m ReplicaInfo
decodeReplicaInfo version =
  do
    fieldcandidateid <- if version >= 1
      then deserialize
      else pure (0)
    fieldcandidatedirectoryid <- if version >= 1
      then deserialize
      else pure (P.nullUuid)
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReplicaInfo
      {
      replicaInfoCandidateId = fieldcandidateid
      ,
      replicaInfoCandidateDirectoryId = fieldcandidatedirectoryid
      }


-- | The partitions.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The current leader ID that is resigning.

  -- Versions: 0+
  partitionDataLeaderId :: !(Int32)
,

  -- | The current epoch.

  -- Versions: 0+
  partitionDataLeaderEpoch :: !(Int32)
,

  -- | A sorted list of preferred successors to start the election.

  -- Versions: 0
  partitionDataPreferredSuccessors :: !(KafkaArray (Int32))
,

  -- | A sorted list of preferred candidates to start the election.

  -- Versions: 1+
  partitionDataPreferredCandidates :: !(KafkaArray (ReplicaInfo))

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataLeaderId pmsg)
    serialize (partitionDataLeaderEpoch pmsg)
    when (version == 0) $
      E.encodeVersionedArray version 1 (\_ x -> serialize x) (case P.unKafkaArray (partitionDataPreferredSuccessors pmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 1) $
      E.encodeVersionedArray version 1 encodeReplicaInfo (case P.unKafkaArray (partitionDataPreferredCandidates pmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fieldleaderid <- deserialize
    fieldleaderepoch <- deserialize
    fieldpreferredsuccessors <- if version == 0
      then P.mkKafkaArray <$> E.decodeVersionedArray version 1 (\_ -> deserialize)
      else pure (P.mkKafkaArray V.empty)
    fieldpreferredcandidates <- if version >= 1
      then P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeReplicaInfo
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataLeaderId = fieldleaderid
      ,
      partitionDataLeaderEpoch = fieldleaderepoch
      ,
      partitionDataPreferredSuccessors = fieldpreferredsuccessors
      ,
      partitionDataPreferredCandidates = fieldpreferredcandidates
      }


-- | The topics.
data TopicData = TopicData
  {

  -- | The topic name.

  -- Versions: 0+
  topicDataTopicName :: !(KafkaString)
,

  -- | The partitions.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicData with version-aware field handling.
encodeTopicData :: MonadPut m => E.ApiVersion -> TopicData -> m ()
encodeTopicData version tmsg =
  do
    if version >= 1 then serialize (toCompactString (topicDataTopicName tmsg)) else serialize (topicDataTopicName tmsg)
    E.encodeVersionedArray version 1 encodePartitionData (case P.unKafkaArray (topicDataPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicData with version-aware field handling.
decodeTopicData :: MonadGet m => E.ApiVersion -> m TopicData
decodeTopicData version =
  do
    fieldtopicname <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodePartitionData
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicData
      {
      topicDataTopicName = fieldtopicname
      ,
      topicDataPartitions = fieldpartitions
      }


-- | Endpoints for the leader.
data LeaderEndpoint = LeaderEndpoint
  {

  -- | The name of the endpoint.

  -- Versions: 1+
  leaderEndpointName :: !(KafkaString)
,

  -- | The node's hostname.

  -- Versions: 1+
  leaderEndpointHost :: !(KafkaString)
,

  -- | The node's port.

  -- Versions: 1+
  leaderEndpointPort :: !(Word16)

  }
  deriving (Eq, Show, Generic)


-- | Encode LeaderEndpoint with version-aware field handling.
encodeLeaderEndpoint :: MonadPut m => E.ApiVersion -> LeaderEndpoint -> m ()
encodeLeaderEndpoint version lmsg =
  do
    when (version >= 1) $
      if version >= 1 then serialize (toCompactString (leaderEndpointName lmsg)) else serialize (leaderEndpointName lmsg)
    when (version >= 1) $
      if version >= 1 then serialize (toCompactString (leaderEndpointHost lmsg)) else serialize (leaderEndpointHost lmsg)
    when (version >= 1) $
      serialize (leaderEndpointPort lmsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode LeaderEndpoint with version-aware field handling.
decodeLeaderEndpoint :: MonadGet m => E.ApiVersion -> m LeaderEndpoint
decodeLeaderEndpoint version =
  do
    fieldname <- if version >= 1
      then if version >= 1 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldhost <- if version >= 1
      then if version >= 1 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldport <- if version >= 1
      then deserialize
      else pure (0)
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure LeaderEndpoint
      {
      leaderEndpointName = fieldname
      ,
      leaderEndpointHost = fieldhost
      ,
      leaderEndpointPort = fieldport
      }



data EndQuorumEpochRequest = EndQuorumEpochRequest
  {

  -- | The cluster id.

  -- Versions: 0+
  endQuorumEpochRequestClusterId :: !(KafkaString)
,

  -- | The topics.

  -- Versions: 0+
  endQuorumEpochRequestTopics :: !(KafkaArray (TopicData))
,

  -- | Endpoints for the leader.

  -- Versions: 1+
  endQuorumEpochRequestLeaderEndpoints :: !(KafkaArray (LeaderEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for EndQuorumEpochRequest.
maxEndQuorumEpochRequestVersion :: Int16
maxEndQuorumEpochRequestVersion = 1

-- | Encode EndQuorumEpochRequest with the given API version.
encodeEndQuorumEpochRequest :: MonadPut m => E.ApiVersion -> EndQuorumEpochRequest -> m ()
encodeEndQuorumEpochRequest version msg
  | version == 0 =
    do
      serialize (endQuorumEpochRequestClusterId msg)
      E.encodeVersionedArray version 1 encodeTopicData (case P.unKafkaArray (endQuorumEpochRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 1 =
    do
      serialize (toCompactString (endQuorumEpochRequestClusterId msg))
      E.encodeVersionedArray version 1 encodeTopicData (case P.unKafkaArray (endQuorumEpochRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 1 encodeLeaderEndpoint (case P.unKafkaArray (endQuorumEpochRequestLeaderEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode EndQuorumEpochRequest with the given API version.
decodeEndQuorumEpochRequest :: MonadGet m => E.ApiVersion -> m EndQuorumEpochRequest
decodeEndQuorumEpochRequest version
  | version == 0 =
    do
      fieldclusterid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeTopicData
      pure EndQuorumEpochRequest
        {
        endQuorumEpochRequestClusterId = fieldclusterid
        ,
        endQuorumEpochRequestTopics = fieldtopics
        ,
        endQuorumEpochRequestLeaderEndpoints = P.mkKafkaArray V.empty
        }

  | version == 1 =
    do
      fieldclusterid <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeTopicData
      fieldleaderendpoints <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeLeaderEndpoint
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure EndQuorumEpochRequest
        {
        endQuorumEpochRequestClusterId = fieldclusterid
        ,
        endQuorumEpochRequestTopics = fieldtopics
        ,
        endQuorumEpochRequestLeaderEndpoints = fieldleaderendpoints
        }
  | otherwise = fail $ "Unsupported version: " ++ show version