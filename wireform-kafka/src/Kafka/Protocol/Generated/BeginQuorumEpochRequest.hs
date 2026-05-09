{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.BeginQuorumEpochRequest
Description : Kafka BeginQuorumEpochRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 53.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.BeginQuorumEpochRequest
  (
    BeginQuorumEpochRequest(..),
    TopicData(..),
    PartitionData(..),
    LeaderEndpoint(..),
    encodeBeginQuorumEpochRequest,
    decodeBeginQuorumEpochRequest,
    maxBeginQuorumEpochRequestVersion
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


-- | The partitions.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The directory id of the receiving replica.

  -- Versions: 1+
  partitionDataVoterDirectoryId :: !(KafkaUuid)
,

  -- | The ID of the newly elected leader.

  -- Versions: 0+
  partitionDataLeaderId :: !(Int32)
,

  -- | The epoch of the newly elected leader.

  -- Versions: 0+
  partitionDataLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    when (version >= 1) $
      serialize (partitionDataVoterDirectoryId pmsg)
    serialize (partitionDataLeaderId pmsg)
    serialize (partitionDataLeaderEpoch pmsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fieldvoterdirectoryid <- if version >= 1
      then deserialize
      else pure (P.nullUuid)
    fieldleaderid <- deserialize
    fieldleaderepoch <- deserialize
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataVoterDirectoryId = fieldvoterdirectoryid
      ,
      partitionDataLeaderId = fieldleaderid
      ,
      partitionDataLeaderEpoch = fieldleaderepoch
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



data BeginQuorumEpochRequest = BeginQuorumEpochRequest
  {

  -- | The cluster id.

  -- Versions: 0+
  beginQuorumEpochRequestClusterId :: !(KafkaString)
,

  -- | The replica id of the voter receiving the request.

  -- Versions: 1+
  beginQuorumEpochRequestVoterId :: !(Int32)
,

  -- | The topics.

  -- Versions: 0+
  beginQuorumEpochRequestTopics :: !(KafkaArray (TopicData))
,

  -- | Endpoints for the leader.

  -- Versions: 1+
  beginQuorumEpochRequestLeaderEndpoints :: !(KafkaArray (LeaderEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for BeginQuorumEpochRequest.
maxBeginQuorumEpochRequestVersion :: Int16
maxBeginQuorumEpochRequestVersion = 1

-- | Encode BeginQuorumEpochRequest with the given API version.
encodeBeginQuorumEpochRequest :: MonadPut m => E.ApiVersion -> BeginQuorumEpochRequest -> m ()
encodeBeginQuorumEpochRequest version msg
  | version == 0 =
    do
      serialize (beginQuorumEpochRequestClusterId msg)
      E.encodeVersionedArray version 1 encodeTopicData (case P.unKafkaArray (beginQuorumEpochRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 1 =
    do
      serialize (toCompactString (beginQuorumEpochRequestClusterId msg))
      serialize (beginQuorumEpochRequestVoterId msg)
      E.encodeVersionedArray version 1 encodeTopicData (case P.unKafkaArray (beginQuorumEpochRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 1 encodeLeaderEndpoint (case P.unKafkaArray (beginQuorumEpochRequestLeaderEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode BeginQuorumEpochRequest with the given API version.
decodeBeginQuorumEpochRequest :: MonadGet m => E.ApiVersion -> m BeginQuorumEpochRequest
decodeBeginQuorumEpochRequest version
  | version == 0 =
    do
      fieldclusterid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeTopicData
      pure BeginQuorumEpochRequest
        {
        beginQuorumEpochRequestClusterId = fieldclusterid
        ,
        beginQuorumEpochRequestVoterId = (-1)
        ,
        beginQuorumEpochRequestTopics = fieldtopics
        ,
        beginQuorumEpochRequestLeaderEndpoints = P.mkKafkaArray V.empty
        }

  | version == 1 =
    do
      fieldclusterid <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
      fieldvoterid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeTopicData
      fieldleaderendpoints <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeLeaderEndpoint
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure BeginQuorumEpochRequest
        {
        beginQuorumEpochRequestClusterId = fieldclusterid
        ,
        beginQuorumEpochRequestVoterId = fieldvoterid
        ,
        beginQuorumEpochRequestTopics = fieldtopics
        ,
        beginQuorumEpochRequestLeaderEndpoints = fieldleaderendpoints
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec BeginQuorumEpochRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
