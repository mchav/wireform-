{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ShareFetchResponse
Description : Kafka ShareFetchResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 78.



Valid versions: 1-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ShareFetchResponse
  (
    ShareFetchResponse(..),
    ShareFetchableTopicResponse(..),
    PartitionData(..),
    LeaderIdAndEpoch(..),
    AcquiredRecords(..),
    NodeEndpoint(..),
    encodeShareFetchResponse,
    decodeShareFetchResponse,
    maxShareFetchResponseVersion
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


-- | The current leader of the partition.
data LeaderIdAndEpoch = LeaderIdAndEpoch
  {

  -- | The ID of the current leader or -1 if the leader is unknown.

  -- Versions: 0+
  leaderIdAndEpochLeaderId :: !(Int32)
,

  -- | The latest known leader epoch.

  -- Versions: 0+
  leaderIdAndEpochLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode LeaderIdAndEpoch with version-aware field handling.
encodeLeaderIdAndEpoch :: MonadPut m => E.ApiVersion -> LeaderIdAndEpoch -> m ()
encodeLeaderIdAndEpoch version lmsg =
  do
    serialize (leaderIdAndEpochLeaderId lmsg)
    serialize (leaderIdAndEpochLeaderEpoch lmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode LeaderIdAndEpoch with version-aware field handling.
decodeLeaderIdAndEpoch :: MonadGet m => E.ApiVersion -> m LeaderIdAndEpoch
decodeLeaderIdAndEpoch version =
  do
    fieldleaderid <- deserialize
    fieldleaderepoch <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure LeaderIdAndEpoch
      {
      leaderIdAndEpochLeaderId = fieldleaderid
      ,
      leaderIdAndEpochLeaderEpoch = fieldleaderepoch
      }


-- | The acquired records.
data AcquiredRecords = AcquiredRecords
  {

  -- | The earliest offset in this batch of acquired records.

  -- Versions: 0+
  acquiredRecordsFirstOffset :: !(Int64)
,

  -- | The last offset of this batch of acquired records.

  -- Versions: 0+
  acquiredRecordsLastOffset :: !(Int64)
,

  -- | The delivery count of this batch of acquired records.

  -- Versions: 0+
  acquiredRecordsDeliveryCount :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode AcquiredRecords with version-aware field handling.
encodeAcquiredRecords :: MonadPut m => E.ApiVersion -> AcquiredRecords -> m ()
encodeAcquiredRecords version amsg =
  do
    serialize (acquiredRecordsFirstOffset amsg)
    serialize (acquiredRecordsLastOffset amsg)
    serialize (acquiredRecordsDeliveryCount amsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AcquiredRecords with version-aware field handling.
decodeAcquiredRecords :: MonadGet m => E.ApiVersion -> m AcquiredRecords
decodeAcquiredRecords version =
  do
    fieldfirstoffset <- deserialize
    fieldlastoffset <- deserialize
    fielddeliverycount <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AcquiredRecords
      {
      acquiredRecordsFirstOffset = fieldfirstoffset
      ,
      acquiredRecordsLastOffset = fieldlastoffset
      ,
      acquiredRecordsDeliveryCount = fielddeliverycount
      }


-- | The topic partitions.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The fetch error code, or 0 if there was no fetch error.

  -- Versions: 0+
  partitionDataErrorCode :: !(Int16)
,

  -- | The fetch error message, or null if there was no fetch error.

  -- Versions: 0+
  partitionDataErrorMessage :: !(KafkaString)
,

  -- | The acknowledge error code, or 0 if there was no acknowledge error.

  -- Versions: 0+
  partitionDataAcknowledgeErrorCode :: !(Int16)
,

  -- | The acknowledge error message, or null if there was no acknowledge error.

  -- Versions: 0+
  partitionDataAcknowledgeErrorMessage :: !(KafkaString)
,

  -- | The current leader of the partition.

  -- Versions: 0+
  partitionDataCurrentLeader :: !(LeaderIdAndEpoch)
,

  -- | The record data.

  -- Versions: 0+
  partitionDataRecords :: !(KafkaBytes)
,

  -- | The acquired records.

  -- Versions: 0+
  partitionDataAcquiredRecords :: !(KafkaArray (AcquiredRecords))

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataErrorCode pmsg)
    if version >= 0 then serialize (toCompactString (partitionDataErrorMessage pmsg)) else serialize (partitionDataErrorMessage pmsg)
    serialize (partitionDataAcknowledgeErrorCode pmsg)
    if version >= 0 then serialize (toCompactString (partitionDataAcknowledgeErrorMessage pmsg)) else serialize (partitionDataAcknowledgeErrorMessage pmsg)
    encodeLeaderIdAndEpoch version (partitionDataCurrentLeader pmsg)
    if version >= 0 then serialize (toCompactBytes (partitionDataRecords pmsg)) else serialize (partitionDataRecords pmsg)
    E.encodeVersionedArray version 0 encodeAcquiredRecords (case P.unKafkaArray (partitionDataAcquiredRecords pmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldacknowledgeerrorcode <- deserialize
    fieldacknowledgeerrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldcurrentleader <- decodeLeaderIdAndEpoch version
    fieldrecords <- if version >= 0 then P.fromCompactBytes <$> deserialize else deserialize
    fieldacquiredrecords <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAcquiredRecords
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataErrorCode = fielderrorcode
      ,
      partitionDataErrorMessage = fielderrormessage
      ,
      partitionDataAcknowledgeErrorCode = fieldacknowledgeerrorcode
      ,
      partitionDataAcknowledgeErrorMessage = fieldacknowledgeerrormessage
      ,
      partitionDataCurrentLeader = fieldcurrentleader
      ,
      partitionDataRecords = fieldrecords
      ,
      partitionDataAcquiredRecords = fieldacquiredrecords
      }


-- | The response topics.
data ShareFetchableTopicResponse = ShareFetchableTopicResponse
  {

  -- | The unique topic ID.

  -- Versions: 0+
  shareFetchableTopicResponseTopicId :: !(KafkaUuid)
,

  -- | The topic partitions.

  -- Versions: 0+
  shareFetchableTopicResponsePartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode ShareFetchableTopicResponse with version-aware field handling.
encodeShareFetchableTopicResponse :: MonadPut m => E.ApiVersion -> ShareFetchableTopicResponse -> m ()
encodeShareFetchableTopicResponse version smsg =
  do
    serialize (shareFetchableTopicResponseTopicId smsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (shareFetchableTopicResponsePartitions smsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ShareFetchableTopicResponse with version-aware field handling.
decodeShareFetchableTopicResponse :: MonadGet m => E.ApiVersion -> m ShareFetchableTopicResponse
decodeShareFetchableTopicResponse version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ShareFetchableTopicResponse
      {
      shareFetchableTopicResponseTopicId = fieldtopicid
      ,
      shareFetchableTopicResponsePartitions = fieldpartitions
      }


-- | Endpoints for all current leaders enumerated in PartitionData with error NOT_LEADER_OR_FOLLOWER.
data NodeEndpoint = NodeEndpoint
  {

  -- | The ID of the associated node.

  -- Versions: 0+
  nodeEndpointNodeId :: !(Int32)
,

  -- | The node's hostname.

  -- Versions: 0+
  nodeEndpointHost :: !(KafkaString)
,

  -- | The node's port.

  -- Versions: 0+
  nodeEndpointPort :: !(Int32)
,

  -- | The rack of the node, or null if it has not been assigned to a rack.

  -- Versions: 0+
  nodeEndpointRack :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode NodeEndpoint with version-aware field handling.
encodeNodeEndpoint :: MonadPut m => E.ApiVersion -> NodeEndpoint -> m ()
encodeNodeEndpoint version nmsg =
  do
    serialize (nodeEndpointNodeId nmsg)
    if version >= 0 then serialize (toCompactString (nodeEndpointHost nmsg)) else serialize (nodeEndpointHost nmsg)
    serialize (nodeEndpointPort nmsg)
    if version >= 0 then serialize (toCompactString (nodeEndpointRack nmsg)) else serialize (nodeEndpointRack nmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode NodeEndpoint with version-aware field handling.
decodeNodeEndpoint :: MonadGet m => E.ApiVersion -> m NodeEndpoint
decodeNodeEndpoint version =
  do
    fieldnodeid <- deserialize
    fieldhost <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldport <- deserialize
    fieldrack <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure NodeEndpoint
      {
      nodeEndpointNodeId = fieldnodeid
      ,
      nodeEndpointHost = fieldhost
      ,
      nodeEndpointPort = fieldport
      ,
      nodeEndpointRack = fieldrack
      }



data ShareFetchResponse = ShareFetchResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  shareFetchResponseThrottleTimeMs :: !(Int32)
,

  -- | The top-level response error code.

  -- Versions: 0+
  shareFetchResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  shareFetchResponseErrorMessage :: !(KafkaString)
,

  -- | The time in milliseconds for which the acquired records are locked.

  -- Versions: 1+
  shareFetchResponseAcquisitionLockTimeoutMs :: !(Int32)
,

  -- | The response topics.

  -- Versions: 0+
  shareFetchResponseResponses :: !(KafkaArray (ShareFetchableTopicResponse))
,

  -- | Endpoints for all current leaders enumerated in PartitionData with error NOT_LEADER_OR_FOLLOWER.

  -- Versions: 0+
  shareFetchResponseNodeEndpoints :: !(KafkaArray (NodeEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ShareFetchResponse.
maxShareFetchResponseVersion :: Int16
maxShareFetchResponseVersion = 2

-- | Encode ShareFetchResponse with the given API version.
encodeShareFetchResponse :: MonadPut m => E.ApiVersion -> ShareFetchResponse -> m ()
encodeShareFetchResponse version msg
  | version >= 1 && version <= 2 =
    do
      serialize (shareFetchResponseThrottleTimeMs msg)
      serialize (shareFetchResponseErrorCode msg)
      serialize (toCompactString (shareFetchResponseErrorMessage msg))
      serialize (shareFetchResponseAcquisitionLockTimeoutMs msg)
      E.encodeVersionedArray version 0 encodeShareFetchableTopicResponse (case P.unKafkaArray (shareFetchResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeNodeEndpoint (case P.unKafkaArray (shareFetchResponseNodeEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ShareFetchResponse with the given API version.
decodeShareFetchResponse :: MonadGet m => E.ApiVersion -> m ShareFetchResponse
decodeShareFetchResponse version
  | version >= 1 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldacquisitionlocktimeoutms <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeShareFetchableTopicResponse
      fieldnodeendpoints <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeNodeEndpoint
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ShareFetchResponse
        {
        shareFetchResponseThrottleTimeMs = fieldthrottletimems
        ,
        shareFetchResponseErrorCode = fielderrorcode
        ,
        shareFetchResponseErrorMessage = fielderrormessage
        ,
        shareFetchResponseAcquisitionLockTimeoutMs = fieldacquisitionlocktimeoutms
        ,
        shareFetchResponseResponses = fieldresponses
        ,
        shareFetchResponseNodeEndpoints = fieldnodeendpoints
        }
  | otherwise = fail $ "Unsupported version: " ++ show version