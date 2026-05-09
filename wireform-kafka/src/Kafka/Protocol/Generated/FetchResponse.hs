{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.FetchResponse
Description : Kafka FetchResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 1.



Valid versions: 4-17
Flexible versions: 12+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.FetchResponse
  (
    FetchResponse(..),
    FetchableTopicResponse(..),
    PartitionData(..),
    EpochEndOffset(..),
    LeaderIdAndEpoch(..),
    SnapshotId(..),
    AbortedTransaction(..),
    NodeEndpoint(..),
    encodeFetchResponse,
    decodeFetchResponse,
    maxFetchResponseVersion
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


-- | In case divergence is detected based on the `LastFetchedEpoch` and `FetchOffset` in the request, this field indicates the largest epoch and its end offset such that subsequent records are known to div
data EpochEndOffset = EpochEndOffset
  {

  -- | The largest epoch.

  -- Versions: 12+
  epochEndOffsetEpoch :: !(Int32)
,

  -- | The end offset of the epoch.

  -- Versions: 12+
  epochEndOffsetEndOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode EpochEndOffset with version-aware field handling.
encodeEpochEndOffset :: MonadPut m => E.ApiVersion -> EpochEndOffset -> m ()
encodeEpochEndOffset version emsg =
  do
    when (version >= 12) $
      serialize (epochEndOffsetEpoch emsg)
    when (version >= 12) $
      serialize (epochEndOffsetEndOffset emsg)
    when (version >= 12) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode EpochEndOffset with version-aware field handling.
decodeEpochEndOffset :: MonadGet m => E.ApiVersion -> m EpochEndOffset
decodeEpochEndOffset version =
  do
    fieldepoch <- if version >= 12
      then deserialize
      else pure ((-1))
    fieldendoffset <- if version >= 12
      then deserialize
      else pure ((-1))
    _ <- if version >= 12 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure EpochEndOffset
      {
      epochEndOffsetEpoch = fieldepoch
      ,
      epochEndOffsetEndOffset = fieldendoffset
      }


-- | The current leader of the partition.
data LeaderIdAndEpoch = LeaderIdAndEpoch
  {

  -- | The ID of the current leader or -1 if the leader is unknown.

  -- Versions: 12+
  leaderIdAndEpochLeaderId :: !(Int32)
,

  -- | The latest known leader epoch.

  -- Versions: 12+
  leaderIdAndEpochLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode LeaderIdAndEpoch with version-aware field handling.
encodeLeaderIdAndEpoch :: MonadPut m => E.ApiVersion -> LeaderIdAndEpoch -> m ()
encodeLeaderIdAndEpoch version lmsg =
  do
    when (version >= 12) $
      serialize (leaderIdAndEpochLeaderId lmsg)
    when (version >= 12) $
      serialize (leaderIdAndEpochLeaderEpoch lmsg)
    when (version >= 12) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode LeaderIdAndEpoch with version-aware field handling.
decodeLeaderIdAndEpoch :: MonadGet m => E.ApiVersion -> m LeaderIdAndEpoch
decodeLeaderIdAndEpoch version =
  do
    fieldleaderid <- if version >= 12
      then deserialize
      else pure ((-1))
    fieldleaderepoch <- if version >= 12
      then deserialize
      else pure ((-1))
    _ <- if version >= 12 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure LeaderIdAndEpoch
      {
      leaderIdAndEpochLeaderId = fieldleaderid
      ,
      leaderIdAndEpochLeaderEpoch = fieldleaderepoch
      }


-- | In the case of fetching an offset less than the LogStartOffset, this is the end offset and epoch that should be used in the FetchSnapshot request.
data SnapshotId = SnapshotId
  {

  -- | The end offset of the epoch.

  -- Versions: 0+
  snapshotIdEndOffset :: !(Int64)
,

  -- | The largest epoch.

  -- Versions: 0+
  snapshotIdEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode SnapshotId with version-aware field handling.
encodeSnapshotId :: MonadPut m => E.ApiVersion -> SnapshotId -> m ()
encodeSnapshotId version smsg =
  do
    serialize (snapshotIdEndOffset smsg)
    serialize (snapshotIdEpoch smsg)
    when (version >= 12) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode SnapshotId with version-aware field handling.
decodeSnapshotId :: MonadGet m => E.ApiVersion -> m SnapshotId
decodeSnapshotId version =
  do
    fieldendoffset <- deserialize
    fieldepoch <- deserialize
    _ <- if version >= 12 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure SnapshotId
      {
      snapshotIdEndOffset = fieldendoffset
      ,
      snapshotIdEpoch = fieldepoch
      }


-- | The aborted transactions.
data AbortedTransaction = AbortedTransaction
  {

  -- | The producer id associated with the aborted transaction.

  -- Versions: 4+
  abortedTransactionProducerId :: !(Int64)
,

  -- | The first offset in the aborted transaction.

  -- Versions: 4+
  abortedTransactionFirstOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode AbortedTransaction with version-aware field handling.
encodeAbortedTransaction :: MonadPut m => E.ApiVersion -> AbortedTransaction -> m ()
encodeAbortedTransaction version amsg =
  do
    when (version >= 4) $
      serialize (abortedTransactionProducerId amsg)
    when (version >= 4) $
      serialize (abortedTransactionFirstOffset amsg)
    when (version >= 12) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AbortedTransaction with version-aware field handling.
decodeAbortedTransaction :: MonadGet m => E.ApiVersion -> m AbortedTransaction
decodeAbortedTransaction version =
  do
    fieldproducerid <- if version >= 4
      then deserialize
      else pure (0)
    fieldfirstoffset <- if version >= 4
      then deserialize
      else pure (0)
    _ <- if version >= 12 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AbortedTransaction
      {
      abortedTransactionProducerId = fieldproducerid
      ,
      abortedTransactionFirstOffset = fieldfirstoffset
      }


-- | The topic partitions.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no fetch error.

  -- Versions: 0+
  partitionDataErrorCode :: !(Int16)
,

  -- | The current high water mark.

  -- Versions: 0+
  partitionDataHighWatermark :: !(Int64)
,

  -- | The last stable offset (or LSO) of the partition. This is the last offset such that the state of all

  -- Versions: 4+
  partitionDataLastStableOffset :: !(Int64)
,

  -- | The current log start offset.

  -- Versions: 5+
  partitionDataLogStartOffset :: !(Int64)
,

  -- | In case divergence is detected based on the `LastFetchedEpoch` and `FetchOffset` in the request, thi

  -- Versions: 12+
  partitionDataDivergingEpoch :: !(EpochEndOffset)
,

  -- | The current leader of the partition.

  -- Versions: 12+
  partitionDataCurrentLeader :: !(LeaderIdAndEpoch)
,

  -- | In the case of fetching an offset less than the LogStartOffset, this is the end offset and epoch tha

  -- Versions: 12+
  partitionDataSnapshotId :: !(SnapshotId)
,

  -- | The aborted transactions.

  -- Versions: 4+
  partitionDataAbortedTransactions :: !(KafkaArray (AbortedTransaction))
,

  -- | The preferred read replica for the consumer to use on its next fetch request.

  -- Versions: 11+
  partitionDataPreferredReadReplica :: !(Int32)
,

  -- | The record data.

  -- Versions: 0+
  partitionDataRecords :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataErrorCode pmsg)
    serialize (partitionDataHighWatermark pmsg)
    when (version >= 4) $
      serialize (partitionDataLastStableOffset pmsg)
    when (version >= 5) $
      serialize (partitionDataLogStartOffset pmsg)
    when (version >= 4) $
      E.encodeVersionedNullableArray version 12 encodeAbortedTransaction (partitionDataAbortedTransactions pmsg)
    when (version >= 11) $
      serialize (partitionDataPreferredReadReplica pmsg)
    if version >= 12 then serialize (toCompactBytes (partitionDataRecords pmsg)) else serialize (partitionDataRecords pmsg)
    when (version >= 12) $ do
      let _entries = (if version >= 12 then [(0, Data.Bytes.Put.runPutS (encodeEpochEndOffset version (partitionDataDivergingEpoch pmsg)))] else []) ++ (if version >= 12 then [(1, Data.Bytes.Put.runPutS (encodeLeaderIdAndEpoch version (partitionDataCurrentLeader pmsg)))] else []) ++ (if version >= 12 then [(2, Data.Bytes.Put.runPutS (encodeSnapshotId version (partitionDataSnapshotId pmsg)))] else [])
      P.serializeTaggedFieldEntries _entries


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fieldhighwatermark <- deserialize
    fieldlaststableoffset <- if version >= 4
      then deserialize
      else pure ((-1))
    fieldlogstartoffset <- if version >= 5
      then deserialize
      else pure ((-1))
    fieldabortedtransactions <- if version >= 4
      then E.decodeVersionedNullableArray version 12 decodeAbortedTransaction
      else pure (P.KafkaArray P.Null)
    fieldpreferredreadreplica <- if version >= 11
      then deserialize
      else pure ((-1))
    fieldrecords <- if version >= 12 then P.fromCompactBytes <$> deserialize else deserialize
    _taggedFields <- if version >= 12 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    let fielddivergingepoch =
          if version >= 12
            then case P.lookupTaggedField 0 _taggedFields of
              Just _bs -> case Data.Bytes.Get.runGetS (decodeEpochEndOffset version) _bs of
                  Right _v -> _v
                  Left  _  -> (EpochEndOffset { epochEndOffsetEpoch = (-1), epochEndOffsetEndOffset = (-1) })
              Nothing  -> (EpochEndOffset { epochEndOffsetEpoch = (-1), epochEndOffsetEndOffset = (-1) })
            else (EpochEndOffset { epochEndOffsetEpoch = (-1), epochEndOffsetEndOffset = (-1) })
    let fieldcurrentleader =
          if version >= 12
            then case P.lookupTaggedField 1 _taggedFields of
              Just _bs -> case Data.Bytes.Get.runGetS (decodeLeaderIdAndEpoch version) _bs of
                  Right _v -> _v
                  Left  _  -> (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = (-1), leaderIdAndEpochLeaderEpoch = (-1) })
              Nothing  -> (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = (-1), leaderIdAndEpochLeaderEpoch = (-1) })
            else (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = (-1), leaderIdAndEpochLeaderEpoch = (-1) })
    let fieldsnapshotid =
          if version >= 12
            then case P.lookupTaggedField 2 _taggedFields of
              Just _bs -> case Data.Bytes.Get.runGetS (decodeSnapshotId version) _bs of
                  Right _v -> _v
                  Left  _  -> (SnapshotId { snapshotIdEndOffset = (-1), snapshotIdEpoch = (-1) })
              Nothing  -> (SnapshotId { snapshotIdEndOffset = (-1), snapshotIdEpoch = (-1) })
            else (SnapshotId { snapshotIdEndOffset = (-1), snapshotIdEpoch = (-1) })
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataErrorCode = fielderrorcode
      ,
      partitionDataHighWatermark = fieldhighwatermark
      ,
      partitionDataLastStableOffset = fieldlaststableoffset
      ,
      partitionDataLogStartOffset = fieldlogstartoffset
      ,
      partitionDataDivergingEpoch = fielddivergingepoch
      ,
      partitionDataCurrentLeader = fieldcurrentleader
      ,
      partitionDataSnapshotId = fieldsnapshotid
      ,
      partitionDataAbortedTransactions = fieldabortedtransactions
      ,
      partitionDataPreferredReadReplica = fieldpreferredreadreplica
      ,
      partitionDataRecords = fieldrecords
      }


-- | The response topics.
data FetchableTopicResponse = FetchableTopicResponse
  {

  -- | The topic name.

  -- Versions: 0-12
  fetchableTopicResponseTopic :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 13+
  fetchableTopicResponseTopicId :: !(KafkaUuid)
,

  -- | The topic partitions.

  -- Versions: 0+
  fetchableTopicResponsePartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode FetchableTopicResponse with version-aware field handling.
encodeFetchableTopicResponse :: MonadPut m => E.ApiVersion -> FetchableTopicResponse -> m ()
encodeFetchableTopicResponse version fmsg =
  do
    when (version >= 0 && version <= 12) $
      if version >= 12 then serialize (toCompactString (fetchableTopicResponseTopic fmsg)) else serialize (fetchableTopicResponseTopic fmsg)
    when (version >= 13) $
      serialize (fetchableTopicResponseTopicId fmsg)
    E.encodeVersionedArray version 12 encodePartitionData (case P.unKafkaArray (fetchableTopicResponsePartitions fmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 12) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode FetchableTopicResponse with version-aware field handling.
decodeFetchableTopicResponse :: MonadGet m => E.ApiVersion -> m FetchableTopicResponse
decodeFetchableTopicResponse version =
  do
    fieldtopic <- if version >= 0 && version <= 12
      then if version >= 12 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 13
      then deserialize
      else pure (P.nullUuid)
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodePartitionData
    _ <- if version >= 12 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure FetchableTopicResponse
      {
      fetchableTopicResponseTopic = fieldtopic
      ,
      fetchableTopicResponseTopicId = fieldtopicid
      ,
      fetchableTopicResponsePartitions = fieldpartitions
      }


-- | Endpoints for all current-leaders enumerated in PartitionData, with errors NOT_LEADER_OR_FOLLOWER & FENCED_LEADER_EPOCH.
data NodeEndpoint = NodeEndpoint
  {

  -- | The ID of the associated node.

  -- Versions: 16+
  nodeEndpointNodeId :: !(Int32)
,

  -- | The node's hostname.

  -- Versions: 16+
  nodeEndpointHost :: !(KafkaString)
,

  -- | The node's port.

  -- Versions: 16+
  nodeEndpointPort :: !(Int32)
,

  -- | The rack of the node, or null if it has not been assigned to a rack.

  -- Versions: 16+
  nodeEndpointRack :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode NodeEndpoint with version-aware field handling.
encodeNodeEndpoint :: MonadPut m => E.ApiVersion -> NodeEndpoint -> m ()
encodeNodeEndpoint version nmsg =
  do
    when (version >= 16) $
      serialize (nodeEndpointNodeId nmsg)
    when (version >= 16) $
      if version >= 12 then serialize (toCompactString (nodeEndpointHost nmsg)) else serialize (nodeEndpointHost nmsg)
    when (version >= 16) $
      serialize (nodeEndpointPort nmsg)
    when (version >= 16) $
      if version >= 12 then serialize (toCompactString (nodeEndpointRack nmsg)) else serialize (nodeEndpointRack nmsg)
    when (version >= 12) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode NodeEndpoint with version-aware field handling.
decodeNodeEndpoint :: MonadGet m => E.ApiVersion -> m NodeEndpoint
decodeNodeEndpoint version =
  do
    fieldnodeid <- if version >= 16
      then deserialize
      else pure (0)
    fieldhost <- if version >= 16
      then if version >= 12 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldport <- if version >= 16
      then deserialize
      else pure (0)
    fieldrack <- if version >= 16
      then if version >= 12 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 12 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
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



data FetchResponse = FetchResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  fetchResponseThrottleTimeMs :: !(Int32)
,

  -- | The top level response error code.

  -- Versions: 7+
  fetchResponseErrorCode :: !(Int16)
,

  -- | The fetch session ID, or 0 if this is not part of a fetch session.

  -- Versions: 7+
  fetchResponseSessionId :: !(Int32)
,

  -- | The response topics.

  -- Versions: 0+
  fetchResponseResponses :: !(KafkaArray (FetchableTopicResponse))
,

  -- | Endpoints for all current-leaders enumerated in PartitionData, with errors NOT_LEADER_OR_FOLLOWER & 

  -- Versions: 16+
  fetchResponseNodeEndpoints :: !(KafkaArray (NodeEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for FetchResponse.
maxFetchResponseVersion :: Int16
maxFetchResponseVersion = 17

-- | KafkaMessage instance for FetchResponse.
instance KafkaMessage FetchResponse where
  messageApiKey = 1
  messageMinVersion = 4
  messageMaxVersion = 17
  messageFlexibleVersion = Just 12

-- | Encode FetchResponse with the given API version.
encodeFetchResponse :: MonadPut m => E.ApiVersion -> FetchResponse -> m ()
encodeFetchResponse version msg
  | version >= 16 && version <= 17 =
    do
      serialize (fetchResponseThrottleTimeMs msg)
      serialize (fetchResponseErrorCode msg)
      serialize (fetchResponseSessionId msg)
      E.encodeVersionedArray version 12 encodeFetchableTopicResponse (case P.unKafkaArray (fetchResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      do
        let _entries = (if version >= 16 then [(0, Data.Bytes.Put.runPutS (E.encodeVersionedArray version 999 encodeNodeEndpoint (case P.unKafkaArray (fetchResponseNodeEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })))] else [])
        P.serializeTaggedFieldEntries _entries

  | version >= 4 && version <= 6 =
    do
      serialize (fetchResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 12 encodeFetchableTopicResponse (case P.unKafkaArray (fetchResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 12 && version <= 15 =
    do
      serialize (fetchResponseThrottleTimeMs msg)
      serialize (fetchResponseErrorCode msg)
      serialize (fetchResponseSessionId msg)
      E.encodeVersionedArray version 12 encodeFetchableTopicResponse (case P.unKafkaArray (fetchResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      do
        let _entries = (if version >= 16 then [(0, Data.Bytes.Put.runPutS (E.encodeVersionedArray version 999 encodeNodeEndpoint (case P.unKafkaArray (fetchResponseNodeEndpoints msg) of { P.NotNull v -> v; P.Null -> V.empty })))] else [])
        P.serializeTaggedFieldEntries _entries

  | version >= 7 && version <= 11 =
    do
      serialize (fetchResponseThrottleTimeMs msg)
      serialize (fetchResponseErrorCode msg)
      serialize (fetchResponseSessionId msg)
      E.encodeVersionedArray version 12 encodeFetchableTopicResponse (case P.unKafkaArray (fetchResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode FetchResponse with the given API version.
decodeFetchResponse :: MonadGet m => E.ApiVersion -> m FetchResponse
decodeFetchResponse version
  | version >= 16 && version <= 17 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldsessionid <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeFetchableTopicResponse
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldnodeendpoints =
            if version >= 16
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeNodeEndpoint) _bs of
                    Right _v -> _v
                    Left  _  -> (P.mkKafkaArray V.empty)
                Nothing  -> (P.mkKafkaArray V.empty)
              else (P.mkKafkaArray V.empty)
      pure FetchResponse
        {
        fetchResponseThrottleTimeMs = fieldthrottletimems
        ,
        fetchResponseErrorCode = fielderrorcode
        ,
        fetchResponseSessionId = fieldsessionid
        ,
        fetchResponseResponses = fieldresponses
        ,
        fetchResponseNodeEndpoints = fieldnodeendpoints
        }

  | version >= 4 && version <= 6 =
    do
      fieldthrottletimems <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeFetchableTopicResponse
      pure FetchResponse
        {
        fetchResponseThrottleTimeMs = fieldthrottletimems
        ,
        fetchResponseErrorCode = 0
        ,
        fetchResponseSessionId = 0
        ,
        fetchResponseResponses = fieldresponses
        ,
        fetchResponseNodeEndpoints = P.mkKafkaArray V.empty
        }

  | version >= 12 && version <= 15 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldsessionid <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeFetchableTopicResponse
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldnodeendpoints =
            if version >= 16
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeNodeEndpoint) _bs of
                    Right _v -> _v
                    Left  _  -> (P.mkKafkaArray V.empty)
                Nothing  -> (P.mkKafkaArray V.empty)
              else (P.mkKafkaArray V.empty)
      pure FetchResponse
        {
        fetchResponseThrottleTimeMs = fieldthrottletimems
        ,
        fetchResponseErrorCode = fielderrorcode
        ,
        fetchResponseSessionId = fieldsessionid
        ,
        fetchResponseResponses = fieldresponses
        ,
        fetchResponseNodeEndpoints = fieldnodeendpoints
        }

  | version >= 7 && version <= 11 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldsessionid <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeFetchableTopicResponse
      pure FetchResponse
        {
        fetchResponseThrottleTimeMs = fieldthrottletimems
        ,
        fetchResponseErrorCode = fielderrorcode
        ,
        fetchResponseSessionId = fieldsessionid
        ,
        fetchResponseResponses = fieldresponses
        ,
        fetchResponseNodeEndpoints = P.mkKafkaArray V.empty
        }
  | otherwise = fail $ "Unsupported version: " ++ show version