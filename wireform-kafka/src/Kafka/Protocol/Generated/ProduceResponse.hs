{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ProduceResponse
Description : Kafka ProduceResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 0.



Valid versions: 3-13
Flexible versions: 9+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ProduceResponse
  (
    ProduceResponse(..),
    TopicProduceResponse(..),
    PartitionProduceResponse(..),
    BatchIndexAndErrorMessage(..),
    LeaderIdAndEpoch(..),
    NodeEndpoint(..),
    encodeProduceResponse,
    decodeProduceResponse,
    maxProduceResponseVersion
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


-- | The batch indices of records that caused the batch to be dropped.
data BatchIndexAndErrorMessage = BatchIndexAndErrorMessage
  {

  -- | The batch index of the record that caused the batch to be dropped.

  -- Versions: 8+
  batchIndexAndErrorMessageBatchIndex :: !(Int32)
,

  -- | The error message of the record that caused the batch to be dropped.

  -- Versions: 8+
  batchIndexAndErrorMessageBatchIndexErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode BatchIndexAndErrorMessage with version-aware field handling.
encodeBatchIndexAndErrorMessage :: MonadPut m => E.ApiVersion -> BatchIndexAndErrorMessage -> m ()
encodeBatchIndexAndErrorMessage version bmsg =
  do
    when (version >= 8) $
      serialize (batchIndexAndErrorMessageBatchIndex bmsg)
    when (version >= 8) $
      if version >= 9 then serialize (toCompactString (batchIndexAndErrorMessageBatchIndexErrorMessage bmsg)) else serialize (batchIndexAndErrorMessageBatchIndexErrorMessage bmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode BatchIndexAndErrorMessage with version-aware field handling.
decodeBatchIndexAndErrorMessage :: MonadGet m => E.ApiVersion -> m BatchIndexAndErrorMessage
decodeBatchIndexAndErrorMessage version =
  do
    fieldbatchindex <- if version >= 8
      then deserialize
      else pure (0)
    fieldbatchindexerrormessage <- if version >= 8
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure BatchIndexAndErrorMessage
      {
      batchIndexAndErrorMessageBatchIndex = fieldbatchindex
      ,
      batchIndexAndErrorMessageBatchIndexErrorMessage = fieldbatchindexerrormessage
      }


-- | The leader broker that the producer should use for future requests.
data LeaderIdAndEpoch = LeaderIdAndEpoch
  {

  -- | The ID of the current leader or -1 if the leader is unknown.

  -- Versions: 10+
  leaderIdAndEpochLeaderId :: !(Int32)
,

  -- | The latest known leader epoch.

  -- Versions: 10+
  leaderIdAndEpochLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode LeaderIdAndEpoch with version-aware field handling.
encodeLeaderIdAndEpoch :: MonadPut m => E.ApiVersion -> LeaderIdAndEpoch -> m ()
encodeLeaderIdAndEpoch version lmsg =
  do
    when (version >= 10) $
      serialize (leaderIdAndEpochLeaderId lmsg)
    when (version >= 10) $
      serialize (leaderIdAndEpochLeaderEpoch lmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode LeaderIdAndEpoch with version-aware field handling.
decodeLeaderIdAndEpoch :: MonadGet m => E.ApiVersion -> m LeaderIdAndEpoch
decodeLeaderIdAndEpoch version =
  do
    fieldleaderid <- if version >= 10
      then deserialize
      else pure ((-1))
    fieldleaderepoch <- if version >= 10
      then deserialize
      else pure ((-1))
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure LeaderIdAndEpoch
      {
      leaderIdAndEpochLeaderId = fieldleaderid
      ,
      leaderIdAndEpochLeaderEpoch = fieldleaderepoch
      }


-- | Each partition that we produced to within the topic.
data PartitionProduceResponse = PartitionProduceResponse
  {

  -- | The partition index.

  -- Versions: 0+
  partitionProduceResponseIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  partitionProduceResponseErrorCode :: !(Int16)
,

  -- | The base offset.

  -- Versions: 0+
  partitionProduceResponseBaseOffset :: !(Int64)
,

  -- | The timestamp returned by broker after appending the messages. If CreateTime is used for the topic, 

  -- Versions: 2+
  partitionProduceResponseLogAppendTimeMs :: !(Int64)
,

  -- | The log start offset.

  -- Versions: 5+
  partitionProduceResponseLogStartOffset :: !(Int64)
,

  -- | The batch indices of records that caused the batch to be dropped.

  -- Versions: 8+
  partitionProduceResponseRecordErrors :: !(KafkaArray (BatchIndexAndErrorMessage))
,

  -- | The global error message summarizing the common root cause of the records that caused the batch to b

  -- Versions: 8+
  partitionProduceResponseErrorMessage :: !(KafkaString)
,

  -- | The leader broker that the producer should use for future requests.

  -- Versions: 10+
  partitionProduceResponseCurrentLeader :: !(LeaderIdAndEpoch)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionProduceResponse with version-aware field handling.
encodePartitionProduceResponse :: MonadPut m => E.ApiVersion -> PartitionProduceResponse -> m ()
encodePartitionProduceResponse version pmsg =
  do
    serialize (partitionProduceResponseIndex pmsg)
    serialize (partitionProduceResponseErrorCode pmsg)
    serialize (partitionProduceResponseBaseOffset pmsg)
    when (version >= 2) $
      serialize (partitionProduceResponseLogAppendTimeMs pmsg)
    when (version >= 5) $
      serialize (partitionProduceResponseLogStartOffset pmsg)
    when (version >= 8) $
      E.encodeVersionedArray version 9 encodeBatchIndexAndErrorMessage (case P.unKafkaArray (partitionProduceResponseRecordErrors pmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 8) $
      if version >= 9 then serialize (toCompactString (partitionProduceResponseErrorMessage pmsg)) else serialize (partitionProduceResponseErrorMessage pmsg)
    when (version >= 10) $
      encodeLeaderIdAndEpoch version (partitionProduceResponseCurrentLeader pmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionProduceResponse with version-aware field handling.
decodePartitionProduceResponse :: MonadGet m => E.ApiVersion -> m PartitionProduceResponse
decodePartitionProduceResponse version =
  do
    fieldindex <- deserialize
    fielderrorcode <- deserialize
    fieldbaseoffset <- deserialize
    fieldlogappendtimems <- if version >= 2
      then deserialize
      else pure ((-1))
    fieldlogstartoffset <- if version >= 5
      then deserialize
      else pure ((-1))
    fieldrecorderrors <- if version >= 8
      then P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeBatchIndexAndErrorMessage
      else pure (P.mkKafkaArray V.empty)
    fielderrormessage <- if version >= 8
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldcurrentleader <- if version >= 10
      then decodeLeaderIdAndEpoch version
      else pure (LeaderIdAndEpoch { leaderIdAndEpochLeaderId = (-1), leaderIdAndEpochLeaderEpoch = (-1) })
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionProduceResponse
      {
      partitionProduceResponseIndex = fieldindex
      ,
      partitionProduceResponseErrorCode = fielderrorcode
      ,
      partitionProduceResponseBaseOffset = fieldbaseoffset
      ,
      partitionProduceResponseLogAppendTimeMs = fieldlogappendtimems
      ,
      partitionProduceResponseLogStartOffset = fieldlogstartoffset
      ,
      partitionProduceResponseRecordErrors = fieldrecorderrors
      ,
      partitionProduceResponseErrorMessage = fielderrormessage
      ,
      partitionProduceResponseCurrentLeader = fieldcurrentleader
      }


-- | Each produce response.
data TopicProduceResponse = TopicProduceResponse
  {

  -- | The topic name.

  -- Versions: 0-12
  topicProduceResponseName :: !(KafkaString)
,

  -- | The unique topic ID

  -- Versions: 13+
  topicProduceResponseTopicId :: !(KafkaUuid)
,

  -- | Each partition that we produced to within the topic.

  -- Versions: 0+
  topicProduceResponsePartitionResponses :: !(KafkaArray (PartitionProduceResponse))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicProduceResponse with version-aware field handling.
encodeTopicProduceResponse :: MonadPut m => E.ApiVersion -> TopicProduceResponse -> m ()
encodeTopicProduceResponse version tmsg =
  do
    when (version >= 0 && version <= 12) $
      if version >= 9 then serialize (toCompactString (topicProduceResponseName tmsg)) else serialize (topicProduceResponseName tmsg)
    when (version >= 13) $
      serialize (topicProduceResponseTopicId tmsg)
    E.encodeVersionedArray version 9 encodePartitionProduceResponse (case P.unKafkaArray (topicProduceResponsePartitionResponses tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicProduceResponse with version-aware field handling.
decodeTopicProduceResponse :: MonadGet m => E.ApiVersion -> m TopicProduceResponse
decodeTopicProduceResponse version =
  do
    fieldname <- if version >= 0 && version <= 12
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 13
      then deserialize
      else pure (P.nullUuid)
    fieldpartitionresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodePartitionProduceResponse
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicProduceResponse
      {
      topicProduceResponseName = fieldname
      ,
      topicProduceResponseTopicId = fieldtopicid
      ,
      topicProduceResponsePartitionResponses = fieldpartitionresponses
      }


-- | Endpoints for all current-leaders enumerated in PartitionProduceResponses, with errors NOT_LEADER_OR_FOLLOWER.
data NodeEndpoint = NodeEndpoint
  {

  -- | The ID of the associated node.

  -- Versions: 10+
  nodeEndpointNodeId :: !(Int32)
,

  -- | The node's hostname.

  -- Versions: 10+
  nodeEndpointHost :: !(KafkaString)
,

  -- | The node's port.

  -- Versions: 10+
  nodeEndpointPort :: !(Int32)
,

  -- | The rack of the node, or null if it has not been assigned to a rack.

  -- Versions: 10+
  nodeEndpointRack :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode NodeEndpoint with version-aware field handling.
encodeNodeEndpoint :: MonadPut m => E.ApiVersion -> NodeEndpoint -> m ()
encodeNodeEndpoint version nmsg =
  do
    when (version >= 10) $
      serialize (nodeEndpointNodeId nmsg)
    when (version >= 10) $
      if version >= 9 then serialize (toCompactString (nodeEndpointHost nmsg)) else serialize (nodeEndpointHost nmsg)
    when (version >= 10) $
      serialize (nodeEndpointPort nmsg)
    when (version >= 10) $
      if version >= 9 then serialize (toCompactString (nodeEndpointRack nmsg)) else serialize (nodeEndpointRack nmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode NodeEndpoint with version-aware field handling.
decodeNodeEndpoint :: MonadGet m => E.ApiVersion -> m NodeEndpoint
decodeNodeEndpoint version =
  do
    fieldnodeid <- if version >= 10
      then deserialize
      else pure (0)
    fieldhost <- if version >= 10
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldport <- if version >= 10
      then deserialize
      else pure (0)
    fieldrack <- if version >= 10
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
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



data ProduceResponse = ProduceResponse
  {

  -- | Each produce response.

  -- Versions: 0+
  produceResponseResponses :: !(KafkaArray (TopicProduceResponse))
,

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  produceResponseThrottleTimeMs :: !(Int32)
,

  -- | Endpoints for all current-leaders enumerated in PartitionProduceResponses, with errors NOT_LEADER_OR

  -- Versions: 10+
  produceResponseNodeEndpoints :: !(KafkaArray (NodeEndpoint))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ProduceResponse.
maxProduceResponseVersion :: Int16
maxProduceResponseVersion = 13

-- | Encode ProduceResponse with the given API version.
encodeProduceResponse :: MonadPut m => E.ApiVersion -> ProduceResponse -> m ()
encodeProduceResponse version msg
  | version == 9 =
    do
      E.encodeVersionedArray version 9 encodeTopicProduceResponse (case P.unKafkaArray (produceResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (produceResponseThrottleTimeMs msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 10 && version <= 13 =
    do
      E.encodeVersionedArray version 9 encodeTopicProduceResponse (case P.unKafkaArray (produceResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (produceResponseThrottleTimeMs msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 3 && version <= 8 =
    do
      E.encodeVersionedArray version 9 encodeTopicProduceResponse (case P.unKafkaArray (produceResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (produceResponseThrottleTimeMs msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ProduceResponse with the given API version.
decodeProduceResponse :: MonadGet m => E.ApiVersion -> m ProduceResponse
decodeProduceResponse version
  | version == 9 =
    do
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeTopicProduceResponse
      fieldthrottletimems <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ProduceResponse
        {
        produceResponseResponses = fieldresponses
        ,
        produceResponseThrottleTimeMs = fieldthrottletimems
        ,
        produceResponseNodeEndpoints = P.mkKafkaArray V.empty
        }

  | version >= 10 && version <= 13 =
    do
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeTopicProduceResponse
      fieldthrottletimems <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ProduceResponse
        {
        produceResponseResponses = fieldresponses
        ,
        produceResponseThrottleTimeMs = fieldthrottletimems
        ,
        produceResponseNodeEndpoints = P.mkKafkaArray V.empty
        }

  | version >= 3 && version <= 8 =
    do
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeTopicProduceResponse
      fieldthrottletimems <- deserialize
      pure ProduceResponse
        {
        produceResponseResponses = fieldresponses
        ,
        produceResponseThrottleTimeMs = fieldthrottletimems
        ,
        produceResponseNodeEndpoints = P.mkKafkaArray V.empty
        }
  | otherwise = fail $ "Unsupported version: " ++ show version