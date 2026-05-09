{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeQuorumResponse
Description : Kafka DescribeQuorumResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 55.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeQuorumResponse
  (
    DescribeQuorumResponse(..),
    TopicData(..),
    PartitionData(..),
    ReplicaState(..),
    Node(..),
    Listener(..),
    encodeDescribeQuorumResponse,
    decodeDescribeQuorumResponse,
    maxDescribeQuorumResponseVersion
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


data ReplicaState = ReplicaState
  {

  -- | The ID of the replica.

  -- Versions: 0+
  replicaStateReplicaId :: !(Int32)
,

  -- | The replica directory ID of the replica.

  -- Versions: 2+
  replicaStateReplicaDirectoryId :: !(KafkaUuid)
,

  -- | The last known log end offset of the follower or -1 if it is unknown.

  -- Versions: 0+
  replicaStateLogEndOffset :: !(Int64)
,

  -- | The last known leader wall clock time time when a follower fetched from the leader. This is reported

  -- Versions: 1+
  replicaStateLastFetchTimestamp :: !(Int64)
,

  -- | The leader wall clock append time of the offset for which the follower made the most recent fetch re

  -- Versions: 1+
  replicaStateLastCaughtUpTimestamp :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode ReplicaState with version-aware field handling.
encodeReplicaState :: MonadPut m => E.ApiVersion -> ReplicaState -> m ()
encodeReplicaState version rmsg =
  do
    serialize (replicaStateReplicaId rmsg)
    when (version >= 2) $
      serialize (replicaStateReplicaDirectoryId rmsg)
    serialize (replicaStateLogEndOffset rmsg)
    when (version >= 1) $
      serialize (replicaStateLastFetchTimestamp rmsg)
    when (version >= 1) $
      serialize (replicaStateLastCaughtUpTimestamp rmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReplicaState with version-aware field handling.
decodeReplicaState :: MonadGet m => E.ApiVersion -> m ReplicaState
decodeReplicaState version =
  do
    fieldreplicaid <- deserialize
    fieldreplicadirectoryid <- if version >= 2
      then deserialize
      else pure (P.nullUuid)
    fieldlogendoffset <- deserialize
    fieldlastfetchtimestamp <- if version >= 1
      then deserialize
      else pure ((-1))
    fieldlastcaughtuptimestamp <- if version >= 1
      then deserialize
      else pure ((-1))
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReplicaState
      {
      replicaStateReplicaId = fieldreplicaid
      ,
      replicaStateReplicaDirectoryId = fieldreplicadirectoryid
      ,
      replicaStateLogEndOffset = fieldlogendoffset
      ,
      replicaStateLastFetchTimestamp = fieldlastfetchtimestamp
      ,
      replicaStateLastCaughtUpTimestamp = fieldlastcaughtuptimestamp
      }


-- | The partition data.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The partition error code.

  -- Versions: 0+
  partitionDataErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 2+
  partitionDataErrorMessage :: !(KafkaString)
,

  -- | The ID of the current leader or -1 if the leader is unknown.

  -- Versions: 0+
  partitionDataLeaderId :: !(Int32)
,

  -- | The latest known leader epoch.

  -- Versions: 0+
  partitionDataLeaderEpoch :: !(Int32)
,

  -- | The high water mark.

  -- Versions: 0+
  partitionDataHighWatermark :: !(Int64)
,

  -- | The current voters of the partition.

  -- Versions: 0+
  partitionDataCurrentVoters :: !(KafkaArray (ReplicaState))
,

  -- | The observers of the partition.

  -- Versions: 0+
  partitionDataObservers :: !(KafkaArray (ReplicaState))

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataErrorCode pmsg)
    when (version >= 2) $
      if version >= 0 then serialize (toCompactString (partitionDataErrorMessage pmsg)) else serialize (partitionDataErrorMessage pmsg)
    serialize (partitionDataLeaderId pmsg)
    serialize (partitionDataLeaderEpoch pmsg)
    serialize (partitionDataHighWatermark pmsg)
    E.encodeVersionedArray version 0 encodeReplicaState (case P.unKafkaArray (partitionDataCurrentVoters pmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 0 encodeReplicaState (case P.unKafkaArray (partitionDataObservers pmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 2
      then if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldleaderid <- deserialize
    fieldleaderepoch <- deserialize
    fieldhighwatermark <- deserialize
    fieldcurrentvoters <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReplicaState
    fieldobservers <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReplicaState
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataErrorCode = fielderrorcode
      ,
      partitionDataErrorMessage = fielderrormessage
      ,
      partitionDataLeaderId = fieldleaderid
      ,
      partitionDataLeaderEpoch = fieldleaderepoch
      ,
      partitionDataHighWatermark = fieldhighwatermark
      ,
      partitionDataCurrentVoters = fieldcurrentvoters
      ,
      partitionDataObservers = fieldobservers
      }


-- | The response from the describe quorum API.
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


-- | The listeners of this controller.
data Listener = Listener
  {

  -- | The name of the endpoint.

  -- Versions: 2+
  listenerName :: !(KafkaString)
,

  -- | The hostname.

  -- Versions: 2+
  listenerHost :: !(KafkaString)
,

  -- | The port.

  -- Versions: 2+
  listenerPort :: !(Word16)

  }
  deriving (Eq, Show, Generic)


-- | Encode Listener with version-aware field handling.
encodeListener :: MonadPut m => E.ApiVersion -> Listener -> m ()
encodeListener version lmsg =
  do
    when (version >= 2) $
      if version >= 0 then serialize (toCompactString (listenerName lmsg)) else serialize (listenerName lmsg)
    when (version >= 2) $
      if version >= 0 then serialize (toCompactString (listenerHost lmsg)) else serialize (listenerHost lmsg)
    when (version >= 2) $
      serialize (listenerPort lmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Listener with version-aware field handling.
decodeListener :: MonadGet m => E.ApiVersion -> m Listener
decodeListener version =
  do
    fieldname <- if version >= 2
      then if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldhost <- if version >= 2
      then if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldport <- if version >= 2
      then deserialize
      else pure (0)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Listener
      {
      listenerName = fieldname
      ,
      listenerHost = fieldhost
      ,
      listenerPort = fieldport
      }


-- | The nodes in the quorum.
data Node = Node
  {

  -- | The ID of the associated node.

  -- Versions: 2+
  nodeNodeId :: !(Int32)
,

  -- | The listeners of this controller.

  -- Versions: 2+
  nodeListeners :: !(KafkaArray (Listener))

  }
  deriving (Eq, Show, Generic)


-- | Encode Node with version-aware field handling.
encodeNode :: MonadPut m => E.ApiVersion -> Node -> m ()
encodeNode version nmsg =
  do
    when (version >= 2) $
      serialize (nodeNodeId nmsg)
    when (version >= 2) $
      E.encodeVersionedArray version 0 encodeListener (case P.unKafkaArray (nodeListeners nmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Node with version-aware field handling.
decodeNode :: MonadGet m => E.ApiVersion -> m Node
decodeNode version =
  do
    fieldnodeid <- if version >= 2
      then deserialize
      else pure (0)
    fieldlisteners <- if version >= 2
      then P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeListener
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Node
      {
      nodeNodeId = fieldnodeid
      ,
      nodeListeners = fieldlisteners
      }



data DescribeQuorumResponse = DescribeQuorumResponse
  {

  -- | The top level error code.

  -- Versions: 0+
  describeQuorumResponseErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 2+
  describeQuorumResponseErrorMessage :: !(KafkaString)
,

  -- | The response from the describe quorum API.

  -- Versions: 0+
  describeQuorumResponseTopics :: !(KafkaArray (TopicData))
,

  -- | The nodes in the quorum.

  -- Versions: 2+
  describeQuorumResponseNodes :: !(KafkaArray (Node))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeQuorumResponse.
maxDescribeQuorumResponseVersion :: Int16
maxDescribeQuorumResponseVersion = 2

-- | Encode DescribeQuorumResponse with the given API version.
encodeDescribeQuorumResponse :: MonadPut m => E.ApiVersion -> DescribeQuorumResponse -> m ()
encodeDescribeQuorumResponse version msg
  | version == 2 =
    do
      serialize (describeQuorumResponseErrorCode msg)
      serialize (toCompactString (describeQuorumResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeTopicData (case P.unKafkaArray (describeQuorumResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 0 encodeNode (case P.unKafkaArray (describeQuorumResponseNodes msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      serialize (describeQuorumResponseErrorCode msg)
      E.encodeVersionedArray version 0 encodeTopicData (case P.unKafkaArray (describeQuorumResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeQuorumResponse with the given API version.
decodeDescribeQuorumResponse :: MonadGet m => E.ApiVersion -> m DescribeQuorumResponse
decodeDescribeQuorumResponse version
  | version == 2 =
    do
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicData
      fieldnodes <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeNode
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeQuorumResponse
        {
        describeQuorumResponseErrorCode = fielderrorcode
        ,
        describeQuorumResponseErrorMessage = fielderrormessage
        ,
        describeQuorumResponseTopics = fieldtopics
        ,
        describeQuorumResponseNodes = fieldnodes
        }

  | version >= 0 && version <= 1 =
    do
      fielderrorcode <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeQuorumResponse
        {
        describeQuorumResponseErrorCode = fielderrorcode
        ,
        describeQuorumResponseErrorMessage = P.KafkaString Null
        ,
        describeQuorumResponseTopics = fieldtopics
        ,
        describeQuorumResponseNodes = P.mkKafkaArray V.empty
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeQuorumResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
