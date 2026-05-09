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
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


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

-- | KafkaMessage instance for DescribeQuorumResponse.
instance KafkaMessage DescribeQuorumResponse where
  messageApiKey = 55
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

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

-- | Worst-case wire size of a ReplicaState.
wireMaxSizeReplicaState :: Int -> ReplicaState -> Int
wireMaxSizeReplicaState _version msg =
  0
  + 4
  + 16
  + 8
  + 8
  + 8
  + 1

-- | Direct-poke encoder for ReplicaState.
wirePokeReplicaState :: Int -> Ptr Word8 -> ReplicaState -> IO (Ptr Word8)
wirePokeReplicaState version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (replicaStateReplicaId msg)
  p2 <- WP.pokeKafkaUuid p1 (replicaStateReplicaDirectoryId msg)
  p3 <- W.pokeInt64BE p2 (replicaStateLogEndOffset msg)
  p4 <- W.pokeInt64BE p3 (replicaStateLastFetchTimestamp msg)
  p5 <- W.pokeInt64BE p4 (replicaStateLastCaughtUpTimestamp msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for ReplicaState.
wirePeekReplicaState :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReplicaState, Ptr Word8)
wirePeekReplicaState version _fp _basePtr p0 endPtr = do
  (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
  (f1_replicadirectoryid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_logendoffset, p3) <- W.peekInt64BE p2 endPtr
  (f3_lastfetchtimestamp, p4) <- W.peekInt64BE p3 endPtr
  (f4_lastcaughtuptimestamp, p5) <- W.peekInt64BE p4 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (ReplicaState { replicaStateReplicaId = f0_replicaid, replicaStateReplicaDirectoryId = f1_replicadirectoryid, replicaStateLogEndOffset = f2_logendoffset, replicaStateLastFetchTimestamp = f3_lastfetchtimestamp, replicaStateLastCaughtUpTimestamp = f4_lastcaughtuptimestamp }, pTagsEnd)

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (partitionDataErrorMessage msg))
  + 4
  + 4
  + 8
  + (5 + (case P.unKafkaArray (partitionDataCurrentVoters msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReplicaState _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (partitionDataObservers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReplicaState _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (partitionDataErrorCode msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (partitionDataErrorMessage msg))
  p4 <- W.pokeInt32BE p3 (partitionDataLeaderId msg)
  p5 <- W.pokeInt32BE p4 (partitionDataLeaderEpoch msg)
  p6 <- W.pokeInt64BE p5 (partitionDataHighWatermark msg)
  p7 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReplicaState version p x) p6 (partitionDataCurrentVoters msg)
  p8 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReplicaState version p x) p7 (partitionDataObservers msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p8 else pure p8

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_leaderid, p4) <- W.peekInt32BE p3 endPtr
  (f4_leaderepoch, p5) <- W.peekInt32BE p4 endPtr
  (f5_highwatermark, p6) <- W.peekInt64BE p5 endPtr
  (f6_currentvoters, p7) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReplicaState version _fp _basePtr p e) p6 endPtr
  (f7_observers, p8) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReplicaState version _fp _basePtr p e) p7 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p8 endPtr else pure p8
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataErrorCode = f1_errorcode, partitionDataErrorMessage = f2_errormessage, partitionDataLeaderId = f3_leaderid, partitionDataLeaderEpoch = f4_leaderepoch, partitionDataHighWatermark = f5_highwatermark, partitionDataCurrentVoters = f6_currentvoters, partitionDataObservers = f7_observers }, pTagsEnd)

-- | Worst-case wire size of a TopicData.
wireMaxSizeTopicData :: Int -> TopicData -> Int
wireMaxSizeTopicData _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (topicDataTopicName msg))
  + (5 + (case P.unKafkaArray (topicDataPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicData.
wirePokeTopicData :: Int -> Ptr Word8 -> TopicData -> IO (Ptr Word8)
wirePokeTopicData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (topicDataTopicName msg))
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionData version p x) p1 (topicDataPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicData.
wirePeekTopicData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicData, Ptr Word8)
wirePeekTopicData version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicData { topicDataTopicName = f0_topicname, topicDataPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a Listener.
wireMaxSizeListener :: Int -> Listener -> Int
wireMaxSizeListener _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (listenerName msg))
  + WP.compactStringMaxSize (P.toCompactString (listenerHost msg))
  + 2
  + 1

-- | Direct-poke encoder for Listener.
wirePokeListener :: Int -> Ptr Word8 -> Listener -> IO (Ptr Word8)
wirePokeListener version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (listenerName msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (listenerHost msg))
  p3 <- W.pokeWord16BE p2 (listenerPort msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for Listener.
wirePeekListener :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Listener, Ptr Word8)
wirePeekListener version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_host, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_port, p3) <- W.peekWord16BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (Listener { listenerName = f0_name, listenerHost = f1_host, listenerPort = f2_port }, pTagsEnd)

-- | Worst-case wire size of a Node.
wireMaxSizeNode :: Int -> Node -> Int
wireMaxSizeNode _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (nodeListeners msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeListener _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for Node.
wirePokeNode :: Int -> Ptr Word8 -> Node -> IO (Ptr Word8)
wirePokeNode version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (nodeNodeId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeListener version p x) p1 (nodeListeners msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for Node.
wirePeekNode :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Node, Ptr Word8)
wirePeekNode version _fp _basePtr p0 endPtr = do
  (f0_nodeid, p1) <- W.peekInt32BE p0 endPtr
  (f1_listeners, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekListener version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (Node { nodeNodeId = f0_nodeid, nodeListeners = f1_listeners }, pTagsEnd)

-- | Worst-case wire size of a DescribeQuorumResponse.
wireMaxSizeDescribeQuorumResponse :: Int -> DescribeQuorumResponse -> Int
wireMaxSizeDescribeQuorumResponse _version msg =
  0
  + 2
  + WP.compactStringMaxSize (P.toCompactString (describeQuorumResponseErrorMessage msg))
  + (5 + (case P.unKafkaArray (describeQuorumResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicData _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (describeQuorumResponseNodes msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeNode _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeQuorumResponse.
wirePokeDescribeQuorumResponse :: Int -> Ptr Word8 -> DescribeQuorumResponse -> IO (Ptr Word8)
wirePokeDescribeQuorumResponse version basePtr msg
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (describeQuorumResponseErrorCode msg)
    p2 <- WP.pokeCompactString p1 (P.toCompactString (describeQuorumResponseErrorMessage msg))
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicData version p x) p2 (describeQuorumResponseTopics msg)
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeNode version p x) p3 (describeQuorumResponseNodes msg)
    WP.pokeEmptyTaggedFields p4
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (describeQuorumResponseErrorCode msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicData version p x) p1 (describeQuorumResponseTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke DescribeQuorumResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeQuorumResponse.
wirePeekDescribeQuorumResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeQuorumResponse, Ptr Word8)
wirePeekDescribeQuorumResponse version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_errormessage, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_topics, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicData version _fp _basePtr p e) p2 endPtr
    (f3_nodes, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekNode version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (DescribeQuorumResponse { describeQuorumResponseErrorCode = f0_errorcode, describeQuorumResponseErrorMessage = f1_errormessage, describeQuorumResponseTopics = f2_topics, describeQuorumResponseNodes = f3_nodes }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicData version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DescribeQuorumResponse { describeQuorumResponseErrorCode = f0_errorcode, describeQuorumResponseErrorMessage = P.KafkaString Null, describeQuorumResponseTopics = f1_topics, describeQuorumResponseNodes = P.mkKafkaArray V.empty }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeQuorumResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec DescribeQuorumResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeQuorumResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeQuorumResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeQuorumResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}