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
    maxDescribeQuorumResponseVersion
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , Nullable(..)
  )
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
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
  p2 <- (if version >= 2 then WP.pokeKafkaUuid p1 (replicaStateReplicaDirectoryId msg) else pure p1)
  p3 <- W.pokeInt64BE p2 (replicaStateLogEndOffset msg)
  p4 <- (if version >= 1 then W.pokeInt64BE p3 (replicaStateLastFetchTimestamp msg) else pure p3)
  p5 <- (if version >= 1 then W.pokeInt64BE p4 (replicaStateLastCaughtUpTimestamp msg) else pure p4)
  if version >= 0 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for ReplicaState.
wirePeekReplicaState :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReplicaState, Ptr Word8)
wirePeekReplicaState version _fp _basePtr p0 endPtr = do
  (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
  (f1_replicadirectoryid, p2) <- (if version >= 2 then WP.peekKafkaUuid p1 endPtr else pure (P.nullUuid, p1))
  (f2_logendoffset, p3) <- W.peekInt64BE p2 endPtr
  (f3_lastfetchtimestamp, p4) <- (if version >= 1 then W.peekInt64BE p3 endPtr else pure (0, p3))
  (f4_lastcaughtuptimestamp, p5) <- (if version >= 1 then W.peekInt64BE p4 endPtr else pure (0, p4))
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (ReplicaState { replicaStateReplicaId = f0_replicaid, replicaStateReplicaDirectoryId = f1_replicadirectoryid, replicaStateLogEndOffset = f2_logendoffset, replicaStateLastFetchTimestamp = f3_lastfetchtimestamp, replicaStateLastCaughtUpTimestamp = f4_lastcaughtuptimestamp }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultReplicaState :: ReplicaState
defaultReplicaState = ReplicaState { replicaStateReplicaId = 0, replicaStateReplicaDirectoryId = P.nullUuid, replicaStateLogEndOffset = 0, replicaStateLastFetchTimestamp = 0, replicaStateLastCaughtUpTimestamp = 0 }

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
  p3 <- (if version >= 2 then (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (partitionDataErrorMessage msg)) else WP.pokeKafkaString p2 (partitionDataErrorMessage msg)) else pure p2)
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
  (f2_errormessage, p3) <- (if version >= 2 then (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr) else pure (P.KafkaString Null, p2))
  (f3_leaderid, p4) <- W.peekInt32BE p3 endPtr
  (f4_leaderepoch, p5) <- W.peekInt32BE p4 endPtr
  (f5_highwatermark, p6) <- W.peekInt64BE p5 endPtr
  (f6_currentvoters, p7) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReplicaState version _fp _basePtr p e) p6 endPtr
  (f7_observers, p8) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReplicaState version _fp _basePtr p e) p7 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p8 endPtr else pure p8
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataErrorCode = f1_errorcode, partitionDataErrorMessage = f2_errormessage, partitionDataLeaderId = f3_leaderid, partitionDataLeaderEpoch = f4_leaderepoch, partitionDataHighWatermark = f5_highwatermark, partitionDataCurrentVoters = f6_currentvoters, partitionDataObservers = f7_observers }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultPartitionData :: PartitionData
defaultPartitionData = PartitionData { partitionDataPartitionIndex = 0, partitionDataErrorCode = 0, partitionDataErrorMessage = P.KafkaString Null, partitionDataLeaderId = 0, partitionDataLeaderEpoch = 0, partitionDataHighWatermark = 0, partitionDataCurrentVoters = P.mkKafkaArray V.empty, partitionDataObservers = P.mkKafkaArray V.empty }

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
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (topicDataTopicName msg)) else WP.pokeKafkaString p0 (topicDataTopicName msg))
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionData version p x) p1 (topicDataPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicData.
wirePeekTopicData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicData, Ptr Word8)
wirePeekTopicData version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicData { topicDataTopicName = f0_topicname, topicDataPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultTopicData :: TopicData
defaultTopicData = TopicData { topicDataTopicName = P.KafkaString Null, topicDataPartitions = P.mkKafkaArray V.empty }

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
  p1 <- (if version >= 2 then (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (listenerName msg)) else WP.pokeKafkaString p0 (listenerName msg)) else pure p0)
  p2 <- (if version >= 2 then (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (listenerHost msg)) else WP.pokeKafkaString p1 (listenerHost msg)) else pure p1)
  p3 <- (if version >= 2 then W.pokeWord16BE p2 (listenerPort msg) else pure p2)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for Listener.
wirePeekListener :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Listener, Ptr Word8)
wirePeekListener version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 2 then (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_host, p2) <- (if version >= 2 then (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
  (f2_port, p3) <- (if version >= 2 then W.peekWord16BE p2 endPtr else pure (0, p2))
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (Listener { listenerName = f0_name, listenerHost = f1_host, listenerPort = f2_port }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultListener :: Listener
defaultListener = Listener { listenerName = P.KafkaString Null, listenerHost = P.KafkaString Null, listenerPort = 0 }

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
  p1 <- (if version >= 2 then W.pokeInt32BE p0 (nodeNodeId msg) else pure p0)
  p2 <- (if version >= 2 then WP.pokeVersionedArray version 0 (\p x -> wirePokeListener version p x) p1 (nodeListeners msg) else pure p1)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for Node.
wirePeekNode :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Node, Ptr Word8)
wirePeekNode version _fp _basePtr p0 endPtr = do
  (f0_nodeid, p1) <- (if version >= 2 then W.peekInt32BE p0 endPtr else pure (0, p0))
  (f1_listeners, p2) <- (if version >= 2 then WP.peekVersionedArray version 0 (\p e -> wirePeekListener version _fp _basePtr p e) p1 endPtr else pure (P.mkKafkaArray V.empty, p1))
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (Node { nodeNodeId = f0_nodeid, nodeListeners = f1_listeners }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultNode :: Node
defaultNode = Node { nodeNodeId = 0, nodeListeners = P.mkKafkaArray V.empty }

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
    p2 <- (if version >= 2 then (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (describeQuorumResponseErrorMessage msg)) else WP.pokeKafkaString p1 (describeQuorumResponseErrorMessage msg)) else pure p1)
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicData version p x) p2 (describeQuorumResponseTopics msg)
    p4 <- (if version >= 2 then WP.pokeVersionedArray version 0 (\p x -> wirePokeNode version p x) p3 (describeQuorumResponseNodes msg) else pure p3)
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
    (f1_errormessage, p2) <- (if version >= 2 then (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
    (f2_topics, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicData version _fp _basePtr p e) p2 endPtr
    (f3_nodes, p4) <- (if version >= 2 then WP.peekVersionedArray version 0 (\p e -> wirePeekNode version _fp _basePtr p e) p3 endPtr else pure (P.mkKafkaArray V.empty, p3))
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
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeQuorumResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeQuorumResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeQuorumResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeQuorumResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}