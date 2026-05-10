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
    maxEndQuorumEpochRequestVersion
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

-- | KafkaMessage instance for EndQuorumEpochRequest.
instance KafkaMessage EndQuorumEpochRequest where
  messageApiKey = 54
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 1

-- | Worst-case wire size of a ReplicaInfo.
wireMaxSizeReplicaInfo :: Int -> ReplicaInfo -> Int
wireMaxSizeReplicaInfo _version msg =
  0
  + 4
  + 16
  + 1

-- | Direct-poke encoder for ReplicaInfo.
wirePokeReplicaInfo :: Int -> Ptr Word8 -> ReplicaInfo -> IO (Ptr Word8)
wirePokeReplicaInfo version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 1 then W.pokeInt32BE p0 (replicaInfoCandidateId msg) else pure p0)
  p2 <- (if version >= 1 then WP.pokeKafkaUuid p1 (replicaInfoCandidateDirectoryId msg) else pure p1)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReplicaInfo.
wirePeekReplicaInfo :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReplicaInfo, Ptr Word8)
wirePeekReplicaInfo version _fp _basePtr p0 endPtr = do
  (f0_candidateid, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
  (f1_candidatedirectoryid, p2) <- (if version >= 1 then WP.peekKafkaUuid p1 endPtr else pure (P.nullUuid, p1))
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReplicaInfo { replicaInfoCandidateId = f0_candidateid, replicaInfoCandidateDirectoryId = f1_candidatedirectoryid }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultReplicaInfo :: ReplicaInfo
defaultReplicaInfo = ReplicaInfo { replicaInfoCandidateId = 0, replicaInfoCandidateDirectoryId = P.nullUuid }

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 4
  + 4
  + (5 + (case P.unKafkaArray (partitionDataPreferredSuccessors msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (partitionDataPreferredCandidates msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReplicaInfo _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt32BE p1 (partitionDataLeaderId msg)
  p3 <- W.pokeInt32BE p2 (partitionDataLeaderEpoch msg)
  p4 <- (if version == 0 then WP.pokeVersionedArray version 1 W.pokeInt32BE p3 (partitionDataPreferredSuccessors msg) else pure p3)
  p5 <- (if version >= 1 then WP.pokeVersionedArray version 1 (\p x -> wirePokeReplicaInfo version p x) p4 (partitionDataPreferredCandidates msg) else pure p4)
  if version >= 1 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_leaderid, p2) <- W.peekInt32BE p1 endPtr
  (f2_leaderepoch, p3) <- W.peekInt32BE p2 endPtr
  (f3_preferredsuccessors, p4) <- (if version == 0 then WP.peekVersionedArray version 1 W.peekInt32BE p3 endPtr else pure (P.mkKafkaArray V.empty, p3))
  (f4_preferredcandidates, p5) <- (if version >= 1 then WP.peekVersionedArray version 1 (\p e -> wirePeekReplicaInfo version _fp _basePtr p e) p4 endPtr else pure (P.mkKafkaArray V.empty, p4))
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataLeaderId = f1_leaderid, partitionDataLeaderEpoch = f2_leaderepoch, partitionDataPreferredSuccessors = f3_preferredsuccessors, partitionDataPreferredCandidates = f4_preferredcandidates }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultPartitionData :: PartitionData
defaultPartitionData = PartitionData { partitionDataPartitionIndex = 0, partitionDataLeaderId = 0, partitionDataLeaderEpoch = 0, partitionDataPreferredSuccessors = P.mkKafkaArray V.empty, partitionDataPreferredCandidates = P.mkKafkaArray V.empty }

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
  p1 <- (if version >= 1 then WP.pokeCompactString p0 (P.toCompactString (topicDataTopicName msg)) else WP.pokeKafkaString p0 (topicDataTopicName msg))
  p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokePartitionData version p x) p1 (topicDataPartitions msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicData.
wirePeekTopicData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicData, Ptr Word8)
wirePeekTopicData version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (if version >= 1 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicData { topicDataTopicName = f0_topicname, topicDataPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultTopicData :: TopicData
defaultTopicData = TopicData { topicDataTopicName = P.KafkaString Null, topicDataPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a LeaderEndpoint.
wireMaxSizeLeaderEndpoint :: Int -> LeaderEndpoint -> Int
wireMaxSizeLeaderEndpoint _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (leaderEndpointName msg))
  + WP.compactStringMaxSize (P.toCompactString (leaderEndpointHost msg))
  + 2
  + 1

-- | Direct-poke encoder for LeaderEndpoint.
wirePokeLeaderEndpoint :: Int -> Ptr Word8 -> LeaderEndpoint -> IO (Ptr Word8)
wirePokeLeaderEndpoint version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 1 then (if version >= 1 then WP.pokeCompactString p0 (P.toCompactString (leaderEndpointName msg)) else WP.pokeKafkaString p0 (leaderEndpointName msg)) else pure p0)
  p2 <- (if version >= 1 then (if version >= 1 then WP.pokeCompactString p1 (P.toCompactString (leaderEndpointHost msg)) else WP.pokeKafkaString p1 (leaderEndpointHost msg)) else pure p1)
  p3 <- (if version >= 1 then W.pokeWord16BE p2 (leaderEndpointPort msg) else pure p2)
  if version >= 1 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for LeaderEndpoint.
wirePeekLeaderEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaderEndpoint, Ptr Word8)
wirePeekLeaderEndpoint version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 1 then (if version >= 1 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_host, p2) <- (if version >= 1 then (if version >= 1 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr) else pure (P.KafkaString Null, p1))
  (f2_port, p3) <- (if version >= 1 then W.peekWord16BE p2 endPtr else pure (0, p2))
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (LeaderEndpoint { leaderEndpointName = f0_name, leaderEndpointHost = f1_host, leaderEndpointPort = f2_port }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultLeaderEndpoint :: LeaderEndpoint
defaultLeaderEndpoint = LeaderEndpoint { leaderEndpointName = P.KafkaString Null, leaderEndpointHost = P.KafkaString Null, leaderEndpointPort = 0 }

-- | Worst-case wire size of a EndQuorumEpochRequest.
wireMaxSizeEndQuorumEpochRequest :: Int -> EndQuorumEpochRequest -> Int
wireMaxSizeEndQuorumEpochRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (endQuorumEpochRequestClusterId msg))
  + (5 + (case P.unKafkaArray (endQuorumEpochRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicData _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (endQuorumEpochRequestLeaderEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeLeaderEndpoint _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for EndQuorumEpochRequest.
wirePokeEndQuorumEpochRequest :: Int -> Ptr Word8 -> EndQuorumEpochRequest -> IO (Ptr Word8)
wirePokeEndQuorumEpochRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then WP.pokeCompactString p0 (P.toCompactString (endQuorumEpochRequestClusterId msg)) else WP.pokeKafkaString p0 (endQuorumEpochRequestClusterId msg))
    p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeTopicData version p x) p1 (endQuorumEpochRequestTopics msg)
    pure p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then WP.pokeCompactString p0 (P.toCompactString (endQuorumEpochRequestClusterId msg)) else WP.pokeKafkaString p0 (endQuorumEpochRequestClusterId msg))
    p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeTopicData version p x) p1 (endQuorumEpochRequestTopics msg)
    p3 <- (if version >= 1 then WP.pokeVersionedArray version 1 (\p x -> wirePokeLeaderEndpoint version p x) p2 (endQuorumEpochRequestLeaderEndpoints msg) else pure p2)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke EndQuorumEpochRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for EndQuorumEpochRequest.
wirePeekEndQuorumEpochRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EndQuorumEpochRequest, Ptr Word8)
wirePeekEndQuorumEpochRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_clusterid, p1) <- (if version >= 1 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_topics, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekTopicData version _fp _basePtr p e) p1 endPtr
    pure (EndQuorumEpochRequest { endQuorumEpochRequestClusterId = f0_clusterid, endQuorumEpochRequestTopics = f1_topics, endQuorumEpochRequestLeaderEndpoints = P.mkKafkaArray V.empty }, p2)
  | version == 1 = do
    (f0_clusterid, p1) <- (if version >= 1 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_topics, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekTopicData version _fp _basePtr p e) p1 endPtr
    (f2_leaderendpoints, p3) <- (if version >= 1 then WP.peekVersionedArray version 1 (\p e -> wirePeekLeaderEndpoint version _fp _basePtr p e) p2 endPtr else pure (P.mkKafkaArray V.empty, p2))
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (EndQuorumEpochRequest { endQuorumEpochRequestClusterId = f0_clusterid, endQuorumEpochRequestTopics = f1_topics, endQuorumEpochRequestLeaderEndpoints = f2_leaderendpoints }, pTagsEnd)
  | otherwise = error $ "wirePeek EndQuorumEpochRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec EndQuorumEpochRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeEndQuorumEpochRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeEndQuorumEpochRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekEndQuorumEpochRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}