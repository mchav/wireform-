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
    maxBeginQuorumEpochRequestVersion
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

-- | KafkaMessage instance for BeginQuorumEpochRequest.
instance KafkaMessage BeginQuorumEpochRequest where
  messageApiKey = 53
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 1

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 16
  + 4
  + 4
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- WP.pokeKafkaUuid p1 (partitionDataVoterDirectoryId msg)
  p3 <- W.pokeInt32BE p2 (partitionDataLeaderId msg)
  p4 <- W.pokeInt32BE p3 (partitionDataLeaderEpoch msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_voterdirectoryid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_leaderid, p3) <- W.peekInt32BE p2 endPtr
  (f3_leaderepoch, p4) <- W.peekInt32BE p3 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataVoterDirectoryId = f1_voterdirectoryid, partitionDataLeaderId = f2_leaderid, partitionDataLeaderEpoch = f3_leaderepoch }, pTagsEnd)

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
  p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokePartitionData version p x) p1 (topicDataPartitions msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicData.
wirePeekTopicData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicData, Ptr Word8)
wirePeekTopicData version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicData { topicDataTopicName = f0_topicname, topicDataPartitions = f1_partitions }, pTagsEnd)

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
  p1 <- WP.pokeCompactString p0 (P.toCompactString (leaderEndpointName msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (leaderEndpointHost msg))
  p3 <- W.pokeWord16BE p2 (leaderEndpointPort msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for LeaderEndpoint.
wirePeekLeaderEndpoint :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (LeaderEndpoint, Ptr Word8)
wirePeekLeaderEndpoint version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_host, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_port, p3) <- W.peekWord16BE p2 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (LeaderEndpoint { leaderEndpointName = f0_name, leaderEndpointHost = f1_host, leaderEndpointPort = f2_port }, pTagsEnd)

-- | Worst-case wire size of a BeginQuorumEpochRequest.
wireMaxSizeBeginQuorumEpochRequest :: Int -> BeginQuorumEpochRequest -> Int
wireMaxSizeBeginQuorumEpochRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (beginQuorumEpochRequestClusterId msg))
  + 4
  + (5 + (case P.unKafkaArray (beginQuorumEpochRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicData _version x ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (beginQuorumEpochRequestLeaderEndpoints msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeLeaderEndpoint _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for BeginQuorumEpochRequest.
wirePokeBeginQuorumEpochRequest :: Int -> Ptr Word8 -> BeginQuorumEpochRequest -> IO (Ptr Word8)
wirePokeBeginQuorumEpochRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (beginQuorumEpochRequestClusterId msg))
    p2 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeTopicData version p x) p1 (beginQuorumEpochRequestTopics msg)
    pure p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (beginQuorumEpochRequestClusterId msg))
    p2 <- W.pokeInt32BE p1 (beginQuorumEpochRequestVoterId msg)
    p3 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeTopicData version p x) p2 (beginQuorumEpochRequestTopics msg)
    p4 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeLeaderEndpoint version p x) p3 (beginQuorumEpochRequestLeaderEndpoints msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke BeginQuorumEpochRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for BeginQuorumEpochRequest.
wirePeekBeginQuorumEpochRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (BeginQuorumEpochRequest, Ptr Word8)
wirePeekBeginQuorumEpochRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_clusterid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 1 (\p e -> wirePeekTopicData version _fp _basePtr p e) p1 endPtr
    pure (BeginQuorumEpochRequest { beginQuorumEpochRequestClusterId = f0_clusterid, beginQuorumEpochRequestVoterId = 0, beginQuorumEpochRequestTopics = f1_topics, beginQuorumEpochRequestLeaderEndpoints = P.mkKafkaArray V.empty }, p2)
  | version == 1 = do
    (f0_clusterid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_voterid, p2) <- W.peekInt32BE p1 endPtr
    (f2_topics, p3) <- WP.peekVersionedArray version 1 (\p e -> wirePeekTopicData version _fp _basePtr p e) p2 endPtr
    (f3_leaderendpoints, p4) <- WP.peekVersionedArray version 1 (\p e -> wirePeekLeaderEndpoint version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (BeginQuorumEpochRequest { beginQuorumEpochRequestClusterId = f0_clusterid, beginQuorumEpochRequestVoterId = f1_voterid, beginQuorumEpochRequestTopics = f2_topics, beginQuorumEpochRequestLeaderEndpoints = f3_leaderendpoints }, pTagsEnd)
  | otherwise = error $ "wirePeek BeginQuorumEpochRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec BeginQuorumEpochRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeBeginQuorumEpochRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeBeginQuorumEpochRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekBeginQuorumEpochRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}