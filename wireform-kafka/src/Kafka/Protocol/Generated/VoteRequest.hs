{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.VoteRequest
Description : Kafka VoteRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 52.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.VoteRequest
  (
    VoteRequest(..),
    TopicData(..),
    PartitionData(..),
    maxVoteRequestVersion
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


-- | The partition data.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The epoch of the voter sending the request

  -- Versions: 0+
  partitionDataReplicaEpoch :: !(Int32)
,

  -- | The replica id of the voter sending the request

  -- Versions: 0+
  partitionDataReplicaId :: !(Int32)
,

  -- | The directory id of the voter sending the request

  -- Versions: 1+
  partitionDataReplicaDirectoryId :: !(KafkaUuid)
,

  -- | The directory id of the voter receiving the request

  -- Versions: 1+
  partitionDataVoterDirectoryId :: !(KafkaUuid)
,

  -- | The epoch of the last record written to the metadata log.

  -- Versions: 0+
  partitionDataLastOffsetEpoch :: !(Int32)
,

  -- | The log end offset of the metadata log of the voter sending the request.

  -- Versions: 0+
  partitionDataLastOffset :: !(Int64)
,

  -- | Whether the request is a PreVote request (not persisted) or not.

  -- Versions: 2+
  partitionDataPreVote :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | The topic data.
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


data VoteRequest = VoteRequest
  {

  -- | The cluster id.

  -- Versions: 0+
  voteRequestClusterId :: !(KafkaString)
,

  -- | The replica id of the voter receiving the request.

  -- Versions: 1+
  voteRequestVoterId :: !(Int32)
,

  -- | The topic data.

  -- Versions: 0+
  voteRequestTopics :: !(KafkaArray (TopicData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for VoteRequest.
maxVoteRequestVersion :: Int16
maxVoteRequestVersion = 2

-- | KafkaMessage instance for VoteRequest.
instance KafkaMessage VoteRequest where
  messageApiKey = 52
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 4
  + 4
  + 16
  + 16
  + 4
  + 8
  + 1
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt32BE p1 (partitionDataReplicaEpoch msg)
  p3 <- W.pokeInt32BE p2 (partitionDataReplicaId msg)
  p4 <- (if version >= 1 then WP.pokeKafkaUuid p3 (partitionDataReplicaDirectoryId msg) else pure p3)
  p5 <- (if version >= 1 then WP.pokeKafkaUuid p4 (partitionDataVoterDirectoryId msg) else pure p4)
  p6 <- W.pokeInt32BE p5 (partitionDataLastOffsetEpoch msg)
  p7 <- W.pokeInt64BE p6 (partitionDataLastOffset msg)
  p8 <- (if version >= 2 then W.pokeWord8 p7 (if (partitionDataPreVote msg) then 1 else 0) else pure p7)
  if version >= 0 then WP.pokeEmptyTaggedFields p8 else pure p8

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_replicaepoch, p2) <- W.peekInt32BE p1 endPtr
  (f2_replicaid, p3) <- W.peekInt32BE p2 endPtr
  (f3_replicadirectoryid, p4) <- (if version >= 1 then WP.peekKafkaUuid p3 endPtr else pure (P.nullUuid, p3))
  (f4_voterdirectoryid, p5) <- (if version >= 1 then WP.peekKafkaUuid p4 endPtr else pure (P.nullUuid, p4))
  (f5_lastoffsetepoch, p6) <- W.peekInt32BE p5 endPtr
  (f6_lastoffset, p7) <- W.peekInt64BE p6 endPtr
  (f7_prevote, p8) <- (if version >= 2 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p7 endPtr else pure (False, p7))
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p8 endPtr else pure p8
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataReplicaEpoch = f1_replicaepoch, partitionDataReplicaId = f2_replicaid, partitionDataReplicaDirectoryId = f3_replicadirectoryid, partitionDataVoterDirectoryId = f4_voterdirectoryid, partitionDataLastOffsetEpoch = f5_lastoffsetepoch, partitionDataLastOffset = f6_lastoffset, partitionDataPreVote = f7_prevote }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultPartitionData :: PartitionData
defaultPartitionData = PartitionData { partitionDataPartitionIndex = 0, partitionDataReplicaEpoch = 0, partitionDataReplicaId = 0, partitionDataReplicaDirectoryId = P.nullUuid, partitionDataVoterDirectoryId = P.nullUuid, partitionDataLastOffsetEpoch = 0, partitionDataLastOffset = 0, partitionDataPreVote = False }

-- | Worst-case wire size of a TopicData.
wireMaxSizeTopicData :: Int -> TopicData -> Int
wireMaxSizeTopicData _version msg =
  0
  + WP.dualStringMaxSize (topicDataTopicName msg)
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

-- | Worst-case wire size of a VoteRequest.
wireMaxSizeVoteRequest :: Int -> VoteRequest -> Int
wireMaxSizeVoteRequest _version msg =
  0
  + WP.dualStringMaxSize (voteRequestClusterId msg)
  + 4
  + (5 + (case P.unKafkaArray (voteRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for VoteRequest.
wirePokeVoteRequest :: Int -> Ptr Word8 -> VoteRequest -> IO (Ptr Word8)
wirePokeVoteRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (voteRequestClusterId msg)) else WP.pokeKafkaString p0 (voteRequestClusterId msg))
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicData version p x) p1 (voteRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (voteRequestClusterId msg)) else WP.pokeKafkaString p0 (voteRequestClusterId msg))
    p2 <- (if version >= 1 then W.pokeInt32BE p1 (voteRequestVoterId msg) else pure p1)
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicData version p x) p2 (voteRequestTopics msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke VoteRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for VoteRequest.
wirePeekVoteRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (VoteRequest, Ptr Word8)
wirePeekVoteRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_clusterid, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicData version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (VoteRequest { voteRequestClusterId = f0_clusterid, voteRequestVoterId = 0, voteRequestTopics = f1_topics }, pTagsEnd)
  | version >= 1 && version <= 2 = do
    (f0_clusterid, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_voterid, p2) <- (if version >= 1 then W.peekInt32BE p1 endPtr else pure (0, p1))
    (f2_topics, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicData version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (VoteRequest { voteRequestClusterId = f0_clusterid, voteRequestVoterId = f1_voterid, voteRequestTopics = f2_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek VoteRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec VoteRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeVoteRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeVoteRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekVoteRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}