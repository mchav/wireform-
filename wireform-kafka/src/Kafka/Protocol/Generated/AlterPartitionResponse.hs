{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterPartitionResponse
Description : Kafka AlterPartitionResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 56.



Valid versions: 2-3
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterPartitionResponse
  (
    AlterPartitionResponse(..),
    TopicData(..),
    PartitionData(..),
    maxAlterPartitionResponseVersion
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


-- | The responses for each partition.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The partition level error code.

  -- Versions: 0+
  partitionDataErrorCode :: !(Int16)
,

  -- | The broker ID of the leader.

  -- Versions: 0+
  partitionDataLeaderId :: !(Int32)
,

  -- | The leader epoch.

  -- Versions: 0+
  partitionDataLeaderEpoch :: !(Int32)
,

  -- | The in-sync replica IDs.

  -- Versions: 0+
  partitionDataIsr :: !(KafkaArray (Int32))
,

  -- | 1 if the partition is recovering from an unclean leader election; 0 otherwise.

  -- Versions: 1+
  partitionDataLeaderRecoveryState :: !(Int8)
,

  -- | The current epoch for the partition for KRaft controllers.

  -- Versions: 0+
  partitionDataPartitionEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | The responses for each topic.
data TopicData = TopicData
  {

  -- | The ID of the topic.

  -- Versions: 2+
  topicDataTopicId :: !(KafkaUuid)
,

  -- | The responses for each partition.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


data AlterPartitionResponse = AlterPartitionResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  alterPartitionResponseThrottleTimeMs :: !(Int32)
,

  -- | The top level response error code.

  -- Versions: 0+
  alterPartitionResponseErrorCode :: !(Int16)
,

  -- | The responses for each topic.

  -- Versions: 0+
  alterPartitionResponseTopics :: !(KafkaArray (TopicData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterPartitionResponse.
maxAlterPartitionResponseVersion :: Int16
maxAlterPartitionResponseVersion = 3

-- | KafkaMessage instance for AlterPartitionResponse.
instance KafkaMessage AlterPartitionResponse where
  messageApiKey = 56
  messageMinVersion = 2
  messageMaxVersion = 3
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 2
  + 4
  + 4
  + (5 + (case P.unKafkaArray (partitionDataIsr msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1
  + 4
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (partitionDataErrorCode msg)
  p3 <- W.pokeInt32BE p2 (partitionDataLeaderId msg)
  p4 <- W.pokeInt32BE p3 (partitionDataLeaderEpoch msg)
  p5 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p4 (partitionDataIsr msg)
  p6 <- W.pokeWord8 p5 (fromIntegral (partitionDataLeaderRecoveryState msg))
  p7 <- W.pokeInt32BE p6 (partitionDataPartitionEpoch msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p7 else pure p7

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_leaderid, p3) <- W.peekInt32BE p2 endPtr
  (f3_leaderepoch, p4) <- W.peekInt32BE p3 endPtr
  (f4_isr, p5) <- WP.peekVersionedArray version 0 W.peekInt32BE p4 endPtr
  (f5_leaderrecoverystate, p6) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p5 endPtr
  (f6_partitionepoch, p7) <- W.peekInt32BE p6 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p7 endPtr else pure p7
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataErrorCode = f1_errorcode, partitionDataLeaderId = f2_leaderid, partitionDataLeaderEpoch = f3_leaderepoch, partitionDataIsr = f4_isr, partitionDataLeaderRecoveryState = f5_leaderrecoverystate, partitionDataPartitionEpoch = f6_partitionepoch }, pTagsEnd)

-- | Worst-case wire size of a TopicData.
wireMaxSizeTopicData :: Int -> TopicData -> Int
wireMaxSizeTopicData _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (topicDataPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicData.
wirePokeTopicData :: Int -> Ptr Word8 -> TopicData -> IO (Ptr Word8)
wirePokeTopicData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (topicDataTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionData version p x) p1 (topicDataPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicData.
wirePeekTopicData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicData, Ptr Word8)
wirePeekTopicData version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicData { topicDataTopicId = f0_topicid, topicDataPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a AlterPartitionResponse.
wireMaxSizeAlterPartitionResponse :: Int -> AlterPartitionResponse -> Int
wireMaxSizeAlterPartitionResponse _version msg =
  0
  + 4
  + 2
  + (5 + (case P.unKafkaArray (alterPartitionResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterPartitionResponse.
wirePokeAlterPartitionResponse :: Int -> Ptr Word8 -> AlterPartitionResponse -> IO (Ptr Word8)
wirePokeAlterPartitionResponse version basePtr msg
  | version >= 2 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterPartitionResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (alterPartitionResponseErrorCode msg)
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicData version p x) p2 (alterPartitionResponseTopics msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke AlterPartitionResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterPartitionResponse.
wirePeekAlterPartitionResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterPartitionResponse, Ptr Word8)
wirePeekAlterPartitionResponse version _fp _basePtr p0 endPtr
  | version >= 2 && version <= 3 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_topics, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicData version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (AlterPartitionResponse { alterPartitionResponseThrottleTimeMs = f0_throttletimems, alterPartitionResponseErrorCode = f1_errorcode, alterPartitionResponseTopics = f2_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek AlterPartitionResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AlterPartitionResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterPartitionResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterPartitionResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterPartitionResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}