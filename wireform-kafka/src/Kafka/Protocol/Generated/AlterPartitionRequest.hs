{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterPartitionRequest
Description : Kafka AlterPartitionRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 56.



Valid versions: 2-3
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterPartitionRequest
  (
    AlterPartitionRequest(..),
    TopicData(..),
    PartitionData(..),
    BrokerState(..),
    maxAlterPartitionRequestVersion
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


-- | The ISR for this partition.
data BrokerState = BrokerState
  {

  -- | The ID of the broker.

  -- Versions: 3+
  brokerStateBrokerId :: !(Int32)
,

  -- | The epoch of the broker. It will be -1 if the epoch check is not supported.

  -- Versions: 3+
  brokerStateBrokerEpoch :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | The partitions to alter ISRs for.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The leader epoch of this partition.

  -- Versions: 0+
  partitionDataLeaderEpoch :: !(Int32)
,

  -- | The ISR for this partition. Deprecated since version 3.

  -- Versions: 0-2
  partitionDataNewIsr :: !(KafkaArray (Int32))
,

  -- | The ISR for this partition.

  -- Versions: 3+
  partitionDataNewIsrWithEpochs :: !(KafkaArray (BrokerState))
,

  -- | 1 if the partition is recovering from an unclean leader election; 0 otherwise.

  -- Versions: 1+
  partitionDataLeaderRecoveryState :: !(Int8)
,

  -- | The expected epoch of the partition which is being updated.

  -- Versions: 0+
  partitionDataPartitionEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | The topics to alter ISRs for.
data TopicData = TopicData
  {

  -- | The ID of the topic to alter ISRs for.

  -- Versions: 2+
  topicDataTopicId :: !(KafkaUuid)
,

  -- | The partitions to alter ISRs for.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


data AlterPartitionRequest = AlterPartitionRequest
  {

  -- | The ID of the requesting broker.

  -- Versions: 0+
  alterPartitionRequestBrokerId :: !(Int32)
,

  -- | The epoch of the requesting broker.

  -- Versions: 0+
  alterPartitionRequestBrokerEpoch :: !(Int64)
,

  -- | The topics to alter ISRs for.

  -- Versions: 0+
  alterPartitionRequestTopics :: !(KafkaArray (TopicData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterPartitionRequest.
maxAlterPartitionRequestVersion :: Int16
maxAlterPartitionRequestVersion = 3

-- | KafkaMessage instance for AlterPartitionRequest.
instance KafkaMessage AlterPartitionRequest where
  messageApiKey = 56
  messageMinVersion = 2
  messageMaxVersion = 3
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a BrokerState.
wireMaxSizeBrokerState :: Int -> BrokerState -> Int
wireMaxSizeBrokerState _version msg =
  0
  + 4
  + 8
  + 1

-- | Direct-poke encoder for BrokerState.
wirePokeBrokerState :: Int -> Ptr Word8 -> BrokerState -> IO (Ptr Word8)
wirePokeBrokerState version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (brokerStateBrokerId msg)
  p2 <- W.pokeInt64BE p1 (brokerStateBrokerEpoch msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for BrokerState.
wirePeekBrokerState :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (BrokerState, Ptr Word8)
wirePeekBrokerState version _fp _basePtr p0 endPtr = do
  (f0_brokerid, p1) <- W.peekInt32BE p0 endPtr
  (f1_brokerepoch, p2) <- W.peekInt64BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (BrokerState { brokerStateBrokerId = f0_brokerid, brokerStateBrokerEpoch = f1_brokerepoch }, pTagsEnd)

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 4
  + (5 + (case P.unKafkaArray (partitionDataNewIsr msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (partitionDataNewIsrWithEpochs msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeBrokerState _version x ) v); P.Null -> 0 }))
  + 1
  + 4
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt32BE p1 (partitionDataLeaderEpoch msg)
  p3 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p2 (partitionDataNewIsr msg)
  p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeBrokerState version p x) p3 (partitionDataNewIsrWithEpochs msg)
  p5 <- W.pokeWord8 p4 (fromIntegral (partitionDataLeaderRecoveryState msg))
  p6 <- W.pokeInt32BE p5 (partitionDataPartitionEpoch msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p6 else pure p6

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_leaderepoch, p2) <- W.peekInt32BE p1 endPtr
  (f2_newisr, p3) <- WP.peekVersionedArray version 0 W.peekInt32BE p2 endPtr
  (f3_newisrwithepochs, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekBrokerState version _fp _basePtr p e) p3 endPtr
  (f4_leaderrecoverystate, p5) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p4 endPtr
  (f5_partitionepoch, p6) <- W.peekInt32BE p5 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p6 endPtr else pure p6
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataLeaderEpoch = f1_leaderepoch, partitionDataNewIsr = f2_newisr, partitionDataNewIsrWithEpochs = f3_newisrwithepochs, partitionDataLeaderRecoveryState = f4_leaderrecoverystate, partitionDataPartitionEpoch = f5_partitionepoch }, pTagsEnd)

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

-- | Worst-case wire size of a AlterPartitionRequest.
wireMaxSizeAlterPartitionRequest :: Int -> AlterPartitionRequest -> Int
wireMaxSizeAlterPartitionRequest _version msg =
  0
  + 4
  + 8
  + (5 + (case P.unKafkaArray (alterPartitionRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterPartitionRequest.
wirePokeAlterPartitionRequest :: Int -> Ptr Word8 -> AlterPartitionRequest -> IO (Ptr Word8)
wirePokeAlterPartitionRequest version basePtr msg
  | version >= 2 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (alterPartitionRequestBrokerId msg)
    p2 <- W.pokeInt64BE p1 (alterPartitionRequestBrokerEpoch msg)
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicData version p x) p2 (alterPartitionRequestTopics msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke AlterPartitionRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterPartitionRequest.
wirePeekAlterPartitionRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterPartitionRequest, Ptr Word8)
wirePeekAlterPartitionRequest version _fp _basePtr p0 endPtr
  | version >= 2 && version <= 3 = do
    (f0_brokerid, p1) <- W.peekInt32BE p0 endPtr
    (f1_brokerepoch, p2) <- W.peekInt64BE p1 endPtr
    (f2_topics, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicData version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (AlterPartitionRequest { alterPartitionRequestBrokerId = f0_brokerid, alterPartitionRequestBrokerEpoch = f1_brokerepoch, alterPartitionRequestTopics = f2_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek AlterPartitionRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AlterPartitionRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterPartitionRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterPartitionRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterPartitionRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}