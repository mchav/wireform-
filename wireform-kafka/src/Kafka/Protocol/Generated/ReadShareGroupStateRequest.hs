{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ReadShareGroupStateRequest
Description : Kafka ReadShareGroupStateRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 84.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ReadShareGroupStateRequest
  (
    ReadShareGroupStateRequest(..),
    ReadStateData(..),
    PartitionData(..),
    maxReadShareGroupStateRequestVersion
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


-- | The data for the partitions.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartition :: !(Int32)
,

  -- | The leader epoch of the share-partition.

  -- Versions: 0+
  partitionDataLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | The data for the topics.
data ReadStateData = ReadStateData
  {

  -- | The topic identifier.

  -- Versions: 0+
  readStateDataTopicId :: !(KafkaUuid)
,

  -- | The data for the partitions.

  -- Versions: 0+
  readStateDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


data ReadShareGroupStateRequest = ReadShareGroupStateRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  readShareGroupStateRequestGroupId :: !(KafkaString)
,

  -- | The data for the topics.

  -- Versions: 0+
  readShareGroupStateRequestTopics :: !(KafkaArray (ReadStateData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ReadShareGroupStateRequest.
maxReadShareGroupStateRequestVersion :: Int16
maxReadShareGroupStateRequestVersion = 0

-- | KafkaMessage instance for ReadShareGroupStateRequest.
instance KafkaMessage ReadShareGroupStateRequest where
  messageApiKey = 84
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 4
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartition msg)
  p2 <- W.pokeInt32BE p1 (partitionDataLeaderEpoch msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partition, p1) <- W.peekInt32BE p0 endPtr
  (f1_leaderepoch, p2) <- W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (PartitionData { partitionDataPartition = f0_partition, partitionDataLeaderEpoch = f1_leaderepoch }, pTagsEnd)

-- | Worst-case wire size of a ReadStateData.
wireMaxSizeReadStateData :: Int -> ReadStateData -> Int
wireMaxSizeReadStateData _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (readStateDataPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReadStateData.
wirePokeReadStateData :: Int -> Ptr Word8 -> ReadStateData -> IO (Ptr Word8)
wirePokeReadStateData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (readStateDataTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionData version p x) p1 (readStateDataPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReadStateData.
wirePeekReadStateData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReadStateData, Ptr Word8)
wirePeekReadStateData version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReadStateData { readStateDataTopicId = f0_topicid, readStateDataPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a ReadShareGroupStateRequest.
wireMaxSizeReadShareGroupStateRequest :: Int -> ReadShareGroupStateRequest -> Int
wireMaxSizeReadShareGroupStateRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (readShareGroupStateRequestGroupId msg))
  + (5 + (case P.unKafkaArray (readShareGroupStateRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReadStateData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReadShareGroupStateRequest.
wirePokeReadShareGroupStateRequest :: Int -> Ptr Word8 -> ReadShareGroupStateRequest -> IO (Ptr Word8)
wirePokeReadShareGroupStateRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (readShareGroupStateRequestGroupId msg))
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReadStateData version p x) p1 (readShareGroupStateRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke ReadShareGroupStateRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ReadShareGroupStateRequest.
wirePeekReadShareGroupStateRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReadShareGroupStateRequest, Ptr Word8)
wirePeekReadShareGroupStateRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReadStateData version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (ReadShareGroupStateRequest { readShareGroupStateRequestGroupId = f0_groupid, readShareGroupStateRequestTopics = f1_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek ReadShareGroupStateRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ReadShareGroupStateRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeReadShareGroupStateRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeReadShareGroupStateRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekReadShareGroupStateRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}