{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeShareGroupOffsetsResponse
Description : Kafka DescribeShareGroupOffsetsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 90.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeShareGroupOffsetsResponse
  (
    DescribeShareGroupOffsetsResponse(..),
    DescribeShareGroupOffsetsResponseGroup(..),
    DescribeShareGroupOffsetsResponseTopic(..),
    DescribeShareGroupOffsetsResponsePartition(..),
    maxDescribeShareGroupOffsetsResponseVersion
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



data DescribeShareGroupOffsetsResponsePartition = DescribeShareGroupOffsetsResponsePartition
  {

  -- | The partition index.

  -- Versions: 0+
  describeShareGroupOffsetsResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The share-partition start offset.

  -- Versions: 0+
  describeShareGroupOffsetsResponsePartitionStartOffset :: !(Int64)
,

  -- | The leader epoch of the partition.

  -- Versions: 0+
  describeShareGroupOffsetsResponsePartitionLeaderEpoch :: !(Int32)
,

  -- | The share-partition lag.

  -- Versions: 1+
  describeShareGroupOffsetsResponsePartitionLag :: !(Int64)
,

  -- | The partition-level error code, or 0 if there was no error.

  -- Versions: 0+
  describeShareGroupOffsetsResponsePartitionErrorCode :: !(Int16)
,

  -- | The partition-level error message, or null if there was no error.

  -- Versions: 0+
  describeShareGroupOffsetsResponsePartitionErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | The results for each topic.
data DescribeShareGroupOffsetsResponseTopic = DescribeShareGroupOffsetsResponseTopic
  {

  -- | The topic name.

  -- Versions: 0+
  describeShareGroupOffsetsResponseTopicTopicName :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 0+
  describeShareGroupOffsetsResponseTopicTopicId :: !(KafkaUuid)
,


  -- Versions: 0+
  describeShareGroupOffsetsResponseTopicPartitions :: !(KafkaArray (DescribeShareGroupOffsetsResponsePartition))

  }
  deriving (Eq, Show, Generic)

-- | The results for each group.
data DescribeShareGroupOffsetsResponseGroup = DescribeShareGroupOffsetsResponseGroup
  {

  -- | The group identifier.

  -- Versions: 0+
  describeShareGroupOffsetsResponseGroupGroupId :: !(KafkaString)
,

  -- | The results for each topic.

  -- Versions: 0+
  describeShareGroupOffsetsResponseGroupTopics :: !(KafkaArray (DescribeShareGroupOffsetsResponseTopic))
,

  -- | The group-level error code, or 0 if there was no error.

  -- Versions: 0+
  describeShareGroupOffsetsResponseGroupErrorCode :: !(Int16)
,

  -- | The group-level error message, or null if there was no error.

  -- Versions: 0+
  describeShareGroupOffsetsResponseGroupErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


data DescribeShareGroupOffsetsResponse = DescribeShareGroupOffsetsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeShareGroupOffsetsResponseThrottleTimeMs :: !(Int32)
,

  -- | The results for each group.

  -- Versions: 0+
  describeShareGroupOffsetsResponseGroups :: !(KafkaArray (DescribeShareGroupOffsetsResponseGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeShareGroupOffsetsResponse.
maxDescribeShareGroupOffsetsResponseVersion :: Int16
maxDescribeShareGroupOffsetsResponseVersion = 1

-- | KafkaMessage instance for DescribeShareGroupOffsetsResponse.
instance KafkaMessage DescribeShareGroupOffsetsResponse where
  messageApiKey = 90
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a DescribeShareGroupOffsetsResponsePartition.
wireMaxSizeDescribeShareGroupOffsetsResponsePartition :: Int -> DescribeShareGroupOffsetsResponsePartition -> Int
wireMaxSizeDescribeShareGroupOffsetsResponsePartition _version msg =
  0
  + 4
  + 8
  + 4
  + 8
  + 2
  + WP.dualStringMaxSize (describeShareGroupOffsetsResponsePartitionErrorMessage msg)
  + 1

-- | Direct-poke encoder for DescribeShareGroupOffsetsResponsePartition.
wirePokeDescribeShareGroupOffsetsResponsePartition :: Int -> Ptr Word8 -> DescribeShareGroupOffsetsResponsePartition -> IO (Ptr Word8)
wirePokeDescribeShareGroupOffsetsResponsePartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (describeShareGroupOffsetsResponsePartitionPartitionIndex msg)
  p2 <- W.pokeInt64BE p1 (describeShareGroupOffsetsResponsePartitionStartOffset msg)
  p3 <- W.pokeInt32BE p2 (describeShareGroupOffsetsResponsePartitionLeaderEpoch msg)
  p4 <- (if version >= 1 then W.pokeInt64BE p3 (describeShareGroupOffsetsResponsePartitionLag msg) else pure p3)
  p5 <- W.pokeInt16BE p4 (describeShareGroupOffsetsResponsePartitionErrorCode msg)
  p6 <- (if version >= 0 then WP.pokeCompactString p5 (P.toCompactString (describeShareGroupOffsetsResponsePartitionErrorMessage msg)) else WP.pokeKafkaString p5 (describeShareGroupOffsetsResponsePartitionErrorMessage msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p6 else pure p6

-- | Direct-poke decoder for DescribeShareGroupOffsetsResponsePartition.
wirePeekDescribeShareGroupOffsetsResponsePartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeShareGroupOffsetsResponsePartition, Ptr Word8)
wirePeekDescribeShareGroupOffsetsResponsePartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_startoffset, p2) <- W.peekInt64BE p1 endPtr
  (f2_leaderepoch, p3) <- W.peekInt32BE p2 endPtr
  (f3_lag, p4) <- (if version >= 1 then W.peekInt64BE p3 endPtr else pure (0, p3))
  (f4_errorcode, p5) <- W.peekInt16BE p4 endPtr
  (f5_errormessage, p6) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr else WP.peekKafkaString p5 endPtr)
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p6 endPtr else pure p6
  pure (DescribeShareGroupOffsetsResponsePartition { describeShareGroupOffsetsResponsePartitionPartitionIndex = f0_partitionindex, describeShareGroupOffsetsResponsePartitionStartOffset = f1_startoffset, describeShareGroupOffsetsResponsePartitionLeaderEpoch = f2_leaderepoch, describeShareGroupOffsetsResponsePartitionLag = f3_lag, describeShareGroupOffsetsResponsePartitionErrorCode = f4_errorcode, describeShareGroupOffsetsResponsePartitionErrorMessage = f5_errormessage }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribeShareGroupOffsetsResponsePartition :: DescribeShareGroupOffsetsResponsePartition
defaultDescribeShareGroupOffsetsResponsePartition = DescribeShareGroupOffsetsResponsePartition { describeShareGroupOffsetsResponsePartitionPartitionIndex = 0, describeShareGroupOffsetsResponsePartitionStartOffset = 0, describeShareGroupOffsetsResponsePartitionLeaderEpoch = 0, describeShareGroupOffsetsResponsePartitionLag = 0, describeShareGroupOffsetsResponsePartitionErrorCode = 0, describeShareGroupOffsetsResponsePartitionErrorMessage = P.KafkaString Null }

-- | Worst-case wire size of a DescribeShareGroupOffsetsResponseTopic.
wireMaxSizeDescribeShareGroupOffsetsResponseTopic :: Int -> DescribeShareGroupOffsetsResponseTopic -> Int
wireMaxSizeDescribeShareGroupOffsetsResponseTopic _version msg =
  0
  + WP.dualStringMaxSize (describeShareGroupOffsetsResponseTopicTopicName msg)
  + 16
  + (5 + (case P.unKafkaArray (describeShareGroupOffsetsResponseTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeShareGroupOffsetsResponsePartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeShareGroupOffsetsResponseTopic.
wirePokeDescribeShareGroupOffsetsResponseTopic :: Int -> Ptr Word8 -> DescribeShareGroupOffsetsResponseTopic -> IO (Ptr Word8)
wirePokeDescribeShareGroupOffsetsResponseTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (describeShareGroupOffsetsResponseTopicTopicName msg)) else WP.pokeKafkaString p0 (describeShareGroupOffsetsResponseTopicTopicName msg))
  p2 <- WP.pokeKafkaUuid p1 (describeShareGroupOffsetsResponseTopicTopicId msg)
  p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDescribeShareGroupOffsetsResponsePartition version p x) p2 (describeShareGroupOffsetsResponseTopicPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for DescribeShareGroupOffsetsResponseTopic.
wirePeekDescribeShareGroupOffsetsResponseTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeShareGroupOffsetsResponseTopic, Ptr Word8)
wirePeekDescribeShareGroupOffsetsResponseTopic version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_topicid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_partitions, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDescribeShareGroupOffsetsResponsePartition version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (DescribeShareGroupOffsetsResponseTopic { describeShareGroupOffsetsResponseTopicTopicName = f0_topicname, describeShareGroupOffsetsResponseTopicTopicId = f1_topicid, describeShareGroupOffsetsResponseTopicPartitions = f2_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribeShareGroupOffsetsResponseTopic :: DescribeShareGroupOffsetsResponseTopic
defaultDescribeShareGroupOffsetsResponseTopic = DescribeShareGroupOffsetsResponseTopic { describeShareGroupOffsetsResponseTopicTopicName = P.KafkaString Null, describeShareGroupOffsetsResponseTopicTopicId = P.nullUuid, describeShareGroupOffsetsResponseTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a DescribeShareGroupOffsetsResponseGroup.
wireMaxSizeDescribeShareGroupOffsetsResponseGroup :: Int -> DescribeShareGroupOffsetsResponseGroup -> Int
wireMaxSizeDescribeShareGroupOffsetsResponseGroup _version msg =
  0
  + WP.dualStringMaxSize (describeShareGroupOffsetsResponseGroupGroupId msg)
  + (5 + (case P.unKafkaArray (describeShareGroupOffsetsResponseGroupTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeShareGroupOffsetsResponseTopic _version x ) v); P.Null -> 0 }))
  + 2
  + WP.dualStringMaxSize (describeShareGroupOffsetsResponseGroupErrorMessage msg)
  + 1

-- | Direct-poke encoder for DescribeShareGroupOffsetsResponseGroup.
wirePokeDescribeShareGroupOffsetsResponseGroup :: Int -> Ptr Word8 -> DescribeShareGroupOffsetsResponseGroup -> IO (Ptr Word8)
wirePokeDescribeShareGroupOffsetsResponseGroup version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (describeShareGroupOffsetsResponseGroupGroupId msg)) else WP.pokeKafkaString p0 (describeShareGroupOffsetsResponseGroupGroupId msg))
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDescribeShareGroupOffsetsResponseTopic version p x) p1 (describeShareGroupOffsetsResponseGroupTopics msg)
  p3 <- W.pokeInt16BE p2 (describeShareGroupOffsetsResponseGroupErrorCode msg)
  p4 <- (if version >= 0 then WP.pokeCompactString p3 (P.toCompactString (describeShareGroupOffsetsResponseGroupErrorMessage msg)) else WP.pokeKafkaString p3 (describeShareGroupOffsetsResponseGroupErrorMessage msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for DescribeShareGroupOffsetsResponseGroup.
wirePeekDescribeShareGroupOffsetsResponseGroup :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeShareGroupOffsetsResponseGroup, Ptr Word8)
wirePeekDescribeShareGroupOffsetsResponseGroup version _fp _basePtr p0 endPtr = do
  (f0_groupid, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDescribeShareGroupOffsetsResponseTopic version _fp _basePtr p e) p1 endPtr
  (f2_errorcode, p3) <- W.peekInt16BE p2 endPtr
  (f3_errormessage, p4) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (DescribeShareGroupOffsetsResponseGroup { describeShareGroupOffsetsResponseGroupGroupId = f0_groupid, describeShareGroupOffsetsResponseGroupTopics = f1_topics, describeShareGroupOffsetsResponseGroupErrorCode = f2_errorcode, describeShareGroupOffsetsResponseGroupErrorMessage = f3_errormessage }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribeShareGroupOffsetsResponseGroup :: DescribeShareGroupOffsetsResponseGroup
defaultDescribeShareGroupOffsetsResponseGroup = DescribeShareGroupOffsetsResponseGroup { describeShareGroupOffsetsResponseGroupGroupId = P.KafkaString Null, describeShareGroupOffsetsResponseGroupTopics = P.mkKafkaArray V.empty, describeShareGroupOffsetsResponseGroupErrorCode = 0, describeShareGroupOffsetsResponseGroupErrorMessage = P.KafkaString Null }

-- | Worst-case wire size of a DescribeShareGroupOffsetsResponse.
wireMaxSizeDescribeShareGroupOffsetsResponse :: Int -> DescribeShareGroupOffsetsResponse -> Int
wireMaxSizeDescribeShareGroupOffsetsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (describeShareGroupOffsetsResponseGroups msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeShareGroupOffsetsResponseGroup _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeShareGroupOffsetsResponse.
wirePokeDescribeShareGroupOffsetsResponse :: Int -> Ptr Word8 -> DescribeShareGroupOffsetsResponse -> IO (Ptr Word8)
wirePokeDescribeShareGroupOffsetsResponse version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeShareGroupOffsetsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDescribeShareGroupOffsetsResponseGroup version p x) p1 (describeShareGroupOffsetsResponseGroups msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke DescribeShareGroupOffsetsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeShareGroupOffsetsResponse.
wirePeekDescribeShareGroupOffsetsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeShareGroupOffsetsResponse, Ptr Word8)
wirePeekDescribeShareGroupOffsetsResponse version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_groups, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDescribeShareGroupOffsetsResponseGroup version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DescribeShareGroupOffsetsResponse { describeShareGroupOffsetsResponseThrottleTimeMs = f0_throttletimems, describeShareGroupOffsetsResponseGroups = f1_groups }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeShareGroupOffsetsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeShareGroupOffsetsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeShareGroupOffsetsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeShareGroupOffsetsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeShareGroupOffsetsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}