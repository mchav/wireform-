{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetFetchResponse
Description : Kafka OffsetFetchResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 9.



Valid versions: 1-10
Flexible versions: 6+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetFetchResponse
  (
    OffsetFetchResponse(..),
    OffsetFetchResponseTopic(..),
    OffsetFetchResponsePartition(..),
    OffsetFetchResponseGroup(..),
    OffsetFetchResponseTopics(..),
    OffsetFetchResponsePartitions(..),
    maxOffsetFetchResponseVersion
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


-- | The responses per partition.
data OffsetFetchResponsePartition = OffsetFetchResponsePartition
  {

  -- | The partition index.

  -- Versions: 0-7
  offsetFetchResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The committed message offset.

  -- Versions: 0-7
  offsetFetchResponsePartitionCommittedOffset :: !(Int64)
,

  -- | The leader epoch.

  -- Versions: 5-7
  offsetFetchResponsePartitionCommittedLeaderEpoch :: !(Int32)
,

  -- | The partition metadata.

  -- Versions: 0-7
  offsetFetchResponsePartitionMetadata :: !(KafkaString)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0-7
  offsetFetchResponsePartitionErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | The responses per topic.
data OffsetFetchResponseTopic = OffsetFetchResponseTopic
  {

  -- | The topic name.

  -- Versions: 0-7
  offsetFetchResponseTopicName :: !(KafkaString)
,

  -- | The responses per partition.

  -- Versions: 0-7
  offsetFetchResponseTopicPartitions :: !(KafkaArray (OffsetFetchResponsePartition))

  }
  deriving (Eq, Show, Generic)

-- | The responses per partition.
data OffsetFetchResponsePartitions = OffsetFetchResponsePartitions
  {

  -- | The partition index.

  -- Versions: 8+
  offsetFetchResponsePartitionsPartitionIndex :: !(Int32)
,

  -- | The committed message offset.

  -- Versions: 8+
  offsetFetchResponsePartitionsCommittedOffset :: !(Int64)
,

  -- | The leader epoch.

  -- Versions: 8+
  offsetFetchResponsePartitionsCommittedLeaderEpoch :: !(Int32)
,

  -- | The partition metadata.

  -- Versions: 8+
  offsetFetchResponsePartitionsMetadata :: !(KafkaString)
,

  -- | The partition-level error code, or 0 if there was no error.

  -- Versions: 8+
  offsetFetchResponsePartitionsErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | The responses per topic.
data OffsetFetchResponseTopics = OffsetFetchResponseTopics
  {

  -- | The topic name.

  -- Versions: 8-9
  offsetFetchResponseTopicsName :: !(KafkaString)
,

  -- | The topic ID.

  -- Versions: 10+
  offsetFetchResponseTopicsTopicId :: !(KafkaUuid)
,

  -- | The responses per partition.

  -- Versions: 8+
  offsetFetchResponseTopicsPartitions :: !(KafkaArray (OffsetFetchResponsePartitions))

  }
  deriving (Eq, Show, Generic)

-- | The responses per group id.
data OffsetFetchResponseGroup = OffsetFetchResponseGroup
  {

  -- | The group ID.

  -- Versions: 8+
  offsetFetchResponseGroupGroupId :: !(KafkaString)
,

  -- | The responses per topic.

  -- Versions: 8+
  offsetFetchResponseGroupTopics :: !(KafkaArray (OffsetFetchResponseTopics))
,

  -- | The group-level error code, or 0 if there was no error.

  -- Versions: 8+
  offsetFetchResponseGroupErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


data OffsetFetchResponse = OffsetFetchResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 3+
  offsetFetchResponseThrottleTimeMs :: !(Int32)
,

  -- | The responses per topic.

  -- Versions: 0-7
  offsetFetchResponseTopics :: !(KafkaArray (OffsetFetchResponseTopic))
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 2-7
  offsetFetchResponseErrorCode :: !(Int16)
,

  -- | The responses per group id.

  -- Versions: 8+
  offsetFetchResponseGroups :: !(KafkaArray (OffsetFetchResponseGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetFetchResponse.
maxOffsetFetchResponseVersion :: Int16
maxOffsetFetchResponseVersion = 10

-- | KafkaMessage instance for OffsetFetchResponse.
instance KafkaMessage OffsetFetchResponse where
  messageApiKey = 9
  messageMinVersion = 1
  messageMaxVersion = 10
  messageFlexibleVersion = Just 6

-- | Worst-case wire size of a OffsetFetchResponsePartition.
wireMaxSizeOffsetFetchResponsePartition :: Int -> OffsetFetchResponsePartition -> Int
wireMaxSizeOffsetFetchResponsePartition _version msg =
  0
  + 4
  + 8
  + 4
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchResponsePartitionMetadata msg))
  + 2
  + 1

-- | Direct-poke encoder for OffsetFetchResponsePartition.
wirePokeOffsetFetchResponsePartition :: Int -> Ptr Word8 -> OffsetFetchResponsePartition -> IO (Ptr Word8)
wirePokeOffsetFetchResponsePartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version <= 7 then W.pokeInt32BE p0 (offsetFetchResponsePartitionPartitionIndex msg) else pure p0)
  p2 <- (if version <= 7 then W.pokeInt64BE p1 (offsetFetchResponsePartitionCommittedOffset msg) else pure p1)
  p3 <- (if version >= 5 && version <= 7 then W.pokeInt32BE p2 (offsetFetchResponsePartitionCommittedLeaderEpoch msg) else pure p2)
  p4 <- (if version <= 7 then (if version >= 6 then WP.pokeCompactString p3 (P.toCompactString (offsetFetchResponsePartitionMetadata msg)) else WP.pokeKafkaString p3 (offsetFetchResponsePartitionMetadata msg)) else pure p3)
  p5 <- (if version <= 7 then W.pokeInt16BE p4 (offsetFetchResponsePartitionErrorCode msg) else pure p4)
  if version >= 6 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for OffsetFetchResponsePartition.
wirePeekOffsetFetchResponsePartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchResponsePartition, Ptr Word8)
wirePeekOffsetFetchResponsePartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- (if version <= 7 then W.peekInt32BE p0 endPtr else pure (0, p0))
  (f1_committedoffset, p2) <- (if version <= 7 then W.peekInt64BE p1 endPtr else pure (0, p1))
  (f2_committedleaderepoch, p3) <- (if version >= 5 && version <= 7 then W.peekInt32BE p2 endPtr else pure (0, p2))
  (f3_metadata, p4) <- (if version <= 7 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
  (f4_errorcode, p5) <- (if version <= 7 then W.peekInt16BE p4 endPtr else pure (0, p4))
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (OffsetFetchResponsePartition { offsetFetchResponsePartitionPartitionIndex = f0_partitionindex, offsetFetchResponsePartitionCommittedOffset = f1_committedoffset, offsetFetchResponsePartitionCommittedLeaderEpoch = f2_committedleaderepoch, offsetFetchResponsePartitionMetadata = f3_metadata, offsetFetchResponsePartitionErrorCode = f4_errorcode }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultOffsetFetchResponsePartition :: OffsetFetchResponsePartition
defaultOffsetFetchResponsePartition = OffsetFetchResponsePartition { offsetFetchResponsePartitionPartitionIndex = 0, offsetFetchResponsePartitionCommittedOffset = 0, offsetFetchResponsePartitionCommittedLeaderEpoch = 0, offsetFetchResponsePartitionMetadata = P.KafkaString Null, offsetFetchResponsePartitionErrorCode = 0 }

-- | Worst-case wire size of a OffsetFetchResponseTopic.
wireMaxSizeOffsetFetchResponseTopic :: Int -> OffsetFetchResponseTopic -> Int
wireMaxSizeOffsetFetchResponseTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchResponseTopicName msg))
  + (5 + (case P.unKafkaArray (offsetFetchResponseTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchResponsePartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetFetchResponseTopic.
wirePokeOffsetFetchResponseTopic :: Int -> Ptr Word8 -> OffsetFetchResponseTopic -> IO (Ptr Word8)
wirePokeOffsetFetchResponseTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version <= 7 then (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (offsetFetchResponseTopicName msg)) else WP.pokeKafkaString p0 (offsetFetchResponseTopicName msg)) else pure p0)
  p2 <- (if version <= 7 then WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponsePartition version p x) p1 (offsetFetchResponseTopicPartitions msg) else pure p1)
  if version >= 6 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for OffsetFetchResponseTopic.
wirePeekOffsetFetchResponseTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchResponseTopic, Ptr Word8)
wirePeekOffsetFetchResponseTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version <= 7 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_partitions, p2) <- (if version <= 7 then WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponsePartition version _fp _basePtr p e) p1 endPtr else pure (P.mkKafkaArray V.empty, p1))
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (OffsetFetchResponseTopic { offsetFetchResponseTopicName = f0_name, offsetFetchResponseTopicPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultOffsetFetchResponseTopic :: OffsetFetchResponseTopic
defaultOffsetFetchResponseTopic = OffsetFetchResponseTopic { offsetFetchResponseTopicName = P.KafkaString Null, offsetFetchResponseTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a OffsetFetchResponsePartitions.
wireMaxSizeOffsetFetchResponsePartitions :: Int -> OffsetFetchResponsePartitions -> Int
wireMaxSizeOffsetFetchResponsePartitions _version msg =
  0
  + 4
  + 8
  + 4
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchResponsePartitionsMetadata msg))
  + 2
  + 1

-- | Direct-poke encoder for OffsetFetchResponsePartitions.
wirePokeOffsetFetchResponsePartitions :: Int -> Ptr Word8 -> OffsetFetchResponsePartitions -> IO (Ptr Word8)
wirePokeOffsetFetchResponsePartitions version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 8 then W.pokeInt32BE p0 (offsetFetchResponsePartitionsPartitionIndex msg) else pure p0)
  p2 <- (if version >= 8 then W.pokeInt64BE p1 (offsetFetchResponsePartitionsCommittedOffset msg) else pure p1)
  p3 <- (if version >= 8 then W.pokeInt32BE p2 (offsetFetchResponsePartitionsCommittedLeaderEpoch msg) else pure p2)
  p4 <- (if version >= 8 then (if version >= 6 then WP.pokeCompactString p3 (P.toCompactString (offsetFetchResponsePartitionsMetadata msg)) else WP.pokeKafkaString p3 (offsetFetchResponsePartitionsMetadata msg)) else pure p3)
  p5 <- (if version >= 8 then W.pokeInt16BE p4 (offsetFetchResponsePartitionsErrorCode msg) else pure p4)
  if version >= 6 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for OffsetFetchResponsePartitions.
wirePeekOffsetFetchResponsePartitions :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchResponsePartitions, Ptr Word8)
wirePeekOffsetFetchResponsePartitions version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- (if version >= 8 then W.peekInt32BE p0 endPtr else pure (0, p0))
  (f1_committedoffset, p2) <- (if version >= 8 then W.peekInt64BE p1 endPtr else pure (0, p1))
  (f2_committedleaderepoch, p3) <- (if version >= 8 then W.peekInt32BE p2 endPtr else pure (0, p2))
  (f3_metadata, p4) <- (if version >= 8 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
  (f4_errorcode, p5) <- (if version >= 8 then W.peekInt16BE p4 endPtr else pure (0, p4))
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (OffsetFetchResponsePartitions { offsetFetchResponsePartitionsPartitionIndex = f0_partitionindex, offsetFetchResponsePartitionsCommittedOffset = f1_committedoffset, offsetFetchResponsePartitionsCommittedLeaderEpoch = f2_committedleaderepoch, offsetFetchResponsePartitionsMetadata = f3_metadata, offsetFetchResponsePartitionsErrorCode = f4_errorcode }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultOffsetFetchResponsePartitions :: OffsetFetchResponsePartitions
defaultOffsetFetchResponsePartitions = OffsetFetchResponsePartitions { offsetFetchResponsePartitionsPartitionIndex = 0, offsetFetchResponsePartitionsCommittedOffset = 0, offsetFetchResponsePartitionsCommittedLeaderEpoch = 0, offsetFetchResponsePartitionsMetadata = P.KafkaString Null, offsetFetchResponsePartitionsErrorCode = 0 }

-- | Worst-case wire size of a OffsetFetchResponseTopics.
wireMaxSizeOffsetFetchResponseTopics :: Int -> OffsetFetchResponseTopics -> Int
wireMaxSizeOffsetFetchResponseTopics _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchResponseTopicsName msg))
  + 16
  + (5 + (case P.unKafkaArray (offsetFetchResponseTopicsPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchResponsePartitions _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetFetchResponseTopics.
wirePokeOffsetFetchResponseTopics :: Int -> Ptr Word8 -> OffsetFetchResponseTopics -> IO (Ptr Word8)
wirePokeOffsetFetchResponseTopics version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 8 && version <= 9 then (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (offsetFetchResponseTopicsName msg)) else WP.pokeKafkaString p0 (offsetFetchResponseTopicsName msg)) else pure p0)
  p2 <- (if version >= 10 then WP.pokeKafkaUuid p1 (offsetFetchResponseTopicsTopicId msg) else pure p1)
  p3 <- (if version >= 8 then WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponsePartitions version p x) p2 (offsetFetchResponseTopicsPartitions msg) else pure p2)
  if version >= 6 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for OffsetFetchResponseTopics.
wirePeekOffsetFetchResponseTopics :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchResponseTopics, Ptr Word8)
wirePeekOffsetFetchResponseTopics version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 8 && version <= 9 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_topicid, p2) <- (if version >= 10 then WP.peekKafkaUuid p1 endPtr else pure (P.nullUuid, p1))
  (f2_partitions, p3) <- (if version >= 8 then WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponsePartitions version _fp _basePtr p e) p2 endPtr else pure (P.mkKafkaArray V.empty, p2))
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (OffsetFetchResponseTopics { offsetFetchResponseTopicsName = f0_name, offsetFetchResponseTopicsTopicId = f1_topicid, offsetFetchResponseTopicsPartitions = f2_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultOffsetFetchResponseTopics :: OffsetFetchResponseTopics
defaultOffsetFetchResponseTopics = OffsetFetchResponseTopics { offsetFetchResponseTopicsName = P.KafkaString Null, offsetFetchResponseTopicsTopicId = P.nullUuid, offsetFetchResponseTopicsPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a OffsetFetchResponseGroup.
wireMaxSizeOffsetFetchResponseGroup :: Int -> OffsetFetchResponseGroup -> Int
wireMaxSizeOffsetFetchResponseGroup _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchResponseGroupGroupId msg))
  + (5 + (case P.unKafkaArray (offsetFetchResponseGroupTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchResponseTopics _version x ) v); P.Null -> 0 }))
  + 2
  + 1

-- | Direct-poke encoder for OffsetFetchResponseGroup.
wirePokeOffsetFetchResponseGroup :: Int -> Ptr Word8 -> OffsetFetchResponseGroup -> IO (Ptr Word8)
wirePokeOffsetFetchResponseGroup version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 8 then (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (offsetFetchResponseGroupGroupId msg)) else WP.pokeKafkaString p0 (offsetFetchResponseGroupGroupId msg)) else pure p0)
  p2 <- (if version >= 8 then WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponseTopics version p x) p1 (offsetFetchResponseGroupTopics msg) else pure p1)
  p3 <- (if version >= 8 then W.pokeInt16BE p2 (offsetFetchResponseGroupErrorCode msg) else pure p2)
  if version >= 6 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for OffsetFetchResponseGroup.
wirePeekOffsetFetchResponseGroup :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchResponseGroup, Ptr Word8)
wirePeekOffsetFetchResponseGroup version _fp _basePtr p0 endPtr = do
  (f0_groupid, p1) <- (if version >= 8 then (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_topics, p2) <- (if version >= 8 then WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponseTopics version _fp _basePtr p e) p1 endPtr else pure (P.mkKafkaArray V.empty, p1))
  (f2_errorcode, p3) <- (if version >= 8 then W.peekInt16BE p2 endPtr else pure (0, p2))
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (OffsetFetchResponseGroup { offsetFetchResponseGroupGroupId = f0_groupid, offsetFetchResponseGroupTopics = f1_topics, offsetFetchResponseGroupErrorCode = f2_errorcode }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultOffsetFetchResponseGroup :: OffsetFetchResponseGroup
defaultOffsetFetchResponseGroup = OffsetFetchResponseGroup { offsetFetchResponseGroupGroupId = P.KafkaString Null, offsetFetchResponseGroupTopics = P.mkKafkaArray V.empty, offsetFetchResponseGroupErrorCode = 0 }

-- | Worst-case wire size of a OffsetFetchResponse.
wireMaxSizeOffsetFetchResponse :: Int -> OffsetFetchResponse -> Int
wireMaxSizeOffsetFetchResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (offsetFetchResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchResponseTopic _version x ) v); P.Null -> 0 }))
  + 2
  + (5 + (case P.unKafkaArray (offsetFetchResponseGroups msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchResponseGroup _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetFetchResponse.
wirePokeOffsetFetchResponse :: Int -> Ptr Word8 -> OffsetFetchResponse -> IO (Ptr Word8)
wirePokeOffsetFetchResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- (if version <= 7 then WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponseTopic version p x) p0 (offsetFetchResponseTopics msg) else pure p0)
    pure p1
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- (if version <= 7 then WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponseTopic version p x) p0 (offsetFetchResponseTopics msg) else pure p0)
    p2 <- (if version >= 2 && version <= 7 then W.pokeInt16BE p1 (offsetFetchResponseErrorCode msg) else pure p1)
    pure p2
  | version >= 6 && version <= 7 = do
    p0 <- pure basePtr
    p1 <- (if version >= 3 then W.pokeInt32BE p0 (offsetFetchResponseThrottleTimeMs msg) else pure p0)
    p2 <- (if version <= 7 then WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponseTopic version p x) p1 (offsetFetchResponseTopics msg) else pure p1)
    p3 <- (if version >= 2 && version <= 7 then W.pokeInt16BE p2 (offsetFetchResponseErrorCode msg) else pure p2)
    WP.pokeEmptyTaggedFields p3
  | version >= 3 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- (if version >= 3 then W.pokeInt32BE p0 (offsetFetchResponseThrottleTimeMs msg) else pure p0)
    p2 <- (if version <= 7 then WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponseTopic version p x) p1 (offsetFetchResponseTopics msg) else pure p1)
    p3 <- (if version >= 2 && version <= 7 then W.pokeInt16BE p2 (offsetFetchResponseErrorCode msg) else pure p2)
    pure p3
  | version >= 8 && version <= 10 = do
    p0 <- pure basePtr
    p1 <- (if version >= 3 then W.pokeInt32BE p0 (offsetFetchResponseThrottleTimeMs msg) else pure p0)
    p2 <- (if version >= 8 then WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponseGroup version p x) p1 (offsetFetchResponseGroups msg) else pure p1)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke OffsetFetchResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for OffsetFetchResponse.
wirePeekOffsetFetchResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchResponse, Ptr Word8)
wirePeekOffsetFetchResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_topics, p1) <- (if version <= 7 then WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponseTopic version _fp _basePtr p e) p0 endPtr else pure (P.mkKafkaArray V.empty, p0))
    pure (OffsetFetchResponse { offsetFetchResponseThrottleTimeMs = 0, offsetFetchResponseTopics = f0_topics, offsetFetchResponseErrorCode = 0, offsetFetchResponseGroups = P.mkKafkaArray V.empty }, p1)
  | version == 2 = do
    (f0_topics, p1) <- (if version <= 7 then WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponseTopic version _fp _basePtr p e) p0 endPtr else pure (P.mkKafkaArray V.empty, p0))
    (f1_errorcode, p2) <- (if version >= 2 && version <= 7 then W.peekInt16BE p1 endPtr else pure (0, p1))
    pure (OffsetFetchResponse { offsetFetchResponseThrottleTimeMs = 0, offsetFetchResponseTopics = f0_topics, offsetFetchResponseErrorCode = f1_errorcode, offsetFetchResponseGroups = P.mkKafkaArray V.empty }, p2)
  | version >= 6 && version <= 7 = do
    (f0_throttletimems, p1) <- (if version >= 3 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_topics, p2) <- (if version <= 7 then WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponseTopic version _fp _basePtr p e) p1 endPtr else pure (P.mkKafkaArray V.empty, p1))
    (f2_errorcode, p3) <- (if version >= 2 && version <= 7 then W.peekInt16BE p2 endPtr else pure (0, p2))
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (OffsetFetchResponse { offsetFetchResponseThrottleTimeMs = f0_throttletimems, offsetFetchResponseTopics = f1_topics, offsetFetchResponseErrorCode = f2_errorcode, offsetFetchResponseGroups = P.mkKafkaArray V.empty }, pTagsEnd)
  | version >= 3 && version <= 5 = do
    (f0_throttletimems, p1) <- (if version >= 3 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_topics, p2) <- (if version <= 7 then WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponseTopic version _fp _basePtr p e) p1 endPtr else pure (P.mkKafkaArray V.empty, p1))
    (f2_errorcode, p3) <- (if version >= 2 && version <= 7 then W.peekInt16BE p2 endPtr else pure (0, p2))
    pure (OffsetFetchResponse { offsetFetchResponseThrottleTimeMs = f0_throttletimems, offsetFetchResponseTopics = f1_topics, offsetFetchResponseErrorCode = f2_errorcode, offsetFetchResponseGroups = P.mkKafkaArray V.empty }, p3)
  | version >= 8 && version <= 10 = do
    (f0_throttletimems, p1) <- (if version >= 3 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_groups, p2) <- (if version >= 8 then WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponseGroup version _fp _basePtr p e) p1 endPtr else pure (P.mkKafkaArray V.empty, p1))
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (OffsetFetchResponse { offsetFetchResponseThrottleTimeMs = f0_throttletimems, offsetFetchResponseTopics = P.mkKafkaArray V.empty, offsetFetchResponseErrorCode = 0, offsetFetchResponseGroups = f1_groups }, pTagsEnd)
  | otherwise = error $ "wirePeek OffsetFetchResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec OffsetFetchResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeOffsetFetchResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeOffsetFetchResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekOffsetFetchResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}