{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetCommitRequest
Description : Kafka OffsetCommitRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 8.



Valid versions: 2-10
Flexible versions: 8+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetCommitRequest
  (
    OffsetCommitRequest(..),
    OffsetCommitRequestTopic(..),
    OffsetCommitRequestPartition(..),
    maxOffsetCommitRequestVersion
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


-- | Each partition to commit offsets for.
data OffsetCommitRequestPartition = OffsetCommitRequestPartition
  {

  -- | The partition index.

  -- Versions: 0+
  offsetCommitRequestPartitionPartitionIndex :: !(Int32)
,

  -- | The message offset to be committed.

  -- Versions: 0+
  offsetCommitRequestPartitionCommittedOffset :: !(Int64)
,

  -- | The leader epoch of this partition.

  -- Versions: 6+
  offsetCommitRequestPartitionCommittedLeaderEpoch :: !(Int32)
,

  -- | Any associated metadata the client wants to keep.

  -- Versions: 0+
  offsetCommitRequestPartitionCommittedMetadata :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | The topics to commit offsets for.
data OffsetCommitRequestTopic = OffsetCommitRequestTopic
  {

  -- | The topic name.

  -- Versions: 0-9
  offsetCommitRequestTopicName :: !(KafkaString)
,

  -- | The topic ID.

  -- Versions: 10+
  offsetCommitRequestTopicTopicId :: !(KafkaUuid)
,

  -- | Each partition to commit offsets for.

  -- Versions: 0+
  offsetCommitRequestTopicPartitions :: !(KafkaArray (OffsetCommitRequestPartition))

  }
  deriving (Eq, Show, Generic)


data OffsetCommitRequest = OffsetCommitRequest
  {

  -- | The unique group identifier.

  -- Versions: 0+
  offsetCommitRequestGroupId :: !(KafkaString)
,

  -- | The generation of the group if using the classic group protocol or the member epoch if using the con

  -- Versions: 1+
  offsetCommitRequestGenerationIdOrMemberEpoch :: !(Int32)
,

  -- | The member ID assigned by the group coordinator.

  -- Versions: 1+
  offsetCommitRequestMemberId :: !(KafkaString)
,

  -- | The unique identifier of the consumer instance provided by end user.

  -- Versions: 7+
  offsetCommitRequestGroupInstanceId :: !(KafkaString)
,

  -- | The time period in ms to retain the offset.

  -- Versions: 2-4
  offsetCommitRequestRetentionTimeMs :: !(Int64)
,

  -- | The topics to commit offsets for.

  -- Versions: 0+
  offsetCommitRequestTopics :: !(KafkaArray (OffsetCommitRequestTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetCommitRequest.
maxOffsetCommitRequestVersion :: Int16
maxOffsetCommitRequestVersion = 10

-- | KafkaMessage instance for OffsetCommitRequest.
instance KafkaMessage OffsetCommitRequest where
  messageApiKey = 8
  messageMinVersion = 2
  messageMaxVersion = 10
  messageFlexibleVersion = Just 8

-- | Worst-case wire size of a OffsetCommitRequestPartition.
wireMaxSizeOffsetCommitRequestPartition :: Int -> OffsetCommitRequestPartition -> Int
wireMaxSizeOffsetCommitRequestPartition _version msg =
  0
  + 4
  + 8
  + 4
  + WP.dualStringMaxSize (offsetCommitRequestPartitionCommittedMetadata msg)
  + 1

-- | Direct-poke encoder for OffsetCommitRequestPartition.
wirePokeOffsetCommitRequestPartition :: Int -> Ptr Word8 -> OffsetCommitRequestPartition -> IO (Ptr Word8)
wirePokeOffsetCommitRequestPartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (offsetCommitRequestPartitionPartitionIndex msg)
  p2 <- W.pokeInt64BE p1 (offsetCommitRequestPartitionCommittedOffset msg)
  p3 <- (if version >= 6 then W.pokeInt32BE p2 (offsetCommitRequestPartitionCommittedLeaderEpoch msg) else pure p2)
  p4 <- (if version >= 8 then WP.pokeCompactString p3 (P.toCompactString (offsetCommitRequestPartitionCommittedMetadata msg)) else WP.pokeKafkaString p3 (offsetCommitRequestPartitionCommittedMetadata msg))
  if version >= 8 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for OffsetCommitRequestPartition.
wirePeekOffsetCommitRequestPartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetCommitRequestPartition, Ptr Word8)
wirePeekOffsetCommitRequestPartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_committedoffset, p2) <- W.peekInt64BE p1 endPtr
  (f2_committedleaderepoch, p3) <- (if version >= 6 then W.peekInt32BE p2 endPtr else pure (0, p2))
  (f3_committedmetadata, p4) <- (if version >= 8 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  pTagsEnd <- if version >= 8 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (OffsetCommitRequestPartition { offsetCommitRequestPartitionPartitionIndex = f0_partitionindex, offsetCommitRequestPartitionCommittedOffset = f1_committedoffset, offsetCommitRequestPartitionCommittedLeaderEpoch = f2_committedleaderepoch, offsetCommitRequestPartitionCommittedMetadata = f3_committedmetadata }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultOffsetCommitRequestPartition :: OffsetCommitRequestPartition
defaultOffsetCommitRequestPartition = OffsetCommitRequestPartition { offsetCommitRequestPartitionPartitionIndex = 0, offsetCommitRequestPartitionCommittedOffset = 0, offsetCommitRequestPartitionCommittedLeaderEpoch = 0, offsetCommitRequestPartitionCommittedMetadata = P.KafkaString Null }

-- | Worst-case wire size of a OffsetCommitRequestTopic.
wireMaxSizeOffsetCommitRequestTopic :: Int -> OffsetCommitRequestTopic -> Int
wireMaxSizeOffsetCommitRequestTopic _version msg =
  0
  + WP.dualStringMaxSize (offsetCommitRequestTopicName msg)
  + 16
  + (5 + (case P.unKafkaArray (offsetCommitRequestTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetCommitRequestPartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetCommitRequestTopic.
wirePokeOffsetCommitRequestTopic :: Int -> Ptr Word8 -> OffsetCommitRequestTopic -> IO (Ptr Word8)
wirePokeOffsetCommitRequestTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version <= 9 then (if version >= 8 then WP.pokeCompactString p0 (P.toCompactString (offsetCommitRequestTopicName msg)) else WP.pokeKafkaString p0 (offsetCommitRequestTopicName msg)) else pure p0)
  p2 <- (if version >= 10 then WP.pokeKafkaUuid p1 (offsetCommitRequestTopicTopicId msg) else pure p1)
  p3 <- WP.pokeVersionedArray version 8 (\p x -> wirePokeOffsetCommitRequestPartition version p x) p2 (offsetCommitRequestTopicPartitions msg)
  if version >= 8 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for OffsetCommitRequestTopic.
wirePeekOffsetCommitRequestTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetCommitRequestTopic, Ptr Word8)
wirePeekOffsetCommitRequestTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version <= 9 then (if version >= 8 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_topicid, p2) <- (if version >= 10 then WP.peekKafkaUuid p1 endPtr else pure (P.nullUuid, p1))
  (f2_partitions, p3) <- WP.peekVersionedArray version 8 (\p e -> wirePeekOffsetCommitRequestPartition version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 8 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (OffsetCommitRequestTopic { offsetCommitRequestTopicName = f0_name, offsetCommitRequestTopicTopicId = f1_topicid, offsetCommitRequestTopicPartitions = f2_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultOffsetCommitRequestTopic :: OffsetCommitRequestTopic
defaultOffsetCommitRequestTopic = OffsetCommitRequestTopic { offsetCommitRequestTopicName = P.KafkaString Null, offsetCommitRequestTopicTopicId = P.nullUuid, offsetCommitRequestTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a OffsetCommitRequest.
wireMaxSizeOffsetCommitRequest :: Int -> OffsetCommitRequest -> Int
wireMaxSizeOffsetCommitRequest _version msg =
  0
  + WP.dualStringMaxSize (offsetCommitRequestGroupId msg)
  + 4
  + WP.dualStringMaxSize (offsetCommitRequestMemberId msg)
  + WP.dualStringMaxSize (offsetCommitRequestGroupInstanceId msg)
  + 8
  + (5 + (case P.unKafkaArray (offsetCommitRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetCommitRequestTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetCommitRequest.
wirePokeOffsetCommitRequest :: Int -> Ptr Word8 -> OffsetCommitRequest -> IO (Ptr Word8)
wirePokeOffsetCommitRequest version basePtr msg
  | version == 7 = do
    p0 <- pure basePtr
    p1 <- (if version >= 8 then WP.pokeCompactString p0 (P.toCompactString (offsetCommitRequestGroupId msg)) else WP.pokeKafkaString p0 (offsetCommitRequestGroupId msg))
    p2 <- (if version >= 1 then W.pokeInt32BE p1 (offsetCommitRequestGenerationIdOrMemberEpoch msg) else pure p1)
    p3 <- (if version >= 1 then (if version >= 8 then WP.pokeCompactString p2 (P.toCompactString (offsetCommitRequestMemberId msg)) else WP.pokeKafkaString p2 (offsetCommitRequestMemberId msg)) else pure p2)
    p4 <- (if version >= 7 then (if version >= 8 then WP.pokeCompactString p3 (P.toCompactString (offsetCommitRequestGroupInstanceId msg)) else WP.pokeKafkaString p3 (offsetCommitRequestGroupInstanceId msg)) else pure p3)
    p5 <- WP.pokeVersionedArray version 8 (\p x -> wirePokeOffsetCommitRequestTopic version p x) p4 (offsetCommitRequestTopics msg)
    pure p5
  | version >= 5 && version <= 6 = do
    p0 <- pure basePtr
    p1 <- (if version >= 8 then WP.pokeCompactString p0 (P.toCompactString (offsetCommitRequestGroupId msg)) else WP.pokeKafkaString p0 (offsetCommitRequestGroupId msg))
    p2 <- (if version >= 1 then W.pokeInt32BE p1 (offsetCommitRequestGenerationIdOrMemberEpoch msg) else pure p1)
    p3 <- (if version >= 1 then (if version >= 8 then WP.pokeCompactString p2 (P.toCompactString (offsetCommitRequestMemberId msg)) else WP.pokeKafkaString p2 (offsetCommitRequestMemberId msg)) else pure p2)
    p4 <- WP.pokeVersionedArray version 8 (\p x -> wirePokeOffsetCommitRequestTopic version p x) p3 (offsetCommitRequestTopics msg)
    pure p4
  | version >= 2 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- (if version >= 8 then WP.pokeCompactString p0 (P.toCompactString (offsetCommitRequestGroupId msg)) else WP.pokeKafkaString p0 (offsetCommitRequestGroupId msg))
    p2 <- (if version >= 1 then W.pokeInt32BE p1 (offsetCommitRequestGenerationIdOrMemberEpoch msg) else pure p1)
    p3 <- (if version >= 1 then (if version >= 8 then WP.pokeCompactString p2 (P.toCompactString (offsetCommitRequestMemberId msg)) else WP.pokeKafkaString p2 (offsetCommitRequestMemberId msg)) else pure p2)
    p4 <- (if version >= 2 && version <= 4 then W.pokeInt64BE p3 (offsetCommitRequestRetentionTimeMs msg) else pure p3)
    p5 <- WP.pokeVersionedArray version 8 (\p x -> wirePokeOffsetCommitRequestTopic version p x) p4 (offsetCommitRequestTopics msg)
    pure p5
  | version >= 8 && version <= 10 = do
    p0 <- pure basePtr
    p1 <- (if version >= 8 then WP.pokeCompactString p0 (P.toCompactString (offsetCommitRequestGroupId msg)) else WP.pokeKafkaString p0 (offsetCommitRequestGroupId msg))
    p2 <- (if version >= 1 then W.pokeInt32BE p1 (offsetCommitRequestGenerationIdOrMemberEpoch msg) else pure p1)
    p3 <- (if version >= 1 then (if version >= 8 then WP.pokeCompactString p2 (P.toCompactString (offsetCommitRequestMemberId msg)) else WP.pokeKafkaString p2 (offsetCommitRequestMemberId msg)) else pure p2)
    p4 <- (if version >= 7 then (if version >= 8 then WP.pokeCompactString p3 (P.toCompactString (offsetCommitRequestGroupInstanceId msg)) else WP.pokeKafkaString p3 (offsetCommitRequestGroupInstanceId msg)) else pure p3)
    p5 <- WP.pokeVersionedArray version 8 (\p x -> wirePokeOffsetCommitRequestTopic version p x) p4 (offsetCommitRequestTopics msg)
    WP.pokeEmptyTaggedFields p5
  | otherwise = error $ "wirePoke OffsetCommitRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for OffsetCommitRequest.
wirePeekOffsetCommitRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetCommitRequest, Ptr Word8)
wirePeekOffsetCommitRequest version _fp _basePtr p0 endPtr
  | version == 7 = do
    (f0_groupid, p1) <- (if version >= 8 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_generationidormemberepoch, p2) <- (if version >= 1 then W.peekInt32BE p1 endPtr else pure (0, p1))
    (f2_memberid, p3) <- (if version >= 1 then (if version >= 8 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr) else pure (P.KafkaString Null, p2))
    (f3_groupinstanceid, p4) <- (if version >= 7 then (if version >= 8 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
    (f4_topics, p5) <- WP.peekVersionedArray version 8 (\p e -> wirePeekOffsetCommitRequestTopic version _fp _basePtr p e) p4 endPtr
    pure (OffsetCommitRequest { offsetCommitRequestGroupId = f0_groupid, offsetCommitRequestGenerationIdOrMemberEpoch = f1_generationidormemberepoch, offsetCommitRequestMemberId = f2_memberid, offsetCommitRequestGroupInstanceId = f3_groupinstanceid, offsetCommitRequestRetentionTimeMs = 0, offsetCommitRequestTopics = f4_topics }, p5)
  | version >= 5 && version <= 6 = do
    (f0_groupid, p1) <- (if version >= 8 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_generationidormemberepoch, p2) <- (if version >= 1 then W.peekInt32BE p1 endPtr else pure (0, p1))
    (f2_memberid, p3) <- (if version >= 1 then (if version >= 8 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr) else pure (P.KafkaString Null, p2))
    (f3_topics, p4) <- WP.peekVersionedArray version 8 (\p e -> wirePeekOffsetCommitRequestTopic version _fp _basePtr p e) p3 endPtr
    pure (OffsetCommitRequest { offsetCommitRequestGroupId = f0_groupid, offsetCommitRequestGenerationIdOrMemberEpoch = f1_generationidormemberepoch, offsetCommitRequestMemberId = f2_memberid, offsetCommitRequestGroupInstanceId = P.KafkaString Null, offsetCommitRequestRetentionTimeMs = 0, offsetCommitRequestTopics = f3_topics }, p4)
  | version >= 2 && version <= 4 = do
    (f0_groupid, p1) <- (if version >= 8 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_generationidormemberepoch, p2) <- (if version >= 1 then W.peekInt32BE p1 endPtr else pure (0, p1))
    (f2_memberid, p3) <- (if version >= 1 then (if version >= 8 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr) else pure (P.KafkaString Null, p2))
    (f3_retentiontimems, p4) <- (if version >= 2 && version <= 4 then W.peekInt64BE p3 endPtr else pure (0, p3))
    (f4_topics, p5) <- WP.peekVersionedArray version 8 (\p e -> wirePeekOffsetCommitRequestTopic version _fp _basePtr p e) p4 endPtr
    pure (OffsetCommitRequest { offsetCommitRequestGroupId = f0_groupid, offsetCommitRequestGenerationIdOrMemberEpoch = f1_generationidormemberepoch, offsetCommitRequestMemberId = f2_memberid, offsetCommitRequestGroupInstanceId = P.KafkaString Null, offsetCommitRequestRetentionTimeMs = f3_retentiontimems, offsetCommitRequestTopics = f4_topics }, p5)
  | version >= 8 && version <= 10 = do
    (f0_groupid, p1) <- (if version >= 8 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_generationidormemberepoch, p2) <- (if version >= 1 then W.peekInt32BE p1 endPtr else pure (0, p1))
    (f2_memberid, p3) <- (if version >= 1 then (if version >= 8 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr) else pure (P.KafkaString Null, p2))
    (f3_groupinstanceid, p4) <- (if version >= 7 then (if version >= 8 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
    (f4_topics, p5) <- WP.peekVersionedArray version 8 (\p e -> wirePeekOffsetCommitRequestTopic version _fp _basePtr p e) p4 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (OffsetCommitRequest { offsetCommitRequestGroupId = f0_groupid, offsetCommitRequestGenerationIdOrMemberEpoch = f1_generationidormemberepoch, offsetCommitRequestMemberId = f2_memberid, offsetCommitRequestGroupInstanceId = f3_groupinstanceid, offsetCommitRequestRetentionTimeMs = 0, offsetCommitRequestTopics = f4_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek OffsetCommitRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec OffsetCommitRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeOffsetCommitRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeOffsetCommitRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekOffsetCommitRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}