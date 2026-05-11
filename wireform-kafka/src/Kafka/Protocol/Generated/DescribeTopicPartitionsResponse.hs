{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeTopicPartitionsResponse
Description : Kafka DescribeTopicPartitionsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 75.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeTopicPartitionsResponse
  (
    DescribeTopicPartitionsResponse(..),
    DescribeTopicPartitionsResponseTopic(..),
    DescribeTopicPartitionsResponsePartition(..),
    Cursor(..),
    maxDescribeTopicPartitionsResponseVersion
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


-- | Each partition in the topic.
data DescribeTopicPartitionsResponsePartition = DescribeTopicPartitionsResponsePartition
  {

  -- | The partition error, or 0 if there was no error.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionErrorCode :: !(Int16)
,

  -- | The partition index.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The ID of the leader broker.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionLeaderId :: !(Int32)
,

  -- | The leader epoch of this partition.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionLeaderEpoch :: !(Int32)
,

  -- | The set of all nodes that host this partition.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionReplicaNodes :: !(KafkaArray (Int32))
,

  -- | The set of nodes that are in sync with the leader for this partition.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionIsrNodes :: !(KafkaArray (Int32))
,

  -- | The new eligible leader replicas otherwise.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionEligibleLeaderReplicas :: !(KafkaArray (Int32))
,

  -- | The last known ELR.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionLastKnownElr :: !(KafkaArray (Int32))
,

  -- | The set of offline replicas of this partition.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionOfflineReplicas :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)

-- | Each topic in the response.
data DescribeTopicPartitionsResponseTopic = DescribeTopicPartitionsResponseTopic
  {

  -- | The topic error, or 0 if there was no error.

  -- Versions: 0+
  describeTopicPartitionsResponseTopicErrorCode :: !(Int16)
,

  -- | The topic name.

  -- Versions: 0+
  describeTopicPartitionsResponseTopicName :: !(KafkaString)
,

  -- | The topic id.

  -- Versions: 0+
  describeTopicPartitionsResponseTopicTopicId :: !(KafkaUuid)
,

  -- | True if the topic is internal.

  -- Versions: 0+
  describeTopicPartitionsResponseTopicIsInternal :: !(Bool)
,

  -- | Each partition in the topic.

  -- Versions: 0+
  describeTopicPartitionsResponseTopicPartitions :: !(KafkaArray (DescribeTopicPartitionsResponsePartition))
,

  -- | 32-bit bitfield to represent authorized operations for this topic.

  -- Versions: 0+
  describeTopicPartitionsResponseTopicTopicAuthorizedOperations :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | The next topic and partition index to fetch details for.
data Cursor = Cursor
  {

  -- | The name for the first topic to process.

  -- Versions: 0+
  cursorTopicName :: !(KafkaString)
,

  -- | The partition index to start with.

  -- Versions: 0+
  cursorPartitionIndex :: !(Int32)

  }
  deriving (Eq, Show, Generic)


data DescribeTopicPartitionsResponse = DescribeTopicPartitionsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeTopicPartitionsResponseThrottleTimeMs :: !(Int32)
,

  -- | Each topic in the response.

  -- Versions: 0+
  describeTopicPartitionsResponseTopics :: !(KafkaArray (DescribeTopicPartitionsResponseTopic))
,

  -- | The next topic and partition index to fetch details for.

  -- Versions: 0+
  describeTopicPartitionsResponseNextCursor :: !(Nullable (Cursor))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeTopicPartitionsResponse.
maxDescribeTopicPartitionsResponseVersion :: Int16
maxDescribeTopicPartitionsResponseVersion = 0

-- | KafkaMessage instance for DescribeTopicPartitionsResponse.
instance KafkaMessage DescribeTopicPartitionsResponse where
  messageApiKey = 75
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a DescribeTopicPartitionsResponsePartition.
wireMaxSizeDescribeTopicPartitionsResponsePartition :: Int -> DescribeTopicPartitionsResponsePartition -> Int
wireMaxSizeDescribeTopicPartitionsResponsePartition _version msg =
  0
  + 2
  + 4
  + 4
  + 4
  + (5 + (case P.unKafkaArray (describeTopicPartitionsResponsePartitionReplicaNodes msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (describeTopicPartitionsResponsePartitionIsrNodes msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (describeTopicPartitionsResponsePartitionEligibleLeaderReplicas msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (describeTopicPartitionsResponsePartitionLastKnownElr msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (describeTopicPartitionsResponsePartitionOfflineReplicas msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeTopicPartitionsResponsePartition.
wirePokeDescribeTopicPartitionsResponsePartition :: Int -> Ptr Word8 -> DescribeTopicPartitionsResponsePartition -> IO (Ptr Word8)
wirePokeDescribeTopicPartitionsResponsePartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (describeTopicPartitionsResponsePartitionErrorCode msg)
  p2 <- W.pokeInt32BE p1 (describeTopicPartitionsResponsePartitionPartitionIndex msg)
  p3 <- W.pokeInt32BE p2 (describeTopicPartitionsResponsePartitionLeaderId msg)
  p4 <- W.pokeInt32BE p3 (describeTopicPartitionsResponsePartitionLeaderEpoch msg)
  p5 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p4 (describeTopicPartitionsResponsePartitionReplicaNodes msg)
  p6 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p5 (describeTopicPartitionsResponsePartitionIsrNodes msg)
  p7 <- WP.pokeVersionedNullableArray version 0 W.pokeInt32BE p6 (describeTopicPartitionsResponsePartitionEligibleLeaderReplicas msg)
  p8 <- WP.pokeVersionedNullableArray version 0 W.pokeInt32BE p7 (describeTopicPartitionsResponsePartitionLastKnownElr msg)
  p9 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p8 (describeTopicPartitionsResponsePartitionOfflineReplicas msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p9 else pure p9

-- | Direct-poke decoder for DescribeTopicPartitionsResponsePartition.
wirePeekDescribeTopicPartitionsResponsePartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeTopicPartitionsResponsePartition, Ptr Word8)
wirePeekDescribeTopicPartitionsResponsePartition version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_partitionindex, p2) <- W.peekInt32BE p1 endPtr
  (f2_leaderid, p3) <- W.peekInt32BE p2 endPtr
  (f3_leaderepoch, p4) <- W.peekInt32BE p3 endPtr
  (f4_replicanodes, p5) <- WP.peekVersionedArray version 0 W.peekInt32BE p4 endPtr
  (f5_isrnodes, p6) <- WP.peekVersionedArray version 0 W.peekInt32BE p5 endPtr
  (f6_eligibleleaderreplicas, p7) <- WP.peekVersionedNullableArray version 0 W.peekInt32BE p6 endPtr
  (f7_lastknownelr, p8) <- WP.peekVersionedNullableArray version 0 W.peekInt32BE p7 endPtr
  (f8_offlinereplicas, p9) <- WP.peekVersionedArray version 0 W.peekInt32BE p8 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p9 endPtr else pure p9
  pure (DescribeTopicPartitionsResponsePartition { describeTopicPartitionsResponsePartitionErrorCode = f0_errorcode, describeTopicPartitionsResponsePartitionPartitionIndex = f1_partitionindex, describeTopicPartitionsResponsePartitionLeaderId = f2_leaderid, describeTopicPartitionsResponsePartitionLeaderEpoch = f3_leaderepoch, describeTopicPartitionsResponsePartitionReplicaNodes = f4_replicanodes, describeTopicPartitionsResponsePartitionIsrNodes = f5_isrnodes, describeTopicPartitionsResponsePartitionEligibleLeaderReplicas = f6_eligibleleaderreplicas, describeTopicPartitionsResponsePartitionLastKnownElr = f7_lastknownelr, describeTopicPartitionsResponsePartitionOfflineReplicas = f8_offlinereplicas }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribeTopicPartitionsResponsePartition :: DescribeTopicPartitionsResponsePartition
defaultDescribeTopicPartitionsResponsePartition = DescribeTopicPartitionsResponsePartition { describeTopicPartitionsResponsePartitionErrorCode = 0, describeTopicPartitionsResponsePartitionPartitionIndex = 0, describeTopicPartitionsResponsePartitionLeaderId = 0, describeTopicPartitionsResponsePartitionLeaderEpoch = -1, describeTopicPartitionsResponsePartitionReplicaNodes = P.mkKafkaArray V.empty, describeTopicPartitionsResponsePartitionIsrNodes = P.mkKafkaArray V.empty, describeTopicPartitionsResponsePartitionEligibleLeaderReplicas = P.KafkaArray P.Null, describeTopicPartitionsResponsePartitionLastKnownElr = P.KafkaArray P.Null, describeTopicPartitionsResponsePartitionOfflineReplicas = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a DescribeTopicPartitionsResponseTopic.
wireMaxSizeDescribeTopicPartitionsResponseTopic :: Int -> DescribeTopicPartitionsResponseTopic -> Int
wireMaxSizeDescribeTopicPartitionsResponseTopic _version msg =
  0
  + 2
  + WP.dualStringMaxSize (describeTopicPartitionsResponseTopicName msg)
  + 16
  + 1
  + (5 + (case P.unKafkaArray (describeTopicPartitionsResponseTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeTopicPartitionsResponsePartition _version x ) v); P.Null -> 0 }))
  + 4
  + 1

-- | Direct-poke encoder for DescribeTopicPartitionsResponseTopic.
wirePokeDescribeTopicPartitionsResponseTopic :: Int -> Ptr Word8 -> DescribeTopicPartitionsResponseTopic -> IO (Ptr Word8)
wirePokeDescribeTopicPartitionsResponseTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (describeTopicPartitionsResponseTopicErrorCode msg)
  p2 <- (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (describeTopicPartitionsResponseTopicName msg)) else WP.pokeKafkaString p1 (describeTopicPartitionsResponseTopicName msg))
  p3 <- WP.pokeKafkaUuid p2 (describeTopicPartitionsResponseTopicTopicId msg)
  p4 <- W.pokeWord8 p3 (if (describeTopicPartitionsResponseTopicIsInternal msg) then 1 else 0)
  p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDescribeTopicPartitionsResponsePartition version p x) p4 (describeTopicPartitionsResponseTopicPartitions msg)
  p6 <- W.pokeInt32BE p5 (describeTopicPartitionsResponseTopicTopicAuthorizedOperations msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p6 else pure p6

-- | Direct-poke decoder for DescribeTopicPartitionsResponseTopic.
wirePeekDescribeTopicPartitionsResponseTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeTopicPartitionsResponseTopic, Ptr Word8)
wirePeekDescribeTopicPartitionsResponseTopic version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_name, p2) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_topicid, p3) <- WP.peekKafkaUuid p2 endPtr
  (f3_isinternal, p4) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr
  (f4_partitions, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDescribeTopicPartitionsResponsePartition version _fp _basePtr p e) p4 endPtr
  (f5_topicauthorizedoperations, p6) <- W.peekInt32BE p5 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p6 endPtr else pure p6
  pure (DescribeTopicPartitionsResponseTopic { describeTopicPartitionsResponseTopicErrorCode = f0_errorcode, describeTopicPartitionsResponseTopicName = f1_name, describeTopicPartitionsResponseTopicTopicId = f2_topicid, describeTopicPartitionsResponseTopicIsInternal = f3_isinternal, describeTopicPartitionsResponseTopicPartitions = f4_partitions, describeTopicPartitionsResponseTopicTopicAuthorizedOperations = f5_topicauthorizedoperations }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribeTopicPartitionsResponseTopic :: DescribeTopicPartitionsResponseTopic
defaultDescribeTopicPartitionsResponseTopic = DescribeTopicPartitionsResponseTopic { describeTopicPartitionsResponseTopicErrorCode = 0, describeTopicPartitionsResponseTopicName = P.KafkaString Null, describeTopicPartitionsResponseTopicTopicId = P.nullUuid, describeTopicPartitionsResponseTopicIsInternal = False, describeTopicPartitionsResponseTopicPartitions = P.mkKafkaArray V.empty, describeTopicPartitionsResponseTopicTopicAuthorizedOperations = -2147483648 }

-- | Worst-case wire size of a Cursor.
wireMaxSizeCursor :: Int -> Cursor -> Int
wireMaxSizeCursor _version msg =
  0
  + WP.dualStringMaxSize (cursorTopicName msg)
  + 4
  + 1

-- | Direct-poke encoder for Cursor.
wirePokeCursor :: Int -> Ptr Word8 -> Cursor -> IO (Ptr Word8)
wirePokeCursor version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (cursorTopicName msg)) else WP.pokeKafkaString p0 (cursorTopicName msg))
  p2 <- W.pokeInt32BE p1 (cursorPartitionIndex msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for Cursor.
wirePeekCursor :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Cursor, Ptr Word8)
wirePeekCursor version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitionindex, p2) <- W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (Cursor { cursorTopicName = f0_topicname, cursorPartitionIndex = f1_partitionindex }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultCursor :: Cursor
defaultCursor = Cursor { cursorTopicName = P.KafkaString Null, cursorPartitionIndex = 0 }

-- | Worst-case wire size of a DescribeTopicPartitionsResponse.
wireMaxSizeDescribeTopicPartitionsResponse :: Int -> DescribeTopicPartitionsResponse -> Int
wireMaxSizeDescribeTopicPartitionsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (describeTopicPartitionsResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeTopicPartitionsResponseTopic _version x ) v); P.Null -> 0 }))
  + (case (describeTopicPartitionsResponseNextCursor msg) of { P.Null -> 1; P.NotNull s -> 1 + wireMaxSizeCursor _version s })
  + 1

-- | Direct-poke encoder for DescribeTopicPartitionsResponse.
wirePokeDescribeTopicPartitionsResponse :: Int -> Ptr Word8 -> DescribeTopicPartitionsResponse -> IO (Ptr Word8)
wirePokeDescribeTopicPartitionsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeTopicPartitionsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDescribeTopicPartitionsResponseTopic version p x) p1 (describeTopicPartitionsResponseTopics msg)
    p3 <- (case (describeTopicPartitionsResponseNextCursor msg) of { P.Null -> W.pokeWord8 p2 0; P.NotNull s -> W.pokeWord8 p2 1 >>= \p' -> wirePokeCursor version p' s })
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke DescribeTopicPartitionsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeTopicPartitionsResponse.
wirePeekDescribeTopicPartitionsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeTopicPartitionsResponse, Ptr Word8)
wirePeekDescribeTopicPartitionsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDescribeTopicPartitionsResponseTopic version _fp _basePtr p e) p1 endPtr
    (f2_nextcursor, p3) <- (do { (flag, pAfterFlag) <- W.peekWord8 p2 endPtr; case flag of { 0 -> pure (P.Null, pAfterFlag); _ -> do { (s, p'') <- wirePeekCursor version _fp _basePtr pAfterFlag endPtr; pure (P.NotNull s, p'') } } })
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (DescribeTopicPartitionsResponse { describeTopicPartitionsResponseThrottleTimeMs = f0_throttletimems, describeTopicPartitionsResponseTopics = f1_topics, describeTopicPartitionsResponseNextCursor = f2_nextcursor }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeTopicPartitionsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec DescribeTopicPartitionsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeTopicPartitionsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeTopicPartitionsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeTopicPartitionsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}