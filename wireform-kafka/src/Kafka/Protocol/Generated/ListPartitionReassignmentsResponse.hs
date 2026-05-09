{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListPartitionReassignmentsResponse
Description : Kafka ListPartitionReassignmentsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 46.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListPartitionReassignmentsResponse
  (
    ListPartitionReassignmentsResponse(..),
    OngoingTopicReassignment(..),
    OngoingPartitionReassignment(..),
    maxListPartitionReassignmentsResponseVersion
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


-- | The ongoing reassignments for each partition.
data OngoingPartitionReassignment = OngoingPartitionReassignment
  {

  -- | The index of the partition.

  -- Versions: 0+
  ongoingPartitionReassignmentPartitionIndex :: !(Int32)
,

  -- | The current replica set.

  -- Versions: 0+
  ongoingPartitionReassignmentReplicas :: !(KafkaArray (Int32))
,

  -- | The set of replicas we are currently adding.

  -- Versions: 0+
  ongoingPartitionReassignmentAddingReplicas :: !(KafkaArray (Int32))
,

  -- | The set of replicas we are currently removing.

  -- Versions: 0+
  ongoingPartitionReassignmentRemovingReplicas :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)

-- | The ongoing reassignments for each topic.
data OngoingTopicReassignment = OngoingTopicReassignment
  {

  -- | The topic name.

  -- Versions: 0+
  ongoingTopicReassignmentName :: !(KafkaString)
,

  -- | The ongoing reassignments for each partition.

  -- Versions: 0+
  ongoingTopicReassignmentPartitions :: !(KafkaArray (OngoingPartitionReassignment))

  }
  deriving (Eq, Show, Generic)


data ListPartitionReassignmentsResponse = ListPartitionReassignmentsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  listPartitionReassignmentsResponseThrottleTimeMs :: !(Int32)
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 0+
  listPartitionReassignmentsResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  listPartitionReassignmentsResponseErrorMessage :: !(KafkaString)
,

  -- | The ongoing reassignments for each topic.

  -- Versions: 0+
  listPartitionReassignmentsResponseTopics :: !(KafkaArray (OngoingTopicReassignment))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListPartitionReassignmentsResponse.
maxListPartitionReassignmentsResponseVersion :: Int16
maxListPartitionReassignmentsResponseVersion = 0

-- | KafkaMessage instance for ListPartitionReassignmentsResponse.
instance KafkaMessage ListPartitionReassignmentsResponse where
  messageApiKey = 46
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a OngoingPartitionReassignment.
wireMaxSizeOngoingPartitionReassignment :: Int -> OngoingPartitionReassignment -> Int
wireMaxSizeOngoingPartitionReassignment _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (ongoingPartitionReassignmentReplicas msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (ongoingPartitionReassignmentAddingReplicas msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (ongoingPartitionReassignmentRemovingReplicas msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OngoingPartitionReassignment.
wirePokeOngoingPartitionReassignment :: Int -> Ptr Word8 -> OngoingPartitionReassignment -> IO (Ptr Word8)
wirePokeOngoingPartitionReassignment version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (ongoingPartitionReassignmentPartitionIndex msg)
  p2 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p1 (ongoingPartitionReassignmentReplicas msg)
  p3 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p2 (ongoingPartitionReassignmentAddingReplicas msg)
  p4 <- WP.pokeVersionedArray version 0 W.pokeInt32BE p3 (ongoingPartitionReassignmentRemovingReplicas msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for OngoingPartitionReassignment.
wirePeekOngoingPartitionReassignment :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OngoingPartitionReassignment, Ptr Word8)
wirePeekOngoingPartitionReassignment version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_replicas, p2) <- WP.peekVersionedArray version 0 W.peekInt32BE p1 endPtr
  (f2_addingreplicas, p3) <- WP.peekVersionedArray version 0 W.peekInt32BE p2 endPtr
  (f3_removingreplicas, p4) <- WP.peekVersionedArray version 0 W.peekInt32BE p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (OngoingPartitionReassignment { ongoingPartitionReassignmentPartitionIndex = f0_partitionindex, ongoingPartitionReassignmentReplicas = f1_replicas, ongoingPartitionReassignmentAddingReplicas = f2_addingreplicas, ongoingPartitionReassignmentRemovingReplicas = f3_removingreplicas }, pTagsEnd)

-- | Worst-case wire size of a OngoingTopicReassignment.
wireMaxSizeOngoingTopicReassignment :: Int -> OngoingTopicReassignment -> Int
wireMaxSizeOngoingTopicReassignment _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (ongoingTopicReassignmentName msg))
  + (5 + (case P.unKafkaArray (ongoingTopicReassignmentPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOngoingPartitionReassignment _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OngoingTopicReassignment.
wirePokeOngoingTopicReassignment :: Int -> Ptr Word8 -> OngoingTopicReassignment -> IO (Ptr Word8)
wirePokeOngoingTopicReassignment version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (ongoingTopicReassignmentName msg))
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeOngoingPartitionReassignment version p x) p1 (ongoingTopicReassignmentPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for OngoingTopicReassignment.
wirePeekOngoingTopicReassignment :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OngoingTopicReassignment, Ptr Word8)
wirePeekOngoingTopicReassignment version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekOngoingPartitionReassignment version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (OngoingTopicReassignment { ongoingTopicReassignmentName = f0_name, ongoingTopicReassignmentPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a ListPartitionReassignmentsResponse.
wireMaxSizeListPartitionReassignmentsResponse :: Int -> ListPartitionReassignmentsResponse -> Int
wireMaxSizeListPartitionReassignmentsResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (listPartitionReassignmentsResponseErrorMessage msg))
  + (5 + (case P.unKafkaArray (listPartitionReassignmentsResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOngoingTopicReassignment _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ListPartitionReassignmentsResponse.
wirePokeListPartitionReassignmentsResponse :: Int -> Ptr Word8 -> ListPartitionReassignmentsResponse -> IO (Ptr Word8)
wirePokeListPartitionReassignmentsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listPartitionReassignmentsResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (listPartitionReassignmentsResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (listPartitionReassignmentsResponseErrorMessage msg))
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeOngoingTopicReassignment version p x) p3 (listPartitionReassignmentsResponseTopics msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke ListPartitionReassignmentsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ListPartitionReassignmentsResponse.
wirePeekListPartitionReassignmentsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListPartitionReassignmentsResponse, Ptr Word8)
wirePeekListPartitionReassignmentsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_topics, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekOngoingTopicReassignment version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (ListPartitionReassignmentsResponse { listPartitionReassignmentsResponseThrottleTimeMs = f0_throttletimems, listPartitionReassignmentsResponseErrorCode = f1_errorcode, listPartitionReassignmentsResponseErrorMessage = f2_errormessage, listPartitionReassignmentsResponseTopics = f3_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek ListPartitionReassignmentsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ListPartitionReassignmentsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeListPartitionReassignmentsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeListPartitionReassignmentsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekListPartitionReassignmentsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}