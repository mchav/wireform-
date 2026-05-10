{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListOffsetsRequest
Description : Kafka ListOffsetsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 2.



Valid versions: 1-11
Flexible versions: 6+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListOffsetsRequest
  (
    ListOffsetsRequest(..),
    ListOffsetsTopic(..),
    ListOffsetsPartition(..),
    maxListOffsetsRequestVersion
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


-- | Each partition in the request.
data ListOffsetsPartition = ListOffsetsPartition
  {

  -- | The partition index.

  -- Versions: 0+
  listOffsetsPartitionPartitionIndex :: !(Int32)
,

  -- | The current leader epoch.

  -- Versions: 4+
  listOffsetsPartitionCurrentLeaderEpoch :: !(Int32)
,

  -- | The current timestamp.

  -- Versions: 0+
  listOffsetsPartitionTimestamp :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Each topic in the request.
data ListOffsetsTopic = ListOffsetsTopic
  {

  -- | The topic name.

  -- Versions: 0+
  listOffsetsTopicName :: !(KafkaString)
,

  -- | Each partition in the request.

  -- Versions: 0+
  listOffsetsTopicPartitions :: !(KafkaArray (ListOffsetsPartition))

  }
  deriving (Eq, Show, Generic)


data ListOffsetsRequest = ListOffsetsRequest
  {

  -- | The broker ID of the requester, or -1 if this request is being made by a normal consumer.

  -- Versions: 0+
  listOffsetsRequestReplicaId :: !(Int32)
,

  -- | This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_lev

  -- Versions: 2+
  listOffsetsRequestIsolationLevel :: !(Int8)
,

  -- | Each topic in the request.

  -- Versions: 0+
  listOffsetsRequestTopics :: !(KafkaArray (ListOffsetsTopic))
,

  -- | The timeout to await a response in milliseconds for requests that require reading from remote storag

  -- Versions: 10+
  listOffsetsRequestTimeoutMs :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListOffsetsRequest.
maxListOffsetsRequestVersion :: Int16
maxListOffsetsRequestVersion = 11

-- | KafkaMessage instance for ListOffsetsRequest.
instance KafkaMessage ListOffsetsRequest where
  messageApiKey = 2
  messageMinVersion = 1
  messageMaxVersion = 11
  messageFlexibleVersion = Just 6

-- | Worst-case wire size of a ListOffsetsPartition.
wireMaxSizeListOffsetsPartition :: Int -> ListOffsetsPartition -> Int
wireMaxSizeListOffsetsPartition _version msg =
  0
  + 4
  + 4
  + 8
  + 1

-- | Direct-poke encoder for ListOffsetsPartition.
wirePokeListOffsetsPartition :: Int -> Ptr Word8 -> ListOffsetsPartition -> IO (Ptr Word8)
wirePokeListOffsetsPartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (listOffsetsPartitionPartitionIndex msg)
  p2 <- (if version >= 4 then W.pokeInt32BE p1 (listOffsetsPartitionCurrentLeaderEpoch msg) else pure p1)
  p3 <- W.pokeInt64BE p2 (listOffsetsPartitionTimestamp msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for ListOffsetsPartition.
wirePeekListOffsetsPartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListOffsetsPartition, Ptr Word8)
wirePeekListOffsetsPartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_currentleaderepoch, p2) <- (if version >= 4 then W.peekInt32BE p1 endPtr else pure (0, p1))
  (f2_timestamp, p3) <- W.peekInt64BE p2 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (ListOffsetsPartition { listOffsetsPartitionPartitionIndex = f0_partitionindex, listOffsetsPartitionCurrentLeaderEpoch = f1_currentleaderepoch, listOffsetsPartitionTimestamp = f2_timestamp }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultListOffsetsPartition :: ListOffsetsPartition
defaultListOffsetsPartition = ListOffsetsPartition { listOffsetsPartitionPartitionIndex = 0, listOffsetsPartitionCurrentLeaderEpoch = 0, listOffsetsPartitionTimestamp = 0 }

-- | Worst-case wire size of a ListOffsetsTopic.
wireMaxSizeListOffsetsTopic :: Int -> ListOffsetsTopic -> Int
wireMaxSizeListOffsetsTopic _version msg =
  0
  + WP.dualStringMaxSize (listOffsetsTopicName msg)
  + (5 + (case P.unKafkaArray (listOffsetsTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeListOffsetsPartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ListOffsetsTopic.
wirePokeListOffsetsTopic :: Int -> Ptr Word8 -> ListOffsetsTopic -> IO (Ptr Word8)
wirePokeListOffsetsTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 6 then WP.pokeCompactString p0 (P.toCompactString (listOffsetsTopicName msg)) else WP.pokeKafkaString p0 (listOffsetsTopicName msg))
  p2 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsPartition version p x) p1 (listOffsetsTopicPartitions msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ListOffsetsTopic.
wirePeekListOffsetsTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListOffsetsTopic, Ptr Word8)
wirePeekListOffsetsTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 6 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsPartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ListOffsetsTopic { listOffsetsTopicName = f0_name, listOffsetsTopicPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultListOffsetsTopic :: ListOffsetsTopic
defaultListOffsetsTopic = ListOffsetsTopic { listOffsetsTopicName = P.KafkaString Null, listOffsetsTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a ListOffsetsRequest.
wireMaxSizeListOffsetsRequest :: Int -> ListOffsetsRequest -> Int
wireMaxSizeListOffsetsRequest _version msg =
  0
  + 4
  + 1
  + (5 + (case P.unKafkaArray (listOffsetsRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeListOffsetsTopic _version x ) v); P.Null -> 0 }))
  + 4
  + 1

-- | Direct-poke encoder for ListOffsetsRequest.
wirePokeListOffsetsRequest :: Int -> Ptr Word8 -> ListOffsetsRequest -> IO (Ptr Word8)
wirePokeListOffsetsRequest version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listOffsetsRequestReplicaId msg)
    p2 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsTopic version p x) p1 (listOffsetsRequestTopics msg)
    pure p2
  | version >= 10 && version <= 11 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listOffsetsRequestReplicaId msg)
    p2 <- (if version >= 2 then W.pokeWord8 p1 (fromIntegral (listOffsetsRequestIsolationLevel msg)) else pure p1)
    p3 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsTopic version p x) p2 (listOffsetsRequestTopics msg)
    p4 <- (if version >= 10 then W.pokeInt32BE p3 (listOffsetsRequestTimeoutMs msg) else pure p3)
    WP.pokeEmptyTaggedFields p4
  | version >= 2 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listOffsetsRequestReplicaId msg)
    p2 <- (if version >= 2 then W.pokeWord8 p1 (fromIntegral (listOffsetsRequestIsolationLevel msg)) else pure p1)
    p3 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsTopic version p x) p2 (listOffsetsRequestTopics msg)
    pure p3
  | version >= 6 && version <= 9 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listOffsetsRequestReplicaId msg)
    p2 <- (if version >= 2 then W.pokeWord8 p1 (fromIntegral (listOffsetsRequestIsolationLevel msg)) else pure p1)
    p3 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsTopic version p x) p2 (listOffsetsRequestTopics msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke ListOffsetsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ListOffsetsRequest.
wirePeekListOffsetsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListOffsetsRequest, Ptr Word8)
wirePeekListOffsetsRequest version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsTopic version _fp _basePtr p e) p1 endPtr
    pure (ListOffsetsRequest { listOffsetsRequestReplicaId = f0_replicaid, listOffsetsRequestIsolationLevel = 0, listOffsetsRequestTopics = f1_topics, listOffsetsRequestTimeoutMs = 0 }, p2)
  | version >= 10 && version <= 11 = do
    (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
    (f1_isolationlevel, p2) <- (if version >= 2 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p1 endPtr else pure (0, p1))
    (f2_topics, p3) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsTopic version _fp _basePtr p e) p2 endPtr
    (f3_timeoutms, p4) <- (if version >= 10 then W.peekInt32BE p3 endPtr else pure (0, p3))
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (ListOffsetsRequest { listOffsetsRequestReplicaId = f0_replicaid, listOffsetsRequestIsolationLevel = f1_isolationlevel, listOffsetsRequestTopics = f2_topics, listOffsetsRequestTimeoutMs = f3_timeoutms }, pTagsEnd)
  | version >= 2 && version <= 5 = do
    (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
    (f1_isolationlevel, p2) <- (if version >= 2 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p1 endPtr else pure (0, p1))
    (f2_topics, p3) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsTopic version _fp _basePtr p e) p2 endPtr
    pure (ListOffsetsRequest { listOffsetsRequestReplicaId = f0_replicaid, listOffsetsRequestIsolationLevel = f1_isolationlevel, listOffsetsRequestTopics = f2_topics, listOffsetsRequestTimeoutMs = 0 }, p3)
  | version >= 6 && version <= 9 = do
    (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
    (f1_isolationlevel, p2) <- (if version >= 2 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p1 endPtr else pure (0, p1))
    (f2_topics, p3) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsTopic version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (ListOffsetsRequest { listOffsetsRequestReplicaId = f0_replicaid, listOffsetsRequestIsolationLevel = f1_isolationlevel, listOffsetsRequestTopics = f2_topics, listOffsetsRequestTimeoutMs = 0 }, pTagsEnd)
  | otherwise = error $ "wirePeek ListOffsetsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ListOffsetsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeListOffsetsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeListOffsetsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekListOffsetsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}