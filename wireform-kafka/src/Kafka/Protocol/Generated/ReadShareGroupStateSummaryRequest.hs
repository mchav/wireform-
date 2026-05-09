{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ReadShareGroupStateSummaryRequest
Description : Kafka ReadShareGroupStateSummaryRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 87.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ReadShareGroupStateSummaryRequest
  (
    ReadShareGroupStateSummaryRequest(..),
    ReadStateSummaryData(..),
    PartitionData(..),
    encodeReadShareGroupStateSummaryRequest,
    decodeReadShareGroupStateSummaryRequest,
    maxReadShareGroupStateSummaryRequestVersion
  ) where

import Control.Monad (when)
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
import Data.Bytes.Put (MonadPut)
import Data.Bytes.Serial (Serial(..), serialize, deserialize)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( VarInt(..), VarLong(..), UVarInt(..)
  , KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , CompactString, CompactBytes, CompactArray
  , TaggedFields, emptyTaggedFields, Nullable(..)
  , toCompactString, toCompactBytes, toCompactArray
  )
import qualified Kafka.Protocol.Encoding as E
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
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


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartition pmsg)
    serialize (partitionDataLeaderEpoch pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartition <- deserialize
    fieldleaderepoch <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartition = fieldpartition
      ,
      partitionDataLeaderEpoch = fieldleaderepoch
      }


-- | The data for the topics.
data ReadStateSummaryData = ReadStateSummaryData
  {

  -- | The topic identifier.

  -- Versions: 0+
  readStateSummaryDataTopicId :: !(KafkaUuid)
,

  -- | The data for the partitions.

  -- Versions: 0+
  readStateSummaryDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode ReadStateSummaryData with version-aware field handling.
encodeReadStateSummaryData :: MonadPut m => E.ApiVersion -> ReadStateSummaryData -> m ()
encodeReadStateSummaryData version rmsg =
  do
    serialize (readStateSummaryDataTopicId rmsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (readStateSummaryDataPartitions rmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReadStateSummaryData with version-aware field handling.
decodeReadStateSummaryData :: MonadGet m => E.ApiVersion -> m ReadStateSummaryData
decodeReadStateSummaryData version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReadStateSummaryData
      {
      readStateSummaryDataTopicId = fieldtopicid
      ,
      readStateSummaryDataPartitions = fieldpartitions
      }



data ReadShareGroupStateSummaryRequest = ReadShareGroupStateSummaryRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  readShareGroupStateSummaryRequestGroupId :: !(KafkaString)
,

  -- | The data for the topics.

  -- Versions: 0+
  readShareGroupStateSummaryRequestTopics :: !(KafkaArray (ReadStateSummaryData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ReadShareGroupStateSummaryRequest.
maxReadShareGroupStateSummaryRequestVersion :: Int16
maxReadShareGroupStateSummaryRequestVersion = 1

-- | KafkaMessage instance for ReadShareGroupStateSummaryRequest.
instance KafkaMessage ReadShareGroupStateSummaryRequest where
  messageApiKey = 87
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Encode ReadShareGroupStateSummaryRequest with the given API version.
encodeReadShareGroupStateSummaryRequest :: MonadPut m => E.ApiVersion -> ReadShareGroupStateSummaryRequest -> m ()
encodeReadShareGroupStateSummaryRequest version msg
  | version >= 0 && version <= 1 =
    do
      serialize (toCompactString (readShareGroupStateSummaryRequestGroupId msg))
      E.encodeVersionedArray version 0 encodeReadStateSummaryData (case P.unKafkaArray (readShareGroupStateSummaryRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ReadShareGroupStateSummaryRequest with the given API version.
decodeReadShareGroupStateSummaryRequest :: MonadGet m => E.ApiVersion -> m ReadShareGroupStateSummaryRequest
decodeReadShareGroupStateSummaryRequest version
  | version >= 0 && version <= 1 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReadStateSummaryData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ReadShareGroupStateSummaryRequest
        {
        readShareGroupStateSummaryRequestGroupId = fieldgroupid
        ,
        readShareGroupStateSummaryRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

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

-- | Worst-case wire size of a ReadStateSummaryData.
wireMaxSizeReadStateSummaryData :: Int -> ReadStateSummaryData -> Int
wireMaxSizeReadStateSummaryData _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (readStateSummaryDataPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReadStateSummaryData.
wirePokeReadStateSummaryData :: Int -> Ptr Word8 -> ReadStateSummaryData -> IO (Ptr Word8)
wirePokeReadStateSummaryData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (readStateSummaryDataTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionData version p x) p1 (readStateSummaryDataPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReadStateSummaryData.
wirePeekReadStateSummaryData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReadStateSummaryData, Ptr Word8)
wirePeekReadStateSummaryData version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReadStateSummaryData { readStateSummaryDataTopicId = f0_topicid, readStateSummaryDataPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a ReadShareGroupStateSummaryRequest.
wireMaxSizeReadShareGroupStateSummaryRequest :: Int -> ReadShareGroupStateSummaryRequest -> Int
wireMaxSizeReadShareGroupStateSummaryRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (readShareGroupStateSummaryRequestGroupId msg))
  + (5 + (case P.unKafkaArray (readShareGroupStateSummaryRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReadStateSummaryData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReadShareGroupStateSummaryRequest.
wirePokeReadShareGroupStateSummaryRequest :: Int -> Ptr Word8 -> ReadShareGroupStateSummaryRequest -> IO (Ptr Word8)
wirePokeReadShareGroupStateSummaryRequest version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (readShareGroupStateSummaryRequestGroupId msg))
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReadStateSummaryData version p x) p1 (readShareGroupStateSummaryRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke ReadShareGroupStateSummaryRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ReadShareGroupStateSummaryRequest.
wirePeekReadShareGroupStateSummaryRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReadShareGroupStateSummaryRequest, Ptr Word8)
wirePeekReadShareGroupStateSummaryRequest version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReadStateSummaryData version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (ReadShareGroupStateSummaryRequest { readShareGroupStateSummaryRequestGroupId = f0_groupid, readShareGroupStateSummaryRequestTopics = f1_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek ReadShareGroupStateSummaryRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec ReadShareGroupStateSummaryRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeReadShareGroupStateSummaryRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeReadShareGroupStateSummaryRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekReadShareGroupStateSummaryRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}