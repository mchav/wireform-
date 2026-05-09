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
    encodeReadShareGroupStateRequest,
    decodeReadShareGroupStateRequest,
    maxReadShareGroupStateRequestVersion
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


-- | Encode ReadStateData with version-aware field handling.
encodeReadStateData :: MonadPut m => E.ApiVersion -> ReadStateData -> m ()
encodeReadStateData version rmsg =
  do
    serialize (readStateDataTopicId rmsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (readStateDataPartitions rmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReadStateData with version-aware field handling.
decodeReadStateData :: MonadGet m => E.ApiVersion -> m ReadStateData
decodeReadStateData version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReadStateData
      {
      readStateDataTopicId = fieldtopicid
      ,
      readStateDataPartitions = fieldpartitions
      }



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

-- | Encode ReadShareGroupStateRequest with the given API version.
encodeReadShareGroupStateRequest :: MonadPut m => E.ApiVersion -> ReadShareGroupStateRequest -> m ()
encodeReadShareGroupStateRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (readShareGroupStateRequestGroupId msg))
      E.encodeVersionedArray version 0 encodeReadStateData (case P.unKafkaArray (readShareGroupStateRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ReadShareGroupStateRequest with the given API version.
decodeReadShareGroupStateRequest :: MonadGet m => E.ApiVersion -> m ReadShareGroupStateRequest
decodeReadShareGroupStateRequest version
  | version == 0 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReadStateData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ReadShareGroupStateRequest
        {
        readShareGroupStateRequestGroupId = fieldgroupid
        ,
        readShareGroupStateRequestTopics = fieldtopics
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