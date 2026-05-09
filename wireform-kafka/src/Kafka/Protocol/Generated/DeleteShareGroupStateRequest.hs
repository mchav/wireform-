{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteShareGroupStateRequest
Description : Kafka DeleteShareGroupStateRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 86.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteShareGroupStateRequest
  (
    DeleteShareGroupStateRequest(..),
    DeleteStateData(..),
    PartitionData(..),
    encodeDeleteShareGroupStateRequest,
    decodeDeleteShareGroupStateRequest,
    maxDeleteShareGroupStateRequestVersion
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

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartition pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartition <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartition = fieldpartition
      }


-- | The data for the topics.
data DeleteStateData = DeleteStateData
  {

  -- | The topic identifier.

  -- Versions: 0+
  deleteStateDataTopicId :: !(KafkaUuid)
,

  -- | The data for the partitions.

  -- Versions: 0+
  deleteStateDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteStateData with version-aware field handling.
encodeDeleteStateData :: MonadPut m => E.ApiVersion -> DeleteStateData -> m ()
encodeDeleteStateData version dmsg =
  do
    serialize (deleteStateDataTopicId dmsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (deleteStateDataPartitions dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteStateData with version-aware field handling.
decodeDeleteStateData :: MonadGet m => E.ApiVersion -> m DeleteStateData
decodeDeleteStateData version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteStateData
      {
      deleteStateDataTopicId = fieldtopicid
      ,
      deleteStateDataPartitions = fieldpartitions
      }



data DeleteShareGroupStateRequest = DeleteShareGroupStateRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  deleteShareGroupStateRequestGroupId :: !(KafkaString)
,

  -- | The data for the topics.

  -- Versions: 0+
  deleteShareGroupStateRequestTopics :: !(KafkaArray (DeleteStateData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteShareGroupStateRequest.
maxDeleteShareGroupStateRequestVersion :: Int16
maxDeleteShareGroupStateRequestVersion = 0

-- | KafkaMessage instance for DeleteShareGroupStateRequest.
instance KafkaMessage DeleteShareGroupStateRequest where
  messageApiKey = 86
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode DeleteShareGroupStateRequest with the given API version.
encodeDeleteShareGroupStateRequest :: MonadPut m => E.ApiVersion -> DeleteShareGroupStateRequest -> m ()
encodeDeleteShareGroupStateRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (deleteShareGroupStateRequestGroupId msg))
      E.encodeVersionedArray version 0 encodeDeleteStateData (case P.unKafkaArray (deleteShareGroupStateRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteShareGroupStateRequest with the given API version.
decodeDeleteShareGroupStateRequest :: MonadGet m => E.ApiVersion -> m DeleteShareGroupStateRequest
decodeDeleteShareGroupStateRequest version
  | version == 0 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDeleteStateData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteShareGroupStateRequest
        {
        deleteShareGroupStateRequestGroupId = fieldgroupid
        ,
        deleteShareGroupStateRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartition msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p1 else pure p1

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partition, p1) <- W.peekInt32BE p0 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p1 endPtr else pure p1
  pure (PartitionData { partitionDataPartition = f0_partition }, pTagsEnd)

-- | Worst-case wire size of a DeleteStateData.
wireMaxSizeDeleteStateData :: Int -> DeleteStateData -> Int
wireMaxSizeDeleteStateData _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (deleteStateDataPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteStateData.
wirePokeDeleteStateData :: Int -> Ptr Word8 -> DeleteStateData -> IO (Ptr Word8)
wirePokeDeleteStateData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (deleteStateDataTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionData version p x) p1 (deleteStateDataPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DeleteStateData.
wirePeekDeleteStateData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteStateData, Ptr Word8)
wirePeekDeleteStateData version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DeleteStateData { deleteStateDataTopicId = f0_topicid, deleteStateDataPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a DeleteShareGroupStateRequest.
wireMaxSizeDeleteShareGroupStateRequest :: Int -> DeleteShareGroupStateRequest -> Int
wireMaxSizeDeleteShareGroupStateRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (deleteShareGroupStateRequestGroupId msg))
  + (5 + (case P.unKafkaArray (deleteShareGroupStateRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeleteStateData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteShareGroupStateRequest.
wirePokeDeleteShareGroupStateRequest :: Int -> Ptr Word8 -> DeleteShareGroupStateRequest -> IO (Ptr Word8)
wirePokeDeleteShareGroupStateRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (deleteShareGroupStateRequestGroupId msg))
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDeleteStateData version p x) p1 (deleteShareGroupStateRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke DeleteShareGroupStateRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteShareGroupStateRequest.
wirePeekDeleteShareGroupStateRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteShareGroupStateRequest, Ptr Word8)
wirePeekDeleteShareGroupStateRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDeleteStateData version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DeleteShareGroupStateRequest { deleteShareGroupStateRequestGroupId = f0_groupid, deleteShareGroupStateRequestTopics = f1_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek DeleteShareGroupStateRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DeleteShareGroupStateRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteShareGroupStateRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteShareGroupStateRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteShareGroupStateRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}