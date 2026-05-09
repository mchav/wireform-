{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.InitializeShareGroupStateRequest
Description : Kafka InitializeShareGroupStateRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 83.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.InitializeShareGroupStateRequest
  (
    InitializeShareGroupStateRequest(..),
    InitializeStateData(..),
    PartitionData(..),
    encodeInitializeShareGroupStateRequest,
    decodeInitializeShareGroupStateRequest,
    maxInitializeShareGroupStateRequestVersion
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

  -- | The state epoch for this share-partition.

  -- Versions: 0+
  partitionDataStateEpoch :: !(Int32)
,

  -- | The share-partition start offset, or -1 if the start offset is not being initialized.

  -- Versions: 0+
  partitionDataStartOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartition pmsg)
    serialize (partitionDataStateEpoch pmsg)
    serialize (partitionDataStartOffset pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartition <- deserialize
    fieldstateepoch <- deserialize
    fieldstartoffset <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartition = fieldpartition
      ,
      partitionDataStateEpoch = fieldstateepoch
      ,
      partitionDataStartOffset = fieldstartoffset
      }


-- | The data for the topics.
data InitializeStateData = InitializeStateData
  {

  -- | The topic identifier.

  -- Versions: 0+
  initializeStateDataTopicId :: !(KafkaUuid)
,

  -- | The data for the partitions.

  -- Versions: 0+
  initializeStateDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode InitializeStateData with version-aware field handling.
encodeInitializeStateData :: MonadPut m => E.ApiVersion -> InitializeStateData -> m ()
encodeInitializeStateData version imsg =
  do
    serialize (initializeStateDataTopicId imsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (initializeStateDataPartitions imsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode InitializeStateData with version-aware field handling.
decodeInitializeStateData :: MonadGet m => E.ApiVersion -> m InitializeStateData
decodeInitializeStateData version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure InitializeStateData
      {
      initializeStateDataTopicId = fieldtopicid
      ,
      initializeStateDataPartitions = fieldpartitions
      }



data InitializeShareGroupStateRequest = InitializeShareGroupStateRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  initializeShareGroupStateRequestGroupId :: !(KafkaString)
,

  -- | The data for the topics.

  -- Versions: 0+
  initializeShareGroupStateRequestTopics :: !(KafkaArray (InitializeStateData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for InitializeShareGroupStateRequest.
maxInitializeShareGroupStateRequestVersion :: Int16
maxInitializeShareGroupStateRequestVersion = 0

-- | KafkaMessage instance for InitializeShareGroupStateRequest.
instance KafkaMessage InitializeShareGroupStateRequest where
  messageApiKey = 83
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode InitializeShareGroupStateRequest with the given API version.
encodeInitializeShareGroupStateRequest :: MonadPut m => E.ApiVersion -> InitializeShareGroupStateRequest -> m ()
encodeInitializeShareGroupStateRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (initializeShareGroupStateRequestGroupId msg))
      E.encodeVersionedArray version 0 encodeInitializeStateData (case P.unKafkaArray (initializeShareGroupStateRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode InitializeShareGroupStateRequest with the given API version.
decodeInitializeShareGroupStateRequest :: MonadGet m => E.ApiVersion -> m InitializeShareGroupStateRequest
decodeInitializeShareGroupStateRequest version
  | version == 0 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeInitializeStateData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure InitializeShareGroupStateRequest
        {
        initializeShareGroupStateRequestGroupId = fieldgroupid
        ,
        initializeShareGroupStateRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 4
  + 8
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartition msg)
  p2 <- W.pokeInt32BE p1 (partitionDataStateEpoch msg)
  p3 <- W.pokeInt64BE p2 (partitionDataStartOffset msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partition, p1) <- W.peekInt32BE p0 endPtr
  (f1_stateepoch, p2) <- W.peekInt32BE p1 endPtr
  (f2_startoffset, p3) <- W.peekInt64BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (PartitionData { partitionDataPartition = f0_partition, partitionDataStateEpoch = f1_stateepoch, partitionDataStartOffset = f2_startoffset }, pTagsEnd)

-- | Worst-case wire size of a InitializeStateData.
wireMaxSizeInitializeStateData :: Int -> InitializeStateData -> Int
wireMaxSizeInitializeStateData _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (initializeStateDataPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for InitializeStateData.
wirePokeInitializeStateData :: Int -> Ptr Word8 -> InitializeStateData -> IO (Ptr Word8)
wirePokeInitializeStateData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (initializeStateDataTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionData version p x) p1 (initializeStateDataPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for InitializeStateData.
wirePeekInitializeStateData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (InitializeStateData, Ptr Word8)
wirePeekInitializeStateData version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (InitializeStateData { initializeStateDataTopicId = f0_topicid, initializeStateDataPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a InitializeShareGroupStateRequest.
wireMaxSizeInitializeShareGroupStateRequest :: Int -> InitializeShareGroupStateRequest -> Int
wireMaxSizeInitializeShareGroupStateRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (initializeShareGroupStateRequestGroupId msg))
  + (5 + (case P.unKafkaArray (initializeShareGroupStateRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeInitializeStateData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for InitializeShareGroupStateRequest.
wirePokeInitializeShareGroupStateRequest :: Int -> Ptr Word8 -> InitializeShareGroupStateRequest -> IO (Ptr Word8)
wirePokeInitializeShareGroupStateRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (initializeShareGroupStateRequestGroupId msg))
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeInitializeStateData version p x) p1 (initializeShareGroupStateRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke InitializeShareGroupStateRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for InitializeShareGroupStateRequest.
wirePeekInitializeShareGroupStateRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (InitializeShareGroupStateRequest, Ptr Word8)
wirePeekInitializeShareGroupStateRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekInitializeStateData version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (InitializeShareGroupStateRequest { initializeShareGroupStateRequestGroupId = f0_groupid, initializeShareGroupStateRequestTopics = f1_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek InitializeShareGroupStateRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec InitializeShareGroupStateRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeInitializeShareGroupStateRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeInitializeShareGroupStateRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekInitializeShareGroupStateRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}