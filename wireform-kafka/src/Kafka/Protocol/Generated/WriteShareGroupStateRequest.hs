{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.WriteShareGroupStateRequest
Description : Kafka WriteShareGroupStateRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 85.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.WriteShareGroupStateRequest
  (
    WriteShareGroupStateRequest(..),
    WriteStateData(..),
    PartitionData(..),
    StateBatch(..),
    encodeWriteShareGroupStateRequest,
    decodeWriteShareGroupStateRequest,
    maxWriteShareGroupStateRequestVersion
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


-- | The state batches for the share-partition.
data StateBatch = StateBatch
  {

  -- | The first offset of this state batch.

  -- Versions: 0+
  stateBatchFirstOffset :: !(Int64)
,

  -- | The last offset of this state batch.

  -- Versions: 0+
  stateBatchLastOffset :: !(Int64)
,

  -- | The delivery state - 0:Available,2:Acked,4:Archived.

  -- Versions: 0+
  stateBatchDeliveryState :: !(Int8)
,

  -- | The delivery count.

  -- Versions: 0+
  stateBatchDeliveryCount :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode StateBatch with version-aware field handling.
encodeStateBatch :: MonadPut m => E.ApiVersion -> StateBatch -> m ()
encodeStateBatch version smsg =
  do
    serialize (stateBatchFirstOffset smsg)
    serialize (stateBatchLastOffset smsg)
    serialize (stateBatchDeliveryState smsg)
    serialize (stateBatchDeliveryCount smsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode StateBatch with version-aware field handling.
decodeStateBatch :: MonadGet m => E.ApiVersion -> m StateBatch
decodeStateBatch version =
  do
    fieldfirstoffset <- deserialize
    fieldlastoffset <- deserialize
    fielddeliverystate <- deserialize
    fielddeliverycount <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure StateBatch
      {
      stateBatchFirstOffset = fieldfirstoffset
      ,
      stateBatchLastOffset = fieldlastoffset
      ,
      stateBatchDeliveryState = fielddeliverystate
      ,
      stateBatchDeliveryCount = fielddeliverycount
      }


-- | The data for the partitions.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartition :: !(Int32)
,

  -- | The state epoch of the share-partition.

  -- Versions: 0+
  partitionDataStateEpoch :: !(Int32)
,

  -- | The leader epoch of the share-partition.

  -- Versions: 0+
  partitionDataLeaderEpoch :: !(Int32)
,

  -- | The share-partition start offset, or -1 if the start offset is not being written.

  -- Versions: 0+
  partitionDataStartOffset :: !(Int64)
,

  -- | The number of offsets greater than or equal to share-partition start offset for which delivery has b

  -- Versions: 1+
  partitionDataDeliveryCompleteCount :: !(Int32)
,

  -- | The state batches for the share-partition.

  -- Versions: 0+
  partitionDataStateBatches :: !(KafkaArray (StateBatch))

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartition pmsg)
    serialize (partitionDataStateEpoch pmsg)
    serialize (partitionDataLeaderEpoch pmsg)
    serialize (partitionDataStartOffset pmsg)
    when (version >= 1) $
      serialize (partitionDataDeliveryCompleteCount pmsg)
    E.encodeVersionedArray version 0 encodeStateBatch (case P.unKafkaArray (partitionDataStateBatches pmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartition <- deserialize
    fieldstateepoch <- deserialize
    fieldleaderepoch <- deserialize
    fieldstartoffset <- deserialize
    fielddeliverycompletecount <- if version >= 1
      then deserialize
      else pure ((-1))
    fieldstatebatches <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeStateBatch
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartition = fieldpartition
      ,
      partitionDataStateEpoch = fieldstateepoch
      ,
      partitionDataLeaderEpoch = fieldleaderepoch
      ,
      partitionDataStartOffset = fieldstartoffset
      ,
      partitionDataDeliveryCompleteCount = fielddeliverycompletecount
      ,
      partitionDataStateBatches = fieldstatebatches
      }


-- | The data for the topics.
data WriteStateData = WriteStateData
  {

  -- | The topic identifier.

  -- Versions: 0+
  writeStateDataTopicId :: !(KafkaUuid)
,

  -- | The data for the partitions.

  -- Versions: 0+
  writeStateDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode WriteStateData with version-aware field handling.
encodeWriteStateData :: MonadPut m => E.ApiVersion -> WriteStateData -> m ()
encodeWriteStateData version wmsg =
  do
    serialize (writeStateDataTopicId wmsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (writeStateDataPartitions wmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode WriteStateData with version-aware field handling.
decodeWriteStateData :: MonadGet m => E.ApiVersion -> m WriteStateData
decodeWriteStateData version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure WriteStateData
      {
      writeStateDataTopicId = fieldtopicid
      ,
      writeStateDataPartitions = fieldpartitions
      }



data WriteShareGroupStateRequest = WriteShareGroupStateRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  writeShareGroupStateRequestGroupId :: !(KafkaString)
,

  -- | The data for the topics.

  -- Versions: 0+
  writeShareGroupStateRequestTopics :: !(KafkaArray (WriteStateData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for WriteShareGroupStateRequest.
maxWriteShareGroupStateRequestVersion :: Int16
maxWriteShareGroupStateRequestVersion = 1

-- | KafkaMessage instance for WriteShareGroupStateRequest.
instance KafkaMessage WriteShareGroupStateRequest where
  messageApiKey = 85
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Encode WriteShareGroupStateRequest with the given API version.
encodeWriteShareGroupStateRequest :: MonadPut m => E.ApiVersion -> WriteShareGroupStateRequest -> m ()
encodeWriteShareGroupStateRequest version msg
  | version >= 0 && version <= 1 =
    do
      serialize (toCompactString (writeShareGroupStateRequestGroupId msg))
      E.encodeVersionedArray version 0 encodeWriteStateData (case P.unKafkaArray (writeShareGroupStateRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode WriteShareGroupStateRequest with the given API version.
decodeWriteShareGroupStateRequest :: MonadGet m => E.ApiVersion -> m WriteShareGroupStateRequest
decodeWriteShareGroupStateRequest version
  | version >= 0 && version <= 1 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeWriteStateData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure WriteShareGroupStateRequest
        {
        writeShareGroupStateRequestGroupId = fieldgroupid
        ,
        writeShareGroupStateRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a StateBatch.
wireMaxSizeStateBatch :: Int -> StateBatch -> Int
wireMaxSizeStateBatch _version msg =
  0
  + 8
  + 8
  + 1
  + 2
  + 1

-- | Direct-poke encoder for StateBatch.
wirePokeStateBatch :: Int -> Ptr Word8 -> StateBatch -> IO (Ptr Word8)
wirePokeStateBatch version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt64BE p0 (stateBatchFirstOffset msg)
  p2 <- W.pokeInt64BE p1 (stateBatchLastOffset msg)
  p3 <- W.pokeWord8 p2 (fromIntegral (stateBatchDeliveryState msg))
  p4 <- W.pokeInt16BE p3 (stateBatchDeliveryCount msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for StateBatch.
wirePeekStateBatch :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (StateBatch, Ptr Word8)
wirePeekStateBatch version _fp _basePtr p0 endPtr = do
  (f0_firstoffset, p1) <- W.peekInt64BE p0 endPtr
  (f1_lastoffset, p2) <- W.peekInt64BE p1 endPtr
  (f2_deliverystate, p3) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p2 endPtr
  (f3_deliverycount, p4) <- W.peekInt16BE p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (StateBatch { stateBatchFirstOffset = f0_firstoffset, stateBatchLastOffset = f1_lastoffset, stateBatchDeliveryState = f2_deliverystate, stateBatchDeliveryCount = f3_deliverycount }, pTagsEnd)

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 4
  + 4
  + 8
  + 4
  + (5 + (case P.unKafkaArray (partitionDataStateBatches msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeStateBatch _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartition msg)
  p2 <- W.pokeInt32BE p1 (partitionDataStateEpoch msg)
  p3 <- W.pokeInt32BE p2 (partitionDataLeaderEpoch msg)
  p4 <- W.pokeInt64BE p3 (partitionDataStartOffset msg)
  p5 <- W.pokeInt32BE p4 (partitionDataDeliveryCompleteCount msg)
  p6 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeStateBatch version p x) p5 (partitionDataStateBatches msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p6 else pure p6

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partition, p1) <- W.peekInt32BE p0 endPtr
  (f1_stateepoch, p2) <- W.peekInt32BE p1 endPtr
  (f2_leaderepoch, p3) <- W.peekInt32BE p2 endPtr
  (f3_startoffset, p4) <- W.peekInt64BE p3 endPtr
  (f4_deliverycompletecount, p5) <- W.peekInt32BE p4 endPtr
  (f5_statebatches, p6) <- WP.peekVersionedArray version 0 (\p e -> wirePeekStateBatch version _fp _basePtr p e) p5 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p6 endPtr else pure p6
  pure (PartitionData { partitionDataPartition = f0_partition, partitionDataStateEpoch = f1_stateepoch, partitionDataLeaderEpoch = f2_leaderepoch, partitionDataStartOffset = f3_startoffset, partitionDataDeliveryCompleteCount = f4_deliverycompletecount, partitionDataStateBatches = f5_statebatches }, pTagsEnd)

-- | Worst-case wire size of a WriteStateData.
wireMaxSizeWriteStateData :: Int -> WriteStateData -> Int
wireMaxSizeWriteStateData _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (writeStateDataPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for WriteStateData.
wirePokeWriteStateData :: Int -> Ptr Word8 -> WriteStateData -> IO (Ptr Word8)
wirePokeWriteStateData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (writeStateDataTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionData version p x) p1 (writeStateDataPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for WriteStateData.
wirePeekWriteStateData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (WriteStateData, Ptr Word8)
wirePeekWriteStateData version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (WriteStateData { writeStateDataTopicId = f0_topicid, writeStateDataPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a WriteShareGroupStateRequest.
wireMaxSizeWriteShareGroupStateRequest :: Int -> WriteShareGroupStateRequest -> Int
wireMaxSizeWriteShareGroupStateRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (writeShareGroupStateRequestGroupId msg))
  + (5 + (case P.unKafkaArray (writeShareGroupStateRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeWriteStateData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for WriteShareGroupStateRequest.
wirePokeWriteShareGroupStateRequest :: Int -> Ptr Word8 -> WriteShareGroupStateRequest -> IO (Ptr Word8)
wirePokeWriteShareGroupStateRequest version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (writeShareGroupStateRequestGroupId msg))
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeWriteStateData version p x) p1 (writeShareGroupStateRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke WriteShareGroupStateRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for WriteShareGroupStateRequest.
wirePeekWriteShareGroupStateRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (WriteShareGroupStateRequest, Ptr Word8)
wirePeekWriteShareGroupStateRequest version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekWriteStateData version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (WriteShareGroupStateRequest { writeShareGroupStateRequestGroupId = f0_groupid, writeShareGroupStateRequestTopics = f1_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek WriteShareGroupStateRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec WriteShareGroupStateRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeWriteShareGroupStateRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeWriteShareGroupStateRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekWriteShareGroupStateRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}