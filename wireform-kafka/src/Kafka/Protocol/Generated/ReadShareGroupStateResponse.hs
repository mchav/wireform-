{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ReadShareGroupStateResponse
Description : Kafka ReadShareGroupStateResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 84.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ReadShareGroupStateResponse
  (
    ReadShareGroupStateResponse(..),
    ReadStateResult(..),
    PartitionResult(..),
    StateBatch(..),
    encodeReadShareGroupStateResponse,
    decodeReadShareGroupStateResponse,
    maxReadShareGroupStateResponseVersion
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


-- | The state batches for this share-partition.
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


-- | The results for the partitions.
data PartitionResult = PartitionResult
  {

  -- | The partition index.

  -- Versions: 0+
  partitionResultPartition :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  partitionResultErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 0+
  partitionResultErrorMessage :: !(KafkaString)
,

  -- | The state epoch of the share-partition.

  -- Versions: 0+
  partitionResultStateEpoch :: !(Int32)
,

  -- | The share-partition start offset, which can be -1 if it is not yet initialized.

  -- Versions: 0+
  partitionResultStartOffset :: !(Int64)
,

  -- | The state batches for this share-partition.

  -- Versions: 0+
  partitionResultStateBatches :: !(KafkaArray (StateBatch))

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionResult with version-aware field handling.
encodePartitionResult :: MonadPut m => E.ApiVersion -> PartitionResult -> m ()
encodePartitionResult version pmsg =
  do
    serialize (partitionResultPartition pmsg)
    serialize (partitionResultErrorCode pmsg)
    if version >= 0 then serialize (toCompactString (partitionResultErrorMessage pmsg)) else serialize (partitionResultErrorMessage pmsg)
    serialize (partitionResultStateEpoch pmsg)
    serialize (partitionResultStartOffset pmsg)
    E.encodeVersionedArray version 0 encodeStateBatch (case P.unKafkaArray (partitionResultStateBatches pmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionResult with version-aware field handling.
decodePartitionResult :: MonadGet m => E.ApiVersion -> m PartitionResult
decodePartitionResult version =
  do
    fieldpartition <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldstateepoch <- deserialize
    fieldstartoffset <- deserialize
    fieldstatebatches <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeStateBatch
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionResult
      {
      partitionResultPartition = fieldpartition
      ,
      partitionResultErrorCode = fielderrorcode
      ,
      partitionResultErrorMessage = fielderrormessage
      ,
      partitionResultStateEpoch = fieldstateepoch
      ,
      partitionResultStartOffset = fieldstartoffset
      ,
      partitionResultStateBatches = fieldstatebatches
      }


-- | The read results.
data ReadStateResult = ReadStateResult
  {

  -- | The topic identifier.

  -- Versions: 0+
  readStateResultTopicId :: !(KafkaUuid)
,

  -- | The results for the partitions.

  -- Versions: 0+
  readStateResultPartitions :: !(KafkaArray (PartitionResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode ReadStateResult with version-aware field handling.
encodeReadStateResult :: MonadPut m => E.ApiVersion -> ReadStateResult -> m ()
encodeReadStateResult version rmsg =
  do
    serialize (readStateResultTopicId rmsg)
    E.encodeVersionedArray version 0 encodePartitionResult (case P.unKafkaArray (readStateResultPartitions rmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReadStateResult with version-aware field handling.
decodeReadStateResult :: MonadGet m => E.ApiVersion -> m ReadStateResult
decodeReadStateResult version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionResult
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReadStateResult
      {
      readStateResultTopicId = fieldtopicid
      ,
      readStateResultPartitions = fieldpartitions
      }



data ReadShareGroupStateResponse = ReadShareGroupStateResponse
  {

  -- | The read results.

  -- Versions: 0+
  readShareGroupStateResponseResults :: !(KafkaArray (ReadStateResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ReadShareGroupStateResponse.
maxReadShareGroupStateResponseVersion :: Int16
maxReadShareGroupStateResponseVersion = 0

-- | KafkaMessage instance for ReadShareGroupStateResponse.
instance KafkaMessage ReadShareGroupStateResponse where
  messageApiKey = 84
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode ReadShareGroupStateResponse with the given API version.
encodeReadShareGroupStateResponse :: MonadPut m => E.ApiVersion -> ReadShareGroupStateResponse -> m ()
encodeReadShareGroupStateResponse version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 0 encodeReadStateResult (case P.unKafkaArray (readShareGroupStateResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ReadShareGroupStateResponse with the given API version.
decodeReadShareGroupStateResponse :: MonadGet m => E.ApiVersion -> m ReadShareGroupStateResponse
decodeReadShareGroupStateResponse version
  | version == 0 =
    do
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReadStateResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ReadShareGroupStateResponse
        {
        readShareGroupStateResponseResults = fieldresults
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

-- | Worst-case wire size of a PartitionResult.
wireMaxSizePartitionResult :: Int -> PartitionResult -> Int
wireMaxSizePartitionResult _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (partitionResultErrorMessage msg))
  + 4
  + 8
  + (5 + (case P.unKafkaArray (partitionResultStateBatches msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeStateBatch _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for PartitionResult.
wirePokePartitionResult :: Int -> Ptr Word8 -> PartitionResult -> IO (Ptr Word8)
wirePokePartitionResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionResultPartition msg)
  p2 <- W.pokeInt16BE p1 (partitionResultErrorCode msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (partitionResultErrorMessage msg))
  p4 <- W.pokeInt32BE p3 (partitionResultStateEpoch msg)
  p5 <- W.pokeInt64BE p4 (partitionResultStartOffset msg)
  p6 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeStateBatch version p x) p5 (partitionResultStateBatches msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p6 else pure p6

-- | Direct-poke decoder for PartitionResult.
wirePeekPartitionResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionResult, Ptr Word8)
wirePeekPartitionResult version _fp _basePtr p0 endPtr = do
  (f0_partition, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_stateepoch, p4) <- W.peekInt32BE p3 endPtr
  (f4_startoffset, p5) <- W.peekInt64BE p4 endPtr
  (f5_statebatches, p6) <- WP.peekVersionedArray version 0 (\p e -> wirePeekStateBatch version _fp _basePtr p e) p5 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p6 endPtr else pure p6
  pure (PartitionResult { partitionResultPartition = f0_partition, partitionResultErrorCode = f1_errorcode, partitionResultErrorMessage = f2_errormessage, partitionResultStateEpoch = f3_stateepoch, partitionResultStartOffset = f4_startoffset, partitionResultStateBatches = f5_statebatches }, pTagsEnd)

-- | Worst-case wire size of a ReadStateResult.
wireMaxSizeReadStateResult :: Int -> ReadStateResult -> Int
wireMaxSizeReadStateResult _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (readStateResultPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReadStateResult.
wirePokeReadStateResult :: Int -> Ptr Word8 -> ReadStateResult -> IO (Ptr Word8)
wirePokeReadStateResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (readStateResultTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionResult version p x) p1 (readStateResultPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReadStateResult.
wirePeekReadStateResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReadStateResult, Ptr Word8)
wirePeekReadStateResult version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionResult version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReadStateResult { readStateResultTopicId = f0_topicid, readStateResultPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a ReadShareGroupStateResponse.
wireMaxSizeReadShareGroupStateResponse :: Int -> ReadShareGroupStateResponse -> Int
wireMaxSizeReadShareGroupStateResponse _version msg =
  0
  + (5 + (case P.unKafkaArray (readShareGroupStateResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeReadStateResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ReadShareGroupStateResponse.
wirePokeReadShareGroupStateResponse :: Int -> Ptr Word8 -> ReadShareGroupStateResponse -> IO (Ptr Word8)
wirePokeReadShareGroupStateResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeReadStateResult version p x) p0 (readShareGroupStateResponseResults msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke ReadShareGroupStateResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ReadShareGroupStateResponse.
wirePeekReadShareGroupStateResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReadShareGroupStateResponse, Ptr Word8)
wirePeekReadShareGroupStateResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_results, p1) <- WP.peekVersionedArray version 0 (\p e -> wirePeekReadStateResult version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (ReadShareGroupStateResponse { readShareGroupStateResponseResults = f0_results }, pTagsEnd)
  | otherwise = error $ "wirePeek ReadShareGroupStateResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec ReadShareGroupStateResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeReadShareGroupStateResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeReadShareGroupStateResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekReadShareGroupStateResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}