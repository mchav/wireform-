{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.WriteShareGroupStateRequest
Description : Kafka WriteShareGroupStateRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 85.



Valid versions: 0
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
import Data.Bytes.Get (MonadGet)
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
import qualified Kafka.Protocol.Wire.Codec as WC


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
maxWriteShareGroupStateRequestVersion = 0

-- | Encode WriteShareGroupStateRequest with the given API version.
encodeWriteShareGroupStateRequest :: MonadPut m => E.ApiVersion -> WriteShareGroupStateRequest -> m ()
encodeWriteShareGroupStateRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (writeShareGroupStateRequestGroupId msg))
      E.encodeVersionedArray version 0 encodeWriteStateData (case P.unKafkaArray (writeShareGroupStateRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode WriteShareGroupStateRequest with the given API version.
decodeWriteShareGroupStateRequest :: MonadGet m => E.ApiVersion -> m WriteShareGroupStateRequest
decodeWriteShareGroupStateRequest version
  | version == 0 =
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

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec WriteShareGroupStateRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
