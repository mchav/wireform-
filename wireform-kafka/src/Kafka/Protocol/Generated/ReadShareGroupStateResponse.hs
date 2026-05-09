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

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ReadShareGroupStateResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
