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

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeInitializeShareGroupStateRequest' / 'decodeInitializeShareGroupStateRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec InitializeShareGroupStateRequest where
  wireCodec = Just (WC.serialShimCodec encodeInitializeShareGroupStateRequest decodeInitializeShareGroupStateRequest)
  {-# INLINE wireCodec #-}
