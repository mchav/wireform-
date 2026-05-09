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

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DeleteShareGroupStateRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
