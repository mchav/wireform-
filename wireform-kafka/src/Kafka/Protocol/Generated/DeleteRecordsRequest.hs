{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteRecordsRequest
Description : Kafka DeleteRecordsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 21.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteRecordsRequest
  (
    DeleteRecordsRequest(..),
    DeleteRecordsTopic(..),
    DeleteRecordsPartition(..),
    encodeDeleteRecordsRequest,
    decodeDeleteRecordsRequest,
    maxDeleteRecordsRequestVersion
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


-- | Each partition that we want to delete records from.
data DeleteRecordsPartition = DeleteRecordsPartition
  {

  -- | The partition index.

  -- Versions: 0+
  deleteRecordsPartitionPartitionIndex :: !(Int32)
,

  -- | The deletion offset. -1 means that records should be truncated to the high watermark.

  -- Versions: 0+
  deleteRecordsPartitionOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteRecordsPartition with version-aware field handling.
encodeDeleteRecordsPartition :: MonadPut m => E.ApiVersion -> DeleteRecordsPartition -> m ()
encodeDeleteRecordsPartition version dmsg =
  do
    serialize (deleteRecordsPartitionPartitionIndex dmsg)
    serialize (deleteRecordsPartitionOffset dmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteRecordsPartition with version-aware field handling.
decodeDeleteRecordsPartition :: MonadGet m => E.ApiVersion -> m DeleteRecordsPartition
decodeDeleteRecordsPartition version =
  do
    fieldpartitionindex <- deserialize
    fieldoffset <- deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteRecordsPartition
      {
      deleteRecordsPartitionPartitionIndex = fieldpartitionindex
      ,
      deleteRecordsPartitionOffset = fieldoffset
      }


-- | Each topic that we want to delete records from.
data DeleteRecordsTopic = DeleteRecordsTopic
  {

  -- | The topic name.

  -- Versions: 0+
  deleteRecordsTopicName :: !(KafkaString)
,

  -- | Each partition that we want to delete records from.

  -- Versions: 0+
  deleteRecordsTopicPartitions :: !(KafkaArray (DeleteRecordsPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteRecordsTopic with version-aware field handling.
encodeDeleteRecordsTopic :: MonadPut m => E.ApiVersion -> DeleteRecordsTopic -> m ()
encodeDeleteRecordsTopic version dmsg =
  do
    if version >= 2 then serialize (toCompactString (deleteRecordsTopicName dmsg)) else serialize (deleteRecordsTopicName dmsg)
    E.encodeVersionedArray version 2 encodeDeleteRecordsPartition (case P.unKafkaArray (deleteRecordsTopicPartitions dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteRecordsTopic with version-aware field handling.
decodeDeleteRecordsTopic :: MonadGet m => E.ApiVersion -> m DeleteRecordsTopic
decodeDeleteRecordsTopic version =
  do
    fieldname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteRecordsPartition
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteRecordsTopic
      {
      deleteRecordsTopicName = fieldname
      ,
      deleteRecordsTopicPartitions = fieldpartitions
      }



data DeleteRecordsRequest = DeleteRecordsRequest
  {

  -- | Each topic that we want to delete records from.

  -- Versions: 0+
  deleteRecordsRequestTopics :: !(KafkaArray (DeleteRecordsTopic))
,

  -- | How long to wait for the deletion to complete, in milliseconds.

  -- Versions: 0+
  deleteRecordsRequestTimeoutMs :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteRecordsRequest.
maxDeleteRecordsRequestVersion :: Int16
maxDeleteRecordsRequestVersion = 2

-- | Encode DeleteRecordsRequest with the given API version.
encodeDeleteRecordsRequest :: MonadPut m => E.ApiVersion -> DeleteRecordsRequest -> m ()
encodeDeleteRecordsRequest version msg
  | version == 2 =
    do
      E.encodeVersionedArray version 2 encodeDeleteRecordsTopic (case P.unKafkaArray (deleteRecordsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (deleteRecordsRequestTimeoutMs msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      E.encodeVersionedArray version 2 encodeDeleteRecordsTopic (case P.unKafkaArray (deleteRecordsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (deleteRecordsRequestTimeoutMs msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteRecordsRequest with the given API version.
decodeDeleteRecordsRequest :: MonadGet m => E.ApiVersion -> m DeleteRecordsRequest
decodeDeleteRecordsRequest version
  | version == 2 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteRecordsTopic
      fieldtimeoutms <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteRecordsRequest
        {
        deleteRecordsRequestTopics = fieldtopics
        ,
        deleteRecordsRequestTimeoutMs = fieldtimeoutms
        }

  | version >= 0 && version <= 1 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteRecordsTopic
      fieldtimeoutms <- deserialize
      pure DeleteRecordsRequest
        {
        deleteRecordsRequestTopics = fieldtopics
        ,
        deleteRecordsRequestTimeoutMs = fieldtimeoutms
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DeleteRecordsRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
