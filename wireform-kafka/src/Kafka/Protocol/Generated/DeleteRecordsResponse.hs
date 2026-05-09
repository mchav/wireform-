{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteRecordsResponse
Description : Kafka DeleteRecordsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 21.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteRecordsResponse
  (
    DeleteRecordsResponse(..),
    DeleteRecordsTopicResult(..),
    DeleteRecordsPartitionResult(..),
    encodeDeleteRecordsResponse,
    decodeDeleteRecordsResponse,
    maxDeleteRecordsResponseVersion
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


-- | Each partition that we wanted to delete records from.
data DeleteRecordsPartitionResult = DeleteRecordsPartitionResult
  {

  -- | The partition index.

  -- Versions: 0+
  deleteRecordsPartitionResultPartitionIndex :: !(Int32)
,

  -- | The partition low water mark.

  -- Versions: 0+
  deleteRecordsPartitionResultLowWatermark :: !(Int64)
,

  -- | The deletion error code, or 0 if the deletion succeeded.

  -- Versions: 0+
  deleteRecordsPartitionResultErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteRecordsPartitionResult with version-aware field handling.
encodeDeleteRecordsPartitionResult :: MonadPut m => E.ApiVersion -> DeleteRecordsPartitionResult -> m ()
encodeDeleteRecordsPartitionResult version dmsg =
  do
    serialize (deleteRecordsPartitionResultPartitionIndex dmsg)
    serialize (deleteRecordsPartitionResultLowWatermark dmsg)
    serialize (deleteRecordsPartitionResultErrorCode dmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteRecordsPartitionResult with version-aware field handling.
decodeDeleteRecordsPartitionResult :: MonadGet m => E.ApiVersion -> m DeleteRecordsPartitionResult
decodeDeleteRecordsPartitionResult version =
  do
    fieldpartitionindex <- deserialize
    fieldlowwatermark <- deserialize
    fielderrorcode <- deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteRecordsPartitionResult
      {
      deleteRecordsPartitionResultPartitionIndex = fieldpartitionindex
      ,
      deleteRecordsPartitionResultLowWatermark = fieldlowwatermark
      ,
      deleteRecordsPartitionResultErrorCode = fielderrorcode
      }


-- | Each topic that we wanted to delete records from.
data DeleteRecordsTopicResult = DeleteRecordsTopicResult
  {

  -- | The topic name.

  -- Versions: 0+
  deleteRecordsTopicResultName :: !(KafkaString)
,

  -- | Each partition that we wanted to delete records from.

  -- Versions: 0+
  deleteRecordsTopicResultPartitions :: !(KafkaArray (DeleteRecordsPartitionResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteRecordsTopicResult with version-aware field handling.
encodeDeleteRecordsTopicResult :: MonadPut m => E.ApiVersion -> DeleteRecordsTopicResult -> m ()
encodeDeleteRecordsTopicResult version dmsg =
  do
    if version >= 2 then serialize (toCompactString (deleteRecordsTopicResultName dmsg)) else serialize (deleteRecordsTopicResultName dmsg)
    E.encodeVersionedArray version 2 encodeDeleteRecordsPartitionResult (case P.unKafkaArray (deleteRecordsTopicResultPartitions dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteRecordsTopicResult with version-aware field handling.
decodeDeleteRecordsTopicResult :: MonadGet m => E.ApiVersion -> m DeleteRecordsTopicResult
decodeDeleteRecordsTopicResult version =
  do
    fieldname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteRecordsPartitionResult
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteRecordsTopicResult
      {
      deleteRecordsTopicResultName = fieldname
      ,
      deleteRecordsTopicResultPartitions = fieldpartitions
      }



data DeleteRecordsResponse = DeleteRecordsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  deleteRecordsResponseThrottleTimeMs :: !(Int32)
,

  -- | Each topic that we wanted to delete records from.

  -- Versions: 0+
  deleteRecordsResponseTopics :: !(KafkaArray (DeleteRecordsTopicResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteRecordsResponse.
maxDeleteRecordsResponseVersion :: Int16
maxDeleteRecordsResponseVersion = 2

-- | Encode DeleteRecordsResponse with the given API version.
encodeDeleteRecordsResponse :: MonadPut m => E.ApiVersion -> DeleteRecordsResponse -> m ()
encodeDeleteRecordsResponse version msg
  | version == 2 =
    do
      serialize (deleteRecordsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeDeleteRecordsTopicResult (case P.unKafkaArray (deleteRecordsResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      serialize (deleteRecordsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeDeleteRecordsTopicResult (case P.unKafkaArray (deleteRecordsResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteRecordsResponse with the given API version.
decodeDeleteRecordsResponse :: MonadGet m => E.ApiVersion -> m DeleteRecordsResponse
decodeDeleteRecordsResponse version
  | version == 2 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteRecordsTopicResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteRecordsResponse
        {
        deleteRecordsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteRecordsResponseTopics = fieldtopics
        }

  | version >= 0 && version <= 1 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDeleteRecordsTopicResult
      pure DeleteRecordsResponse
        {
        deleteRecordsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteRecordsResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DeleteRecordsResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
