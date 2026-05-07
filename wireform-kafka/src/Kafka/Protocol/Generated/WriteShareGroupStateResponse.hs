{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.WriteShareGroupStateResponse
Description : Kafka WriteShareGroupStateResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 85.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.WriteShareGroupStateResponse
  (
    WriteShareGroupStateResponse(..),
    WriteStateResult(..),
    PartitionResult(..),
    encodeWriteShareGroupStateResponse,
    decodeWriteShareGroupStateResponse,
    maxWriteShareGroupStateResponseVersion
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

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionResult with version-aware field handling.
encodePartitionResult :: MonadPut m => E.ApiVersion -> PartitionResult -> m ()
encodePartitionResult version pmsg =
  do
    serialize (partitionResultPartition pmsg)
    serialize (partitionResultErrorCode pmsg)
    if version >= 0 then serialize (toCompactString (partitionResultErrorMessage pmsg)) else serialize (partitionResultErrorMessage pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionResult with version-aware field handling.
decodePartitionResult :: MonadGet m => E.ApiVersion -> m PartitionResult
decodePartitionResult version =
  do
    fieldpartition <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionResult
      {
      partitionResultPartition = fieldpartition
      ,
      partitionResultErrorCode = fielderrorcode
      ,
      partitionResultErrorMessage = fielderrormessage
      }


-- | The write results.
data WriteStateResult = WriteStateResult
  {

  -- | The topic identifier.

  -- Versions: 0+
  writeStateResultTopicId :: !(KafkaUuid)
,

  -- | The results for the partitions.

  -- Versions: 0+
  writeStateResultPartitions :: !(KafkaArray (PartitionResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode WriteStateResult with version-aware field handling.
encodeWriteStateResult :: MonadPut m => E.ApiVersion -> WriteStateResult -> m ()
encodeWriteStateResult version wmsg =
  do
    serialize (writeStateResultTopicId wmsg)
    E.encodeVersionedArray version 0 encodePartitionResult (case P.unKafkaArray (writeStateResultPartitions wmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode WriteStateResult with version-aware field handling.
decodeWriteStateResult :: MonadGet m => E.ApiVersion -> m WriteStateResult
decodeWriteStateResult version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionResult
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure WriteStateResult
      {
      writeStateResultTopicId = fieldtopicid
      ,
      writeStateResultPartitions = fieldpartitions
      }



data WriteShareGroupStateResponse = WriteShareGroupStateResponse
  {

  -- | The write results.

  -- Versions: 0+
  writeShareGroupStateResponseResults :: !(KafkaArray (WriteStateResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for WriteShareGroupStateResponse.
maxWriteShareGroupStateResponseVersion :: Int16
maxWriteShareGroupStateResponseVersion = 0

-- | Encode WriteShareGroupStateResponse with the given API version.
encodeWriteShareGroupStateResponse :: MonadPut m => E.ApiVersion -> WriteShareGroupStateResponse -> m ()
encodeWriteShareGroupStateResponse version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 0 encodeWriteStateResult (case P.unKafkaArray (writeShareGroupStateResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode WriteShareGroupStateResponse with the given API version.
decodeWriteShareGroupStateResponse :: MonadGet m => E.ApiVersion -> m WriteShareGroupStateResponse
decodeWriteShareGroupStateResponse version
  | version == 0 =
    do
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeWriteStateResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure WriteShareGroupStateResponse
        {
        writeShareGroupStateResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version