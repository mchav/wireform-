{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteShareGroupStateResponse
Description : Kafka DeleteShareGroupStateResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 86.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteShareGroupStateResponse
  (
    DeleteShareGroupStateResponse(..),
    DeleteStateResult(..),
    PartitionResult(..),
    encodeDeleteShareGroupStateResponse,
    decodeDeleteShareGroupStateResponse,
    maxDeleteShareGroupStateResponseVersion
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


-- | The delete results.
data DeleteStateResult = DeleteStateResult
  {

  -- | The topic identifier.

  -- Versions: 0+
  deleteStateResultTopicId :: !(KafkaUuid)
,

  -- | The results for the partitions.

  -- Versions: 0+
  deleteStateResultPartitions :: !(KafkaArray (PartitionResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteStateResult with version-aware field handling.
encodeDeleteStateResult :: MonadPut m => E.ApiVersion -> DeleteStateResult -> m ()
encodeDeleteStateResult version dmsg =
  do
    serialize (deleteStateResultTopicId dmsg)
    E.encodeVersionedArray version 0 encodePartitionResult (case P.unKafkaArray (deleteStateResultPartitions dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteStateResult with version-aware field handling.
decodeDeleteStateResult :: MonadGet m => E.ApiVersion -> m DeleteStateResult
decodeDeleteStateResult version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionResult
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteStateResult
      {
      deleteStateResultTopicId = fieldtopicid
      ,
      deleteStateResultPartitions = fieldpartitions
      }



data DeleteShareGroupStateResponse = DeleteShareGroupStateResponse
  {

  -- | The delete results.

  -- Versions: 0+
  deleteShareGroupStateResponseResults :: !(KafkaArray (DeleteStateResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteShareGroupStateResponse.
maxDeleteShareGroupStateResponseVersion :: Int16
maxDeleteShareGroupStateResponseVersion = 0

-- | Encode DeleteShareGroupStateResponse with the given API version.
encodeDeleteShareGroupStateResponse :: MonadPut m => E.ApiVersion -> DeleteShareGroupStateResponse -> m ()
encodeDeleteShareGroupStateResponse version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 0 encodeDeleteStateResult (case P.unKafkaArray (deleteShareGroupStateResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteShareGroupStateResponse with the given API version.
decodeDeleteShareGroupStateResponse :: MonadGet m => E.ApiVersion -> m DeleteShareGroupStateResponse
decodeDeleteShareGroupStateResponse version
  | version == 0 =
    do
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDeleteStateResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteShareGroupStateResponse
        {
        deleteShareGroupStateResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeDeleteShareGroupStateResponse' / 'decodeDeleteShareGroupStateResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec DeleteShareGroupStateResponse where
  wireCodec = Just (WC.serialShimCodec encodeDeleteShareGroupStateResponse decodeDeleteShareGroupStateResponse)
  {-# INLINE wireCodec #-}
