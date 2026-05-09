{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.WriteTxnMarkersResponse
Description : Kafka WriteTxnMarkersResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 27.



Valid versions: 1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.WriteTxnMarkersResponse
  (
    WriteTxnMarkersResponse(..),
    WritableTxnMarkerResult(..),
    WritableTxnMarkerTopicResult(..),
    WritableTxnMarkerPartitionResult(..),
    encodeWriteTxnMarkersResponse,
    decodeWriteTxnMarkersResponse,
    maxWriteTxnMarkersResponseVersion
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


-- | The results by partition.
data WritableTxnMarkerPartitionResult = WritableTxnMarkerPartitionResult
  {

  -- | The partition index.

  -- Versions: 0+
  writableTxnMarkerPartitionResultPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  writableTxnMarkerPartitionResultErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode WritableTxnMarkerPartitionResult with version-aware field handling.
encodeWritableTxnMarkerPartitionResult :: MonadPut m => E.ApiVersion -> WritableTxnMarkerPartitionResult -> m ()
encodeWritableTxnMarkerPartitionResult version wmsg =
  do
    serialize (writableTxnMarkerPartitionResultPartitionIndex wmsg)
    serialize (writableTxnMarkerPartitionResultErrorCode wmsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode WritableTxnMarkerPartitionResult with version-aware field handling.
decodeWritableTxnMarkerPartitionResult :: MonadGet m => E.ApiVersion -> m WritableTxnMarkerPartitionResult
decodeWritableTxnMarkerPartitionResult version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure WritableTxnMarkerPartitionResult
      {
      writableTxnMarkerPartitionResultPartitionIndex = fieldpartitionindex
      ,
      writableTxnMarkerPartitionResultErrorCode = fielderrorcode
      }


-- | The results by topic.
data WritableTxnMarkerTopicResult = WritableTxnMarkerTopicResult
  {

  -- | The topic name.

  -- Versions: 0+
  writableTxnMarkerTopicResultName :: !(KafkaString)
,

  -- | The results by partition.

  -- Versions: 0+
  writableTxnMarkerTopicResultPartitions :: !(KafkaArray (WritableTxnMarkerPartitionResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode WritableTxnMarkerTopicResult with version-aware field handling.
encodeWritableTxnMarkerTopicResult :: MonadPut m => E.ApiVersion -> WritableTxnMarkerTopicResult -> m ()
encodeWritableTxnMarkerTopicResult version wmsg =
  do
    if version >= 1 then serialize (toCompactString (writableTxnMarkerTopicResultName wmsg)) else serialize (writableTxnMarkerTopicResultName wmsg)
    E.encodeVersionedArray version 1 encodeWritableTxnMarkerPartitionResult (case P.unKafkaArray (writableTxnMarkerTopicResultPartitions wmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode WritableTxnMarkerTopicResult with version-aware field handling.
decodeWritableTxnMarkerTopicResult :: MonadGet m => E.ApiVersion -> m WritableTxnMarkerTopicResult
decodeWritableTxnMarkerTopicResult version =
  do
    fieldname <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeWritableTxnMarkerPartitionResult
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure WritableTxnMarkerTopicResult
      {
      writableTxnMarkerTopicResultName = fieldname
      ,
      writableTxnMarkerTopicResultPartitions = fieldpartitions
      }


-- | The results for writing makers.
data WritableTxnMarkerResult = WritableTxnMarkerResult
  {

  -- | The current producer ID in use by the transactional ID.

  -- Versions: 0+
  writableTxnMarkerResultProducerId :: !(Int64)
,

  -- | The results by topic.

  -- Versions: 0+
  writableTxnMarkerResultTopics :: !(KafkaArray (WritableTxnMarkerTopicResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode WritableTxnMarkerResult with version-aware field handling.
encodeWritableTxnMarkerResult :: MonadPut m => E.ApiVersion -> WritableTxnMarkerResult -> m ()
encodeWritableTxnMarkerResult version wmsg =
  do
    serialize (writableTxnMarkerResultProducerId wmsg)
    E.encodeVersionedArray version 1 encodeWritableTxnMarkerTopicResult (case P.unKafkaArray (writableTxnMarkerResultTopics wmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode WritableTxnMarkerResult with version-aware field handling.
decodeWritableTxnMarkerResult :: MonadGet m => E.ApiVersion -> m WritableTxnMarkerResult
decodeWritableTxnMarkerResult version =
  do
    fieldproducerid <- deserialize
    fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeWritableTxnMarkerTopicResult
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure WritableTxnMarkerResult
      {
      writableTxnMarkerResultProducerId = fieldproducerid
      ,
      writableTxnMarkerResultTopics = fieldtopics
      }



data WriteTxnMarkersResponse = WriteTxnMarkersResponse
  {

  -- | The results for writing makers.

  -- Versions: 0+
  writeTxnMarkersResponseMarkers :: !(KafkaArray (WritableTxnMarkerResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for WriteTxnMarkersResponse.
maxWriteTxnMarkersResponseVersion :: Int16
maxWriteTxnMarkersResponseVersion = 1

-- | Encode WriteTxnMarkersResponse with the given API version.
encodeWriteTxnMarkersResponse :: MonadPut m => E.ApiVersion -> WriteTxnMarkersResponse -> m ()
encodeWriteTxnMarkersResponse version msg
  | version == 1 =
    do
      E.encodeVersionedArray version 1 encodeWritableTxnMarkerResult (case P.unKafkaArray (writeTxnMarkersResponseMarkers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode WriteTxnMarkersResponse with the given API version.
decodeWriteTxnMarkersResponse :: MonadGet m => E.ApiVersion -> m WriteTxnMarkersResponse
decodeWriteTxnMarkersResponse version
  | version == 1 =
    do
      fieldmarkers <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeWritableTxnMarkerResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure WriteTxnMarkersResponse
        {
        writeTxnMarkersResponseMarkers = fieldmarkers
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec WriteTxnMarkersResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
