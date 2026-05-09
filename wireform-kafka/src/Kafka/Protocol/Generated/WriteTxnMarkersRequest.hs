{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.WriteTxnMarkersRequest
Description : Kafka WriteTxnMarkersRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 27.



Valid versions: 1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.WriteTxnMarkersRequest
  (
    WriteTxnMarkersRequest(..),
    WritableTxnMarker(..),
    WritableTxnMarkerTopic(..),
    encodeWriteTxnMarkersRequest,
    decodeWriteTxnMarkersRequest,
    maxWriteTxnMarkersRequestVersion
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


-- | Each topic that we want to write transaction marker(s) for.
data WritableTxnMarkerTopic = WritableTxnMarkerTopic
  {

  -- | The topic name.

  -- Versions: 0+
  writableTxnMarkerTopicName :: !(KafkaString)
,

  -- | The indexes of the partitions to write transaction markers for.

  -- Versions: 0+
  writableTxnMarkerTopicPartitionIndexes :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode WritableTxnMarkerTopic with version-aware field handling.
encodeWritableTxnMarkerTopic :: MonadPut m => E.ApiVersion -> WritableTxnMarkerTopic -> m ()
encodeWritableTxnMarkerTopic version wmsg =
  do
    if version >= 1 then serialize (toCompactString (writableTxnMarkerTopicName wmsg)) else serialize (writableTxnMarkerTopicName wmsg)
    E.encodeVersionedArray version 1 (\_ x -> serialize x) (case P.unKafkaArray (writableTxnMarkerTopicPartitionIndexes wmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode WritableTxnMarkerTopic with version-aware field handling.
decodeWritableTxnMarkerTopic :: MonadGet m => E.ApiVersion -> m WritableTxnMarkerTopic
decodeWritableTxnMarkerTopic version =
  do
    fieldname <- if version >= 1 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitionindexes <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 (\_ -> deserialize)
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure WritableTxnMarkerTopic
      {
      writableTxnMarkerTopicName = fieldname
      ,
      writableTxnMarkerTopicPartitionIndexes = fieldpartitionindexes
      }


-- | The transaction markers to be written.
data WritableTxnMarker = WritableTxnMarker
  {

  -- | The current producer ID.

  -- Versions: 0+
  writableTxnMarkerProducerId :: !(Int64)
,

  -- | The current epoch associated with the producer ID.

  -- Versions: 0+
  writableTxnMarkerProducerEpoch :: !(Int16)
,

  -- | The result of the transaction to write to the partitions (false = ABORT, true = COMMIT).

  -- Versions: 0+
  writableTxnMarkerTransactionResult :: !(Bool)
,

  -- | Each topic that we want to write transaction marker(s) for.

  -- Versions: 0+
  writableTxnMarkerTopics :: !(KafkaArray (WritableTxnMarkerTopic))
,

  -- | Epoch associated with the transaction state partition hosted by this transaction coordinator.

  -- Versions: 0+
  writableTxnMarkerCoordinatorEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode WritableTxnMarker with version-aware field handling.
encodeWritableTxnMarker :: MonadPut m => E.ApiVersion -> WritableTxnMarker -> m ()
encodeWritableTxnMarker version wmsg =
  do
    serialize (writableTxnMarkerProducerId wmsg)
    serialize (writableTxnMarkerProducerEpoch wmsg)
    serialize (writableTxnMarkerTransactionResult wmsg)
    E.encodeVersionedArray version 1 encodeWritableTxnMarkerTopic (case P.unKafkaArray (writableTxnMarkerTopics wmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    serialize (writableTxnMarkerCoordinatorEpoch wmsg)
    when (version >= 1) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode WritableTxnMarker with version-aware field handling.
decodeWritableTxnMarker :: MonadGet m => E.ApiVersion -> m WritableTxnMarker
decodeWritableTxnMarker version =
  do
    fieldproducerid <- deserialize
    fieldproducerepoch <- deserialize
    fieldtransactionresult <- deserialize
    fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeWritableTxnMarkerTopic
    fieldcoordinatorepoch <- deserialize
    _ <- if version >= 1 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure WritableTxnMarker
      {
      writableTxnMarkerProducerId = fieldproducerid
      ,
      writableTxnMarkerProducerEpoch = fieldproducerepoch
      ,
      writableTxnMarkerTransactionResult = fieldtransactionresult
      ,
      writableTxnMarkerTopics = fieldtopics
      ,
      writableTxnMarkerCoordinatorEpoch = fieldcoordinatorepoch
      }



data WriteTxnMarkersRequest = WriteTxnMarkersRequest
  {

  -- | The transaction markers to be written.

  -- Versions: 0+
  writeTxnMarkersRequestMarkers :: !(KafkaArray (WritableTxnMarker))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for WriteTxnMarkersRequest.
maxWriteTxnMarkersRequestVersion :: Int16
maxWriteTxnMarkersRequestVersion = 1

-- | Encode WriteTxnMarkersRequest with the given API version.
encodeWriteTxnMarkersRequest :: MonadPut m => E.ApiVersion -> WriteTxnMarkersRequest -> m ()
encodeWriteTxnMarkersRequest version msg
  | version == 1 =
    do
      E.encodeVersionedArray version 1 encodeWritableTxnMarker (case P.unKafkaArray (writeTxnMarkersRequestMarkers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode WriteTxnMarkersRequest with the given API version.
decodeWriteTxnMarkersRequest :: MonadGet m => E.ApiVersion -> m WriteTxnMarkersRequest
decodeWriteTxnMarkersRequest version
  | version == 1 =
    do
      fieldmarkers <- P.mkKafkaArray <$> E.decodeVersionedArray version 1 decodeWritableTxnMarker
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure WriteTxnMarkersRequest
        {
        writeTxnMarkersRequestMarkers = fieldmarkers
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec WriteTxnMarkersRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
