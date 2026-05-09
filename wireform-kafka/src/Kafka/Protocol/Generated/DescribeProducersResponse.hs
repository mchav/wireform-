{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeProducersResponse
Description : Kafka DescribeProducersResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 61.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeProducersResponse
  (
    DescribeProducersResponse(..),
    TopicResponse(..),
    PartitionResponse(..),
    ProducerState(..),
    encodeDescribeProducersResponse,
    decodeDescribeProducersResponse,
    maxDescribeProducersResponseVersion
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


-- | The active producers for the partition.
data ProducerState = ProducerState
  {

  -- | The producer id.

  -- Versions: 0+
  producerStateProducerId :: !(Int64)
,

  -- | The producer epoch.

  -- Versions: 0+
  producerStateProducerEpoch :: !(Int32)
,

  -- | The last sequence number sent by the producer.

  -- Versions: 0+
  producerStateLastSequence :: !(Int32)
,

  -- | The last timestamp sent by the producer.

  -- Versions: 0+
  producerStateLastTimestamp :: !(Int64)
,

  -- | The current epoch of the producer group.

  -- Versions: 0+
  producerStateCoordinatorEpoch :: !(Int32)
,

  -- | The current transaction start offset of the producer.

  -- Versions: 0+
  producerStateCurrentTxnStartOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode ProducerState with version-aware field handling.
encodeProducerState :: MonadPut m => E.ApiVersion -> ProducerState -> m ()
encodeProducerState version pmsg =
  do
    serialize (producerStateProducerId pmsg)
    serialize (producerStateProducerEpoch pmsg)
    serialize (producerStateLastSequence pmsg)
    serialize (producerStateLastTimestamp pmsg)
    serialize (producerStateCoordinatorEpoch pmsg)
    serialize (producerStateCurrentTxnStartOffset pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ProducerState with version-aware field handling.
decodeProducerState :: MonadGet m => E.ApiVersion -> m ProducerState
decodeProducerState version =
  do
    fieldproducerid <- deserialize
    fieldproducerepoch <- deserialize
    fieldlastsequence <- deserialize
    fieldlasttimestamp <- deserialize
    fieldcoordinatorepoch <- deserialize
    fieldcurrenttxnstartoffset <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ProducerState
      {
      producerStateProducerId = fieldproducerid
      ,
      producerStateProducerEpoch = fieldproducerepoch
      ,
      producerStateLastSequence = fieldlastsequence
      ,
      producerStateLastTimestamp = fieldlasttimestamp
      ,
      producerStateCoordinatorEpoch = fieldcoordinatorepoch
      ,
      producerStateCurrentTxnStartOffset = fieldcurrenttxnstartoffset
      }


-- | Each partition in the response.
data PartitionResponse = PartitionResponse
  {

  -- | The partition index.

  -- Versions: 0+
  partitionResponsePartitionIndex :: !(Int32)
,

  -- | The partition error code, or 0 if there was no error.

  -- Versions: 0+
  partitionResponseErrorCode :: !(Int16)
,

  -- | The partition error message, which may be null if no additional details are available.

  -- Versions: 0+
  partitionResponseErrorMessage :: !(KafkaString)
,

  -- | The active producers for the partition.

  -- Versions: 0+
  partitionResponseActiveProducers :: !(KafkaArray (ProducerState))

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionResponse with version-aware field handling.
encodePartitionResponse :: MonadPut m => E.ApiVersion -> PartitionResponse -> m ()
encodePartitionResponse version pmsg =
  do
    serialize (partitionResponsePartitionIndex pmsg)
    serialize (partitionResponseErrorCode pmsg)
    if version >= 0 then serialize (toCompactString (partitionResponseErrorMessage pmsg)) else serialize (partitionResponseErrorMessage pmsg)
    E.encodeVersionedArray version 0 encodeProducerState (case P.unKafkaArray (partitionResponseActiveProducers pmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionResponse with version-aware field handling.
decodePartitionResponse :: MonadGet m => E.ApiVersion -> m PartitionResponse
decodePartitionResponse version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldactiveproducers <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeProducerState
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionResponse
      {
      partitionResponsePartitionIndex = fieldpartitionindex
      ,
      partitionResponseErrorCode = fielderrorcode
      ,
      partitionResponseErrorMessage = fielderrormessage
      ,
      partitionResponseActiveProducers = fieldactiveproducers
      }


-- | Each topic in the response.
data TopicResponse = TopicResponse
  {

  -- | The topic name.

  -- Versions: 0+
  topicResponseName :: !(KafkaString)
,

  -- | Each partition in the response.

  -- Versions: 0+
  topicResponsePartitions :: !(KafkaArray (PartitionResponse))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicResponse with version-aware field handling.
encodeTopicResponse :: MonadPut m => E.ApiVersion -> TopicResponse -> m ()
encodeTopicResponse version tmsg =
  do
    if version >= 0 then serialize (toCompactString (topicResponseName tmsg)) else serialize (topicResponseName tmsg)
    E.encodeVersionedArray version 0 encodePartitionResponse (case P.unKafkaArray (topicResponsePartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicResponse with version-aware field handling.
decodeTopicResponse :: MonadGet m => E.ApiVersion -> m TopicResponse
decodeTopicResponse version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionResponse
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicResponse
      {
      topicResponseName = fieldname
      ,
      topicResponsePartitions = fieldpartitions
      }



data DescribeProducersResponse = DescribeProducersResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeProducersResponseThrottleTimeMs :: !(Int32)
,

  -- | Each topic in the response.

  -- Versions: 0+
  describeProducersResponseTopics :: !(KafkaArray (TopicResponse))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeProducersResponse.
maxDescribeProducersResponseVersion :: Int16
maxDescribeProducersResponseVersion = 0

-- | Encode DescribeProducersResponse with the given API version.
encodeDescribeProducersResponse :: MonadPut m => E.ApiVersion -> DescribeProducersResponse -> m ()
encodeDescribeProducersResponse version msg
  | version == 0 =
    do
      serialize (describeProducersResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 0 encodeTopicResponse (case P.unKafkaArray (describeProducersResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeProducersResponse with the given API version.
decodeDescribeProducersResponse :: MonadGet m => E.ApiVersion -> m DescribeProducersResponse
decodeDescribeProducersResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicResponse
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeProducersResponse
        {
        describeProducersResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeProducersResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeProducersResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
