{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ProduceRequest
Description : Kafka ProduceRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 0.



Valid versions: 3-13
Flexible versions: 9+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ProduceRequest
  (
    ProduceRequest(..),
    TopicProduceData(..),
    PartitionProduceData(..),
    encodeProduceRequest,
    decodeProduceRequest,
    maxProduceRequestVersion
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
import Kafka.Protocol.Message (KafkaMessage(..))


-- | Each partition to produce to.
data PartitionProduceData = PartitionProduceData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionProduceDataIndex :: !(Int32)
,

  -- | The record data to be produced.

  -- Versions: 0+
  partitionProduceDataRecords :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionProduceData with version-aware field handling.
encodePartitionProduceData :: MonadPut m => E.ApiVersion -> PartitionProduceData -> m ()
encodePartitionProduceData version pmsg =
  do
    serialize (partitionProduceDataIndex pmsg)
    if version >= 9 then serialize (toCompactBytes (partitionProduceDataRecords pmsg)) else serialize (partitionProduceDataRecords pmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionProduceData with version-aware field handling.
decodePartitionProduceData :: MonadGet m => E.ApiVersion -> m PartitionProduceData
decodePartitionProduceData version =
  do
    fieldindex <- deserialize
    fieldrecords <- if version >= 9 then P.fromCompactBytes <$> deserialize else deserialize
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionProduceData
      {
      partitionProduceDataIndex = fieldindex
      ,
      partitionProduceDataRecords = fieldrecords
      }


-- | Each topic to produce to.
data TopicProduceData = TopicProduceData
  {

  -- | The topic name.

  -- Versions: 0-12
  topicProduceDataName :: !(KafkaString)
,

  -- | The unique topic ID

  -- Versions: 13+
  topicProduceDataTopicId :: !(KafkaUuid)
,

  -- | Each partition to produce to.

  -- Versions: 0+
  topicProduceDataPartitionData :: !(KafkaArray (PartitionProduceData))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicProduceData with version-aware field handling.
encodeTopicProduceData :: MonadPut m => E.ApiVersion -> TopicProduceData -> m ()
encodeTopicProduceData version tmsg =
  do
    when (version >= 0 && version <= 12) $
      if version >= 9 then serialize (toCompactString (topicProduceDataName tmsg)) else serialize (topicProduceDataName tmsg)
    when (version >= 13) $
      serialize (topicProduceDataTopicId tmsg)
    E.encodeVersionedArray version 9 encodePartitionProduceData (case P.unKafkaArray (topicProduceDataPartitionData tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicProduceData with version-aware field handling.
decodeTopicProduceData :: MonadGet m => E.ApiVersion -> m TopicProduceData
decodeTopicProduceData version =
  do
    fieldname <- if version >= 0 && version <= 12
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 13
      then deserialize
      else pure (P.nullUuid)
    fieldpartitiondata <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodePartitionProduceData
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicProduceData
      {
      topicProduceDataName = fieldname
      ,
      topicProduceDataTopicId = fieldtopicid
      ,
      topicProduceDataPartitionData = fieldpartitiondata
      }



data ProduceRequest = ProduceRequest
  {

  -- | The transactional ID, or null if the producer is not transactional.

  -- Versions: 3+
  produceRequestTransactionalId :: !(KafkaString)
,

  -- | The number of acknowledgments the producer requires the leader to have received before considering a

  -- Versions: 0+
  produceRequestAcks :: !(Int16)
,

  -- | The timeout to await a response in milliseconds.

  -- Versions: 0+
  produceRequestTimeoutMs :: !(Int32)
,

  -- | Each topic to produce to.

  -- Versions: 0+
  produceRequestTopicData :: !(KafkaArray (TopicProduceData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ProduceRequest.
maxProduceRequestVersion :: Int16
maxProduceRequestVersion = 13

-- | KafkaMessage instance for ProduceRequest.
instance KafkaMessage ProduceRequest where
  messageApiKey = 0
  messageMinVersion = 3
  messageMaxVersion = 13
  messageFlexibleVersion = Just 9

-- | Encode ProduceRequest with the given API version.
encodeProduceRequest :: MonadPut m => E.ApiVersion -> ProduceRequest -> m ()
encodeProduceRequest version msg
  | version >= 9 && version <= 13 =
    do
      serialize (toCompactString (produceRequestTransactionalId msg))
      serialize (produceRequestAcks msg)
      serialize (produceRequestTimeoutMs msg)
      E.encodeVersionedArray version 9 encodeTopicProduceData (case P.unKafkaArray (produceRequestTopicData msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 3 && version <= 8 =
    do
      serialize (produceRequestTransactionalId msg)
      serialize (produceRequestAcks msg)
      serialize (produceRequestTimeoutMs msg)
      E.encodeVersionedArray version 9 encodeTopicProduceData (case P.unKafkaArray (produceRequestTopicData msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ProduceRequest with the given API version.
decodeProduceRequest :: MonadGet m => E.ApiVersion -> m ProduceRequest
decodeProduceRequest version
  | version >= 9 && version <= 13 =
    do
      fieldtransactionalid <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      fieldacks <- deserialize
      fieldtimeoutms <- deserialize
      fieldtopicdata <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeTopicProduceData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ProduceRequest
        {
        produceRequestTransactionalId = fieldtransactionalid
        ,
        produceRequestAcks = fieldacks
        ,
        produceRequestTimeoutMs = fieldtimeoutms
        ,
        produceRequestTopicData = fieldtopicdata
        }

  | version >= 3 && version <= 8 =
    do
      fieldtransactionalid <- deserialize
      fieldacks <- deserialize
      fieldtimeoutms <- deserialize
      fieldtopicdata <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeTopicProduceData
      pure ProduceRequest
        {
        produceRequestTransactionalId = fieldtransactionalid
        ,
        produceRequestAcks = fieldacks
        ,
        produceRequestTimeoutMs = fieldtimeoutms
        ,
        produceRequestTopicData = fieldtopicdata
        }
  | otherwise = fail $ "Unsupported version: " ++ show version