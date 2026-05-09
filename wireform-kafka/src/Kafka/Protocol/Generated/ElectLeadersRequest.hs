{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ElectLeadersRequest
Description : Kafka ElectLeadersRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 43.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ElectLeadersRequest
  (
    ElectLeadersRequest(..),
    TopicPartitions(..),
    encodeElectLeadersRequest,
    decodeElectLeadersRequest,
    maxElectLeadersRequestVersion
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


-- | The topic partitions to elect leaders.
data TopicPartitions = TopicPartitions
  {

  -- | The name of a topic.

  -- Versions: 0+
  topicPartitionsTopic :: !(KafkaString)
,

  -- | The partitions of this topic whose leader should be elected.

  -- Versions: 0+
  topicPartitionsPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicPartitions with version-aware field handling.
encodeTopicPartitions :: MonadPut m => E.ApiVersion -> TopicPartitions -> m ()
encodeTopicPartitions version tmsg =
  do
    if version >= 2 then serialize (toCompactString (topicPartitionsTopic tmsg)) else serialize (topicPartitionsTopic tmsg)
    E.encodeVersionedArray version 2 (\_ x -> serialize x) (case P.unKafkaArray (topicPartitionsPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicPartitions with version-aware field handling.
decodeTopicPartitions :: MonadGet m => E.ApiVersion -> m TopicPartitions
decodeTopicPartitions version =
  do
    fieldtopic <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 (\_ -> deserialize)
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicPartitions
      {
      topicPartitionsTopic = fieldtopic
      ,
      topicPartitionsPartitions = fieldpartitions
      }



data ElectLeadersRequest = ElectLeadersRequest
  {

  -- | Type of elections to conduct for the partition. A value of '0' elects the preferred replica. A value

  -- Versions: 1+
  electLeadersRequestElectionType :: !(Int8)
,

  -- | The topic partitions to elect leaders.

  -- Versions: 0+
  electLeadersRequestTopicPartitions :: !(KafkaArray (TopicPartitions))
,

  -- | The time in ms to wait for the election to complete.

  -- Versions: 0+
  electLeadersRequestTimeoutMs :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ElectLeadersRequest.
maxElectLeadersRequestVersion :: Int16
maxElectLeadersRequestVersion = 2

-- | Encode ElectLeadersRequest with the given API version.
encodeElectLeadersRequest :: MonadPut m => E.ApiVersion -> ElectLeadersRequest -> m ()
encodeElectLeadersRequest version msg
  | version == 0 =
    do
      E.encodeVersionedNullableArray version 2 encodeTopicPartitions (electLeadersRequestTopicPartitions msg)
      serialize (electLeadersRequestTimeoutMs msg)


  | version == 1 =
    do
      serialize (electLeadersRequestElectionType msg)
      E.encodeVersionedNullableArray version 2 encodeTopicPartitions (electLeadersRequestTopicPartitions msg)
      serialize (electLeadersRequestTimeoutMs msg)


  | version == 2 =
    do
      serialize (electLeadersRequestElectionType msg)
      E.encodeVersionedNullableArray version 2 encodeTopicPartitions (electLeadersRequestTopicPartitions msg)
      serialize (electLeadersRequestTimeoutMs msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ElectLeadersRequest with the given API version.
decodeElectLeadersRequest :: MonadGet m => E.ApiVersion -> m ElectLeadersRequest
decodeElectLeadersRequest version
  | version == 0 =
    do
      fieldtopicpartitions <- E.decodeVersionedNullableArray version 2 decodeTopicPartitions
      fieldtimeoutms <- deserialize
      pure ElectLeadersRequest
        {
        electLeadersRequestElectionType = 0
        ,
        electLeadersRequestTopicPartitions = fieldtopicpartitions
        ,
        electLeadersRequestTimeoutMs = fieldtimeoutms
        }

  | version == 1 =
    do
      fieldelectiontype <- deserialize
      fieldtopicpartitions <- E.decodeVersionedNullableArray version 2 decodeTopicPartitions
      fieldtimeoutms <- deserialize
      pure ElectLeadersRequest
        {
        electLeadersRequestElectionType = fieldelectiontype
        ,
        electLeadersRequestTopicPartitions = fieldtopicpartitions
        ,
        electLeadersRequestTimeoutMs = fieldtimeoutms
        }

  | version == 2 =
    do
      fieldelectiontype <- deserialize
      fieldtopicpartitions <- E.decodeVersionedNullableArray version 2 decodeTopicPartitions
      fieldtimeoutms <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ElectLeadersRequest
        {
        electLeadersRequestElectionType = fieldelectiontype
        ,
        electLeadersRequestTopicPartitions = fieldtopicpartitions
        ,
        electLeadersRequestTimeoutMs = fieldtimeoutms
        }
  | otherwise = fail $ "Unsupported version: " ++ show version