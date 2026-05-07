{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeShareGroupOffsetsRequest
Description : Kafka DescribeShareGroupOffsetsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 90.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeShareGroupOffsetsRequest
  (
    DescribeShareGroupOffsetsRequest(..),
    DescribeShareGroupOffsetsRequestGroup(..),
    DescribeShareGroupOffsetsRequestTopic(..),
    encodeDescribeShareGroupOffsetsRequest,
    decodeDescribeShareGroupOffsetsRequest,
    maxDescribeShareGroupOffsetsRequestVersion
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


-- | The topics to describe offsets for, or null for all topic-partitions.
data DescribeShareGroupOffsetsRequestTopic = DescribeShareGroupOffsetsRequestTopic
  {

  -- | The topic name.

  -- Versions: 0+
  describeShareGroupOffsetsRequestTopicTopicName :: !(KafkaString)
,

  -- | The partitions.

  -- Versions: 0+
  describeShareGroupOffsetsRequestTopicPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeShareGroupOffsetsRequestTopic with version-aware field handling.
encodeDescribeShareGroupOffsetsRequestTopic :: MonadPut m => E.ApiVersion -> DescribeShareGroupOffsetsRequestTopic -> m ()
encodeDescribeShareGroupOffsetsRequestTopic version dmsg =
  do
    if version >= 0 then serialize (toCompactString (describeShareGroupOffsetsRequestTopicTopicName dmsg)) else serialize (describeShareGroupOffsetsRequestTopicTopicName dmsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (describeShareGroupOffsetsRequestTopicPartitions dmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeShareGroupOffsetsRequestTopic with version-aware field handling.
decodeDescribeShareGroupOffsetsRequestTopic :: MonadGet m => E.ApiVersion -> m DescribeShareGroupOffsetsRequestTopic
decodeDescribeShareGroupOffsetsRequestTopic version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeShareGroupOffsetsRequestTopic
      {
      describeShareGroupOffsetsRequestTopicTopicName = fieldtopicname
      ,
      describeShareGroupOffsetsRequestTopicPartitions = fieldpartitions
      }


-- | The groups to describe offsets for.
data DescribeShareGroupOffsetsRequestGroup = DescribeShareGroupOffsetsRequestGroup
  {

  -- | The group identifier.

  -- Versions: 0+
  describeShareGroupOffsetsRequestGroupGroupId :: !(KafkaString)
,

  -- | The topics to describe offsets for, or null for all topic-partitions.

  -- Versions: 0+
  describeShareGroupOffsetsRequestGroupTopics :: !(KafkaArray (DescribeShareGroupOffsetsRequestTopic))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeShareGroupOffsetsRequestGroup with version-aware field handling.
encodeDescribeShareGroupOffsetsRequestGroup :: MonadPut m => E.ApiVersion -> DescribeShareGroupOffsetsRequestGroup -> m ()
encodeDescribeShareGroupOffsetsRequestGroup version dmsg =
  do
    if version >= 0 then serialize (toCompactString (describeShareGroupOffsetsRequestGroupGroupId dmsg)) else serialize (describeShareGroupOffsetsRequestGroupGroupId dmsg)
    E.encodeVersionedNullableArray version 0 encodeDescribeShareGroupOffsetsRequestTopic (describeShareGroupOffsetsRequestGroupTopics dmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeShareGroupOffsetsRequestGroup with version-aware field handling.
decodeDescribeShareGroupOffsetsRequestGroup :: MonadGet m => E.ApiVersion -> m DescribeShareGroupOffsetsRequestGroup
decodeDescribeShareGroupOffsetsRequestGroup version =
  do
    fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldtopics <- E.decodeVersionedNullableArray version 0 decodeDescribeShareGroupOffsetsRequestTopic
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeShareGroupOffsetsRequestGroup
      {
      describeShareGroupOffsetsRequestGroupGroupId = fieldgroupid
      ,
      describeShareGroupOffsetsRequestGroupTopics = fieldtopics
      }



data DescribeShareGroupOffsetsRequest = DescribeShareGroupOffsetsRequest
  {

  -- | The groups to describe offsets for.

  -- Versions: 0+
  describeShareGroupOffsetsRequestGroups :: !(KafkaArray (DescribeShareGroupOffsetsRequestGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeShareGroupOffsetsRequest.
maxDescribeShareGroupOffsetsRequestVersion :: Int16
maxDescribeShareGroupOffsetsRequestVersion = 0

-- | Encode DescribeShareGroupOffsetsRequest with the given API version.
encodeDescribeShareGroupOffsetsRequest :: MonadPut m => E.ApiVersion -> DescribeShareGroupOffsetsRequest -> m ()
encodeDescribeShareGroupOffsetsRequest version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 0 encodeDescribeShareGroupOffsetsRequestGroup (case P.unKafkaArray (describeShareGroupOffsetsRequestGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeShareGroupOffsetsRequest with the given API version.
decodeDescribeShareGroupOffsetsRequest :: MonadGet m => E.ApiVersion -> m DescribeShareGroupOffsetsRequest
decodeDescribeShareGroupOffsetsRequest version
  | version == 0 =
    do
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDescribeShareGroupOffsetsRequestGroup
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeShareGroupOffsetsRequest
        {
        describeShareGroupOffsetsRequestGroups = fieldgroups
        }
  | otherwise = fail $ "Unsupported version: " ++ show version