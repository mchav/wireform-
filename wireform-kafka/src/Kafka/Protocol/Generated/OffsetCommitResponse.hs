{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetCommitResponse
Description : Kafka OffsetCommitResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 8.



Valid versions: 2-10
Flexible versions: 8+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetCommitResponse
  (
    OffsetCommitResponse(..),
    OffsetCommitResponseTopic(..),
    OffsetCommitResponsePartition(..),
    encodeOffsetCommitResponse,
    decodeOffsetCommitResponse,
    maxOffsetCommitResponseVersion
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


-- | The responses for each partition in the topic.
data OffsetCommitResponsePartition = OffsetCommitResponsePartition
  {

  -- | The partition index.

  -- Versions: 0+
  offsetCommitResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  offsetCommitResponsePartitionErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetCommitResponsePartition with version-aware field handling.
encodeOffsetCommitResponsePartition :: MonadPut m => E.ApiVersion -> OffsetCommitResponsePartition -> m ()
encodeOffsetCommitResponsePartition version omsg =
  do
    serialize (offsetCommitResponsePartitionPartitionIndex omsg)
    serialize (offsetCommitResponsePartitionErrorCode omsg)
    when (version >= 8) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetCommitResponsePartition with version-aware field handling.
decodeOffsetCommitResponsePartition :: MonadGet m => E.ApiVersion -> m OffsetCommitResponsePartition
decodeOffsetCommitResponsePartition version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    _ <- if version >= 8 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetCommitResponsePartition
      {
      offsetCommitResponsePartitionPartitionIndex = fieldpartitionindex
      ,
      offsetCommitResponsePartitionErrorCode = fielderrorcode
      }


-- | The responses for each topic.
data OffsetCommitResponseTopic = OffsetCommitResponseTopic
  {

  -- | The topic name.

  -- Versions: 0-9
  offsetCommitResponseTopicName :: !(KafkaString)
,

  -- | The topic ID.

  -- Versions: 10+
  offsetCommitResponseTopicTopicId :: !(KafkaUuid)
,

  -- | The responses for each partition in the topic.

  -- Versions: 0+
  offsetCommitResponseTopicPartitions :: !(KafkaArray (OffsetCommitResponsePartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetCommitResponseTopic with version-aware field handling.
encodeOffsetCommitResponseTopic :: MonadPut m => E.ApiVersion -> OffsetCommitResponseTopic -> m ()
encodeOffsetCommitResponseTopic version omsg =
  do
    when (version >= 0 && version <= 9) $
      if version >= 8 then serialize (toCompactString (offsetCommitResponseTopicName omsg)) else serialize (offsetCommitResponseTopicName omsg)
    when (version >= 10) $
      serialize (offsetCommitResponseTopicTopicId omsg)
    E.encodeVersionedArray version 8 encodeOffsetCommitResponsePartition (case P.unKafkaArray (offsetCommitResponseTopicPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 8) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetCommitResponseTopic with version-aware field handling.
decodeOffsetCommitResponseTopic :: MonadGet m => E.ApiVersion -> m OffsetCommitResponseTopic
decodeOffsetCommitResponseTopic version =
  do
    fieldname <- if version >= 0 && version <= 9
      then if version >= 8 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 10
      then deserialize
      else pure (P.nullUuid)
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 8 decodeOffsetCommitResponsePartition
    _ <- if version >= 8 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetCommitResponseTopic
      {
      offsetCommitResponseTopicName = fieldname
      ,
      offsetCommitResponseTopicTopicId = fieldtopicid
      ,
      offsetCommitResponseTopicPartitions = fieldpartitions
      }



data OffsetCommitResponse = OffsetCommitResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 3+
  offsetCommitResponseThrottleTimeMs :: !(Int32)
,

  -- | The responses for each topic.

  -- Versions: 0+
  offsetCommitResponseTopics :: !(KafkaArray (OffsetCommitResponseTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetCommitResponse.
maxOffsetCommitResponseVersion :: Int16
maxOffsetCommitResponseVersion = 10

-- | Encode OffsetCommitResponse with the given API version.
encodeOffsetCommitResponse :: MonadPut m => E.ApiVersion -> OffsetCommitResponse -> m ()
encodeOffsetCommitResponse version msg
  | version == 2 =
    do
      E.encodeVersionedArray version 8 encodeOffsetCommitResponseTopic (case P.unKafkaArray (offsetCommitResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 8 && version <= 10 =
    do
      serialize (offsetCommitResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 8 encodeOffsetCommitResponseTopic (case P.unKafkaArray (offsetCommitResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 3 && version <= 7 =
    do
      serialize (offsetCommitResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 8 encodeOffsetCommitResponseTopic (case P.unKafkaArray (offsetCommitResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetCommitResponse with the given API version.
decodeOffsetCommitResponse :: MonadGet m => E.ApiVersion -> m OffsetCommitResponse
decodeOffsetCommitResponse version
  | version == 2 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 8 decodeOffsetCommitResponseTopic
      pure OffsetCommitResponse
        {
        offsetCommitResponseThrottleTimeMs = 0
        ,
        offsetCommitResponseTopics = fieldtopics
        }

  | version >= 8 && version <= 10 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 8 decodeOffsetCommitResponseTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetCommitResponse
        {
        offsetCommitResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetCommitResponseTopics = fieldtopics
        }

  | version >= 3 && version <= 7 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 8 decodeOffsetCommitResponseTopic
      pure OffsetCommitResponse
        {
        offsetCommitResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetCommitResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version