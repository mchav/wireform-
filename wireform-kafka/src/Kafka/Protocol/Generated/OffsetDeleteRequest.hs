{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetDeleteRequest
Description : Kafka OffsetDeleteRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 47.



Valid versions: 0
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetDeleteRequest
  (
    OffsetDeleteRequest(..),
    OffsetDeleteRequestTopic(..),
    OffsetDeleteRequestPartition(..),
    encodeOffsetDeleteRequest,
    decodeOffsetDeleteRequest,
    maxOffsetDeleteRequestVersion
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


-- | Each partition to delete offsets for.
data OffsetDeleteRequestPartition = OffsetDeleteRequestPartition
  {

  -- | The partition index.

  -- Versions: 0+
  offsetDeleteRequestPartitionPartitionIndex :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetDeleteRequestPartition with version-aware field handling.
encodeOffsetDeleteRequestPartition :: MonadPut m => E.ApiVersion -> OffsetDeleteRequestPartition -> m ()
encodeOffsetDeleteRequestPartition _version omsg =
  do
    serialize (offsetDeleteRequestPartitionPartitionIndex omsg)


-- | Decode OffsetDeleteRequestPartition with version-aware field handling.
decodeOffsetDeleteRequestPartition :: MonadGet m => E.ApiVersion -> m OffsetDeleteRequestPartition
decodeOffsetDeleteRequestPartition _version =
  do
    fieldpartitionindex <- deserialize
    pure OffsetDeleteRequestPartition
      {
      offsetDeleteRequestPartitionPartitionIndex = fieldpartitionindex
      }


-- | The topics to delete offsets for.
data OffsetDeleteRequestTopic = OffsetDeleteRequestTopic
  {

  -- | The topic name.

  -- Versions: 0+
  offsetDeleteRequestTopicName :: !(KafkaString)
,

  -- | Each partition to delete offsets for.

  -- Versions: 0+
  offsetDeleteRequestTopicPartitions :: !(KafkaArray (OffsetDeleteRequestPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetDeleteRequestTopic with version-aware field handling.
encodeOffsetDeleteRequestTopic :: MonadPut m => E.ApiVersion -> OffsetDeleteRequestTopic -> m ()
encodeOffsetDeleteRequestTopic version omsg =
  do
    serialize (offsetDeleteRequestTopicName omsg)
    E.encodeVersionedArray version 999 encodeOffsetDeleteRequestPartition (case P.unKafkaArray (offsetDeleteRequestTopicPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })


-- | Decode OffsetDeleteRequestTopic with version-aware field handling.
decodeOffsetDeleteRequestTopic :: MonadGet m => E.ApiVersion -> m OffsetDeleteRequestTopic
decodeOffsetDeleteRequestTopic version =
  do
    fieldname <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeOffsetDeleteRequestPartition
    pure OffsetDeleteRequestTopic
      {
      offsetDeleteRequestTopicName = fieldname
      ,
      offsetDeleteRequestTopicPartitions = fieldpartitions
      }



data OffsetDeleteRequest = OffsetDeleteRequest
  {

  -- | The unique group identifier.

  -- Versions: 0+
  offsetDeleteRequestGroupId :: !(KafkaString)
,

  -- | The topics to delete offsets for.

  -- Versions: 0+
  offsetDeleteRequestTopics :: !(KafkaArray (OffsetDeleteRequestTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetDeleteRequest.
maxOffsetDeleteRequestVersion :: Int16
maxOffsetDeleteRequestVersion = 0

-- | KafkaMessage instance for OffsetDeleteRequest.
instance KafkaMessage OffsetDeleteRequest where
  messageApiKey = 47
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Nothing

-- | Encode OffsetDeleteRequest with the given API version.
encodeOffsetDeleteRequest :: MonadPut m => E.ApiVersion -> OffsetDeleteRequest -> m ()
encodeOffsetDeleteRequest version msg
  | version == 0 =
    do
      serialize (offsetDeleteRequestGroupId msg)
      E.encodeVersionedArray version 999 encodeOffsetDeleteRequestTopic (case P.unKafkaArray (offsetDeleteRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetDeleteRequest with the given API version.
decodeOffsetDeleteRequest :: MonadGet m => E.ApiVersion -> m OffsetDeleteRequest
decodeOffsetDeleteRequest version
  | version == 0 =
    do
      fieldgroupid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeOffsetDeleteRequestTopic
      pure OffsetDeleteRequest
        {
        offsetDeleteRequestGroupId = fieldgroupid
        ,
        offsetDeleteRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version