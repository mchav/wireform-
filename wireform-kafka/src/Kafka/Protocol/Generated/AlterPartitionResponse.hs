{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterPartitionResponse
Description : Kafka AlterPartitionResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 56.



Valid versions: 2-3
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterPartitionResponse
  (
    AlterPartitionResponse(..),
    TopicData(..),
    PartitionData(..),
    encodeAlterPartitionResponse,
    decodeAlterPartitionResponse,
    maxAlterPartitionResponseVersion
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


-- | The responses for each partition.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The partition level error code.

  -- Versions: 0+
  partitionDataErrorCode :: !(Int16)
,

  -- | The broker ID of the leader.

  -- Versions: 0+
  partitionDataLeaderId :: !(Int32)
,

  -- | The leader epoch.

  -- Versions: 0+
  partitionDataLeaderEpoch :: !(Int32)
,

  -- | The in-sync replica IDs.

  -- Versions: 0+
  partitionDataIsr :: !(KafkaArray (Int32))
,

  -- | 1 if the partition is recovering from an unclean leader election; 0 otherwise.

  -- Versions: 1+
  partitionDataLeaderRecoveryState :: !(Int8)
,

  -- | The current epoch for the partition for KRaft controllers.

  -- Versions: 0+
  partitionDataPartitionEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataErrorCode pmsg)
    serialize (partitionDataLeaderId pmsg)
    serialize (partitionDataLeaderEpoch pmsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (partitionDataIsr pmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 1) $
      serialize (partitionDataLeaderRecoveryState pmsg)
    serialize (partitionDataPartitionEpoch pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fieldleaderid <- deserialize
    fieldleaderepoch <- deserialize
    fieldisr <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    fieldleaderrecoverystate <- if version >= 1
      then deserialize
      else pure (0)
    fieldpartitionepoch <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataErrorCode = fielderrorcode
      ,
      partitionDataLeaderId = fieldleaderid
      ,
      partitionDataLeaderEpoch = fieldleaderepoch
      ,
      partitionDataIsr = fieldisr
      ,
      partitionDataLeaderRecoveryState = fieldleaderrecoverystate
      ,
      partitionDataPartitionEpoch = fieldpartitionepoch
      }


-- | The responses for each topic.
data TopicData = TopicData
  {

  -- | The ID of the topic.

  -- Versions: 2+
  topicDataTopicId :: !(KafkaUuid)
,

  -- | The responses for each partition.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicData with version-aware field handling.
encodeTopicData :: MonadPut m => E.ApiVersion -> TopicData -> m ()
encodeTopicData version tmsg =
  do
    when (version >= 2) $
      serialize (topicDataTopicId tmsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (topicDataPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicData with version-aware field handling.
decodeTopicData :: MonadGet m => E.ApiVersion -> m TopicData
decodeTopicData version =
  do
    fieldtopicid <- if version >= 2
      then deserialize
      else pure (P.nullUuid)
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicData
      {
      topicDataTopicId = fieldtopicid
      ,
      topicDataPartitions = fieldpartitions
      }



data AlterPartitionResponse = AlterPartitionResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  alterPartitionResponseThrottleTimeMs :: !(Int32)
,

  -- | The top level response error code.

  -- Versions: 0+
  alterPartitionResponseErrorCode :: !(Int16)
,

  -- | The responses for each topic.

  -- Versions: 0+
  alterPartitionResponseTopics :: !(KafkaArray (TopicData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterPartitionResponse.
maxAlterPartitionResponseVersion :: Int16
maxAlterPartitionResponseVersion = 3

-- | Encode AlterPartitionResponse with the given API version.
encodeAlterPartitionResponse :: MonadPut m => E.ApiVersion -> AlterPartitionResponse -> m ()
encodeAlterPartitionResponse version msg
  | version >= 2 && version <= 3 =
    do
      serialize (alterPartitionResponseThrottleTimeMs msg)
      serialize (alterPartitionResponseErrorCode msg)
      E.encodeVersionedArray version 0 encodeTopicData (case P.unKafkaArray (alterPartitionResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterPartitionResponse with the given API version.
decodeAlterPartitionResponse :: MonadGet m => E.ApiVersion -> m AlterPartitionResponse
decodeAlterPartitionResponse version
  | version >= 2 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterPartitionResponse
        {
        alterPartitionResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterPartitionResponseErrorCode = fielderrorcode
        ,
        alterPartitionResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeAlterPartitionResponse' / 'decodeAlterPartitionResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec AlterPartitionResponse where
  wireCodec = Just (WC.serialShimCodec encodeAlterPartitionResponse decodeAlterPartitionResponse)
  {-# INLINE wireCodec #-}
