{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetForLeaderEpochRequest
Description : Kafka OffsetForLeaderEpochRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 23.



Valid versions: 2-4
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetForLeaderEpochRequest
  (
    OffsetForLeaderEpochRequest(..),
    OffsetForLeaderTopic(..),
    OffsetForLeaderPartition(..),
    encodeOffsetForLeaderEpochRequest,
    decodeOffsetForLeaderEpochRequest,
    maxOffsetForLeaderEpochRequestVersion
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


-- | Each partition to get offsets for.
data OffsetForLeaderPartition = OffsetForLeaderPartition
  {

  -- | The partition index.

  -- Versions: 0+
  offsetForLeaderPartitionPartition :: !(Int32)
,

  -- | An epoch used to fence consumers/replicas with old metadata. If the epoch provided by the client is 

  -- Versions: 2+
  offsetForLeaderPartitionCurrentLeaderEpoch :: !(Int32)
,

  -- | The epoch to look up an offset for.

  -- Versions: 0+
  offsetForLeaderPartitionLeaderEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetForLeaderPartition with version-aware field handling.
encodeOffsetForLeaderPartition :: MonadPut m => E.ApiVersion -> OffsetForLeaderPartition -> m ()
encodeOffsetForLeaderPartition version omsg =
  do
    serialize (offsetForLeaderPartitionPartition omsg)
    when (version >= 2) $
      serialize (offsetForLeaderPartitionCurrentLeaderEpoch omsg)
    serialize (offsetForLeaderPartitionLeaderEpoch omsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetForLeaderPartition with version-aware field handling.
decodeOffsetForLeaderPartition :: MonadGet m => E.ApiVersion -> m OffsetForLeaderPartition
decodeOffsetForLeaderPartition version =
  do
    fieldpartition <- deserialize
    fieldcurrentleaderepoch <- if version >= 2
      then deserialize
      else pure ((-1))
    fieldleaderepoch <- deserialize
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetForLeaderPartition
      {
      offsetForLeaderPartitionPartition = fieldpartition
      ,
      offsetForLeaderPartitionCurrentLeaderEpoch = fieldcurrentleaderepoch
      ,
      offsetForLeaderPartitionLeaderEpoch = fieldleaderepoch
      }


-- | Each topic to get offsets for.
data OffsetForLeaderTopic = OffsetForLeaderTopic
  {

  -- | The topic name.

  -- Versions: 0+
  offsetForLeaderTopicTopic :: !(KafkaString)
,

  -- | Each partition to get offsets for.

  -- Versions: 0+
  offsetForLeaderTopicPartitions :: !(KafkaArray (OffsetForLeaderPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetForLeaderTopic with version-aware field handling.
encodeOffsetForLeaderTopic :: MonadPut m => E.ApiVersion -> OffsetForLeaderTopic -> m ()
encodeOffsetForLeaderTopic version omsg =
  do
    if version >= 4 then serialize (toCompactString (offsetForLeaderTopicTopic omsg)) else serialize (offsetForLeaderTopicTopic omsg)
    E.encodeVersionedArray version 4 encodeOffsetForLeaderPartition (case P.unKafkaArray (offsetForLeaderTopicPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetForLeaderTopic with version-aware field handling.
decodeOffsetForLeaderTopic :: MonadGet m => E.ApiVersion -> m OffsetForLeaderTopic
decodeOffsetForLeaderTopic version =
  do
    fieldtopic <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeOffsetForLeaderPartition
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetForLeaderTopic
      {
      offsetForLeaderTopicTopic = fieldtopic
      ,
      offsetForLeaderTopicPartitions = fieldpartitions
      }



data OffsetForLeaderEpochRequest = OffsetForLeaderEpochRequest
  {

  -- | The broker ID of the follower, of -1 if this request is from a consumer.

  -- Versions: 3+
  offsetForLeaderEpochRequestReplicaId :: !(Int32)
,

  -- | Each topic to get offsets for.

  -- Versions: 0+
  offsetForLeaderEpochRequestTopics :: !(KafkaArray (OffsetForLeaderTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetForLeaderEpochRequest.
maxOffsetForLeaderEpochRequestVersion :: Int16
maxOffsetForLeaderEpochRequestVersion = 4

-- | Encode OffsetForLeaderEpochRequest with the given API version.
encodeOffsetForLeaderEpochRequest :: MonadPut m => E.ApiVersion -> OffsetForLeaderEpochRequest -> m ()
encodeOffsetForLeaderEpochRequest version msg
  | version == 2 =
    do
      E.encodeVersionedArray version 4 encodeOffsetForLeaderTopic (case P.unKafkaArray (offsetForLeaderEpochRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 3 =
    do
      serialize (offsetForLeaderEpochRequestReplicaId msg)
      E.encodeVersionedArray version 4 encodeOffsetForLeaderTopic (case P.unKafkaArray (offsetForLeaderEpochRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 4 =
    do
      serialize (offsetForLeaderEpochRequestReplicaId msg)
      E.encodeVersionedArray version 4 encodeOffsetForLeaderTopic (case P.unKafkaArray (offsetForLeaderEpochRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetForLeaderEpochRequest with the given API version.
decodeOffsetForLeaderEpochRequest :: MonadGet m => E.ApiVersion -> m OffsetForLeaderEpochRequest
decodeOffsetForLeaderEpochRequest version
  | version == 2 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeOffsetForLeaderTopic
      pure OffsetForLeaderEpochRequest
        {
        offsetForLeaderEpochRequestReplicaId = (-2)
        ,
        offsetForLeaderEpochRequestTopics = fieldtopics
        }

  | version == 3 =
    do
      fieldreplicaid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeOffsetForLeaderTopic
      pure OffsetForLeaderEpochRequest
        {
        offsetForLeaderEpochRequestReplicaId = fieldreplicaid
        ,
        offsetForLeaderEpochRequestTopics = fieldtopics
        }

  | version == 4 =
    do
      fieldreplicaid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeOffsetForLeaderTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetForLeaderEpochRequest
        {
        offsetForLeaderEpochRequestReplicaId = fieldreplicaid
        ,
        offsetForLeaderEpochRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec OffsetForLeaderEpochRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
