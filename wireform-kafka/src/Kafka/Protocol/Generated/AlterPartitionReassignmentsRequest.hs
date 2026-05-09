{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterPartitionReassignmentsRequest
Description : Kafka AlterPartitionReassignmentsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 45.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterPartitionReassignmentsRequest
  (
    AlterPartitionReassignmentsRequest(..),
    ReassignableTopic(..),
    ReassignablePartition(..),
    encodeAlterPartitionReassignmentsRequest,
    decodeAlterPartitionReassignmentsRequest,
    maxAlterPartitionReassignmentsRequestVersion
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


-- | The partitions to reassign.
data ReassignablePartition = ReassignablePartition
  {

  -- | The partition index.

  -- Versions: 0+
  reassignablePartitionPartitionIndex :: !(Int32)
,

  -- | The replicas to place the partitions on, or null to cancel a pending reassignment for this partition

  -- Versions: 0+
  reassignablePartitionReplicas :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode ReassignablePartition with version-aware field handling.
encodeReassignablePartition :: MonadPut m => E.ApiVersion -> ReassignablePartition -> m ()
encodeReassignablePartition version rmsg =
  do
    serialize (reassignablePartitionPartitionIndex rmsg)
    E.encodeVersionedNullableArray version 0 (\_ x -> serialize x) (reassignablePartitionReplicas rmsg) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReassignablePartition with version-aware field handling.
decodeReassignablePartition :: MonadGet m => E.ApiVersion -> m ReassignablePartition
decodeReassignablePartition version =
  do
    fieldpartitionindex <- deserialize
    fieldreplicas <- E.decodeVersionedNullableArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReassignablePartition
      {
      reassignablePartitionPartitionIndex = fieldpartitionindex
      ,
      reassignablePartitionReplicas = fieldreplicas
      }


-- | The topics to reassign.
data ReassignableTopic = ReassignableTopic
  {

  -- | The topic name.

  -- Versions: 0+
  reassignableTopicName :: !(KafkaString)
,

  -- | The partitions to reassign.

  -- Versions: 0+
  reassignableTopicPartitions :: !(KafkaArray (ReassignablePartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode ReassignableTopic with version-aware field handling.
encodeReassignableTopic :: MonadPut m => E.ApiVersion -> ReassignableTopic -> m ()
encodeReassignableTopic version rmsg =
  do
    if version >= 0 then serialize (toCompactString (reassignableTopicName rmsg)) else serialize (reassignableTopicName rmsg)
    E.encodeVersionedArray version 0 encodeReassignablePartition (case P.unKafkaArray (reassignableTopicPartitions rmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReassignableTopic with version-aware field handling.
decodeReassignableTopic :: MonadGet m => E.ApiVersion -> m ReassignableTopic
decodeReassignableTopic version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReassignablePartition
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReassignableTopic
      {
      reassignableTopicName = fieldname
      ,
      reassignableTopicPartitions = fieldpartitions
      }



data AlterPartitionReassignmentsRequest = AlterPartitionReassignmentsRequest
  {

  -- | The time in ms to wait for the request to complete.

  -- Versions: 0+
  alterPartitionReassignmentsRequestTimeoutMs :: !(Int32)
,

  -- | The option indicating whether changing the replication factor of any given partition as part of this

  -- Versions: 1+
  alterPartitionReassignmentsRequestAllowReplicationFactorChange :: !(Bool)
,

  -- | The topics to reassign.

  -- Versions: 0+
  alterPartitionReassignmentsRequestTopics :: !(KafkaArray (ReassignableTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterPartitionReassignmentsRequest.
maxAlterPartitionReassignmentsRequestVersion :: Int16
maxAlterPartitionReassignmentsRequestVersion = 1

-- | Encode AlterPartitionReassignmentsRequest with the given API version.
encodeAlterPartitionReassignmentsRequest :: MonadPut m => E.ApiVersion -> AlterPartitionReassignmentsRequest -> m ()
encodeAlterPartitionReassignmentsRequest version msg
  | version == 0 =
    do
      serialize (alterPartitionReassignmentsRequestTimeoutMs msg)
      E.encodeVersionedArray version 0 encodeReassignableTopic (case P.unKafkaArray (alterPartitionReassignmentsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 1 =
    do
      serialize (alterPartitionReassignmentsRequestTimeoutMs msg)
      serialize (alterPartitionReassignmentsRequestAllowReplicationFactorChange msg)
      E.encodeVersionedArray version 0 encodeReassignableTopic (case P.unKafkaArray (alterPartitionReassignmentsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterPartitionReassignmentsRequest with the given API version.
decodeAlterPartitionReassignmentsRequest :: MonadGet m => E.ApiVersion -> m AlterPartitionReassignmentsRequest
decodeAlterPartitionReassignmentsRequest version
  | version == 0 =
    do
      fieldtimeoutms <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReassignableTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterPartitionReassignmentsRequest
        {
        alterPartitionReassignmentsRequestTimeoutMs = fieldtimeoutms
        ,
        alterPartitionReassignmentsRequestAllowReplicationFactorChange = True
        ,
        alterPartitionReassignmentsRequestTopics = fieldtopics
        }

  | version == 1 =
    do
      fieldtimeoutms <- deserialize
      fieldallowreplicationfactorchange <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReassignableTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterPartitionReassignmentsRequest
        {
        alterPartitionReassignmentsRequestTimeoutMs = fieldtimeoutms
        ,
        alterPartitionReassignmentsRequestAllowReplicationFactorChange = fieldallowreplicationfactorchange
        ,
        alterPartitionReassignmentsRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeAlterPartitionReassignmentsRequest' / 'decodeAlterPartitionReassignmentsRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec AlterPartitionReassignmentsRequest where
  wireCodec = Just (WC.serialShimCodec encodeAlterPartitionReassignmentsRequest decodeAlterPartitionReassignmentsRequest)
  {-# INLINE wireCodec #-}
