{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.CreateTopicsRequest
Description : Kafka CreateTopicsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 19.



Valid versions: 2-7
Flexible versions: 5+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.CreateTopicsRequest
  (
    CreateTopicsRequest(..),
    CreatableTopic(..),
    CreatableReplicaAssignment(..),
    CreatableTopicConfig(..),
    encodeCreateTopicsRequest,
    decodeCreateTopicsRequest,
    maxCreateTopicsRequestVersion
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


-- | The manual partition assignment, or the empty array if we are using automatic assignment.
data CreatableReplicaAssignment = CreatableReplicaAssignment
  {

  -- | The partition index.

  -- Versions: 0+
  creatableReplicaAssignmentPartitionIndex :: !(Int32)
,

  -- | The brokers to place the partition on.

  -- Versions: 0+
  creatableReplicaAssignmentBrokerIds :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode CreatableReplicaAssignment with version-aware field handling.
encodeCreatableReplicaAssignment :: MonadPut m => E.ApiVersion -> CreatableReplicaAssignment -> m ()
encodeCreatableReplicaAssignment version cmsg =
  do
    serialize (creatableReplicaAssignmentPartitionIndex cmsg)
    E.encodeVersionedArray version 5 (\_ x -> serialize x) (case P.unKafkaArray (creatableReplicaAssignmentBrokerIds cmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 5) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CreatableReplicaAssignment with version-aware field handling.
decodeCreatableReplicaAssignment :: MonadGet m => E.ApiVersion -> m CreatableReplicaAssignment
decodeCreatableReplicaAssignment version =
  do
    fieldpartitionindex <- deserialize
    fieldbrokerids <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 (\_ -> deserialize)
    _ <- if version >= 5 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CreatableReplicaAssignment
      {
      creatableReplicaAssignmentPartitionIndex = fieldpartitionindex
      ,
      creatableReplicaAssignmentBrokerIds = fieldbrokerids
      }


-- | The custom topic configurations to set.
data CreatableTopicConfig = CreatableTopicConfig
  {

  -- | The configuration name.

  -- Versions: 0+
  creatableTopicConfigName :: !(KafkaString)
,

  -- | The configuration value.

  -- Versions: 0+
  creatableTopicConfigValue :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode CreatableTopicConfig with version-aware field handling.
encodeCreatableTopicConfig :: MonadPut m => E.ApiVersion -> CreatableTopicConfig -> m ()
encodeCreatableTopicConfig version cmsg =
  do
    if version >= 5 then serialize (toCompactString (creatableTopicConfigName cmsg)) else serialize (creatableTopicConfigName cmsg)
    if version >= 5 then serialize (toCompactString (creatableTopicConfigValue cmsg)) else serialize (creatableTopicConfigValue cmsg)
    when (version >= 5) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CreatableTopicConfig with version-aware field handling.
decodeCreatableTopicConfig :: MonadGet m => E.ApiVersion -> m CreatableTopicConfig
decodeCreatableTopicConfig version =
  do
    fieldname <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldvalue <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 5 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CreatableTopicConfig
      {
      creatableTopicConfigName = fieldname
      ,
      creatableTopicConfigValue = fieldvalue
      }


-- | The topics to create.
data CreatableTopic = CreatableTopic
  {

  -- | The topic name.

  -- Versions: 0+
  creatableTopicName :: !(KafkaString)
,

  -- | The number of partitions to create in the topic, or -1 if we are either specifying a manual partitio

  -- Versions: 0+
  creatableTopicNumPartitions :: !(Int32)
,

  -- | The number of replicas to create for each partition in the topic, or -1 if we are either specifying 

  -- Versions: 0+
  creatableTopicReplicationFactor :: !(Int16)
,

  -- | The manual partition assignment, or the empty array if we are using automatic assignment.

  -- Versions: 0+
  creatableTopicAssignments :: !(KafkaArray (CreatableReplicaAssignment))
,

  -- | The custom topic configurations to set.

  -- Versions: 0+
  creatableTopicConfigs :: !(KafkaArray (CreatableTopicConfig))

  }
  deriving (Eq, Show, Generic)


-- | Encode CreatableTopic with version-aware field handling.
encodeCreatableTopic :: MonadPut m => E.ApiVersion -> CreatableTopic -> m ()
encodeCreatableTopic version cmsg =
  do
    if version >= 5 then serialize (toCompactString (creatableTopicName cmsg)) else serialize (creatableTopicName cmsg)
    serialize (creatableTopicNumPartitions cmsg)
    serialize (creatableTopicReplicationFactor cmsg)
    E.encodeVersionedArray version 5 encodeCreatableReplicaAssignment (case P.unKafkaArray (creatableTopicAssignments cmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    E.encodeVersionedArray version 5 encodeCreatableTopicConfig (case P.unKafkaArray (creatableTopicConfigs cmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 5) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CreatableTopic with version-aware field handling.
decodeCreatableTopic :: MonadGet m => E.ApiVersion -> m CreatableTopic
decodeCreatableTopic version =
  do
    fieldname <- if version >= 5 then P.fromCompactString <$> deserialize else deserialize
    fieldnumpartitions <- deserialize
    fieldreplicationfactor <- deserialize
    fieldassignments <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeCreatableReplicaAssignment
    fieldconfigs <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeCreatableTopicConfig
    _ <- if version >= 5 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CreatableTopic
      {
      creatableTopicName = fieldname
      ,
      creatableTopicNumPartitions = fieldnumpartitions
      ,
      creatableTopicReplicationFactor = fieldreplicationfactor
      ,
      creatableTopicAssignments = fieldassignments
      ,
      creatableTopicConfigs = fieldconfigs
      }



data CreateTopicsRequest = CreateTopicsRequest
  {

  -- | The topics to create.

  -- Versions: 0+
  createTopicsRequestTopics :: !(KafkaArray (CreatableTopic))
,

  -- | How long to wait in milliseconds before timing out the request.

  -- Versions: 0+
  createTopicsRequesttimeoutMs :: !(Int32)
,

  -- | If true, check that the topics can be created as specified, but don't create anything.

  -- Versions: 1+
  createTopicsRequestvalidateOnly :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for CreateTopicsRequest.
maxCreateTopicsRequestVersion :: Int16
maxCreateTopicsRequestVersion = 7

-- | Encode CreateTopicsRequest with the given API version.
encodeCreateTopicsRequest :: MonadPut m => E.ApiVersion -> CreateTopicsRequest -> m ()
encodeCreateTopicsRequest version msg
  | version >= 2 && version <= 4 =
    do
      E.encodeVersionedArray version 5 encodeCreatableTopic (case P.unKafkaArray (createTopicsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (createTopicsRequesttimeoutMs msg)
      serialize (createTopicsRequestvalidateOnly msg)


  | version >= 5 && version <= 7 =
    do
      E.encodeVersionedArray version 5 encodeCreatableTopic (case P.unKafkaArray (createTopicsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (createTopicsRequesttimeoutMs msg)
      serialize (createTopicsRequestvalidateOnly msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode CreateTopicsRequest with the given API version.
decodeCreateTopicsRequest :: MonadGet m => E.ApiVersion -> m CreateTopicsRequest
decodeCreateTopicsRequest version
  | version >= 2 && version <= 4 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeCreatableTopic
      fieldtimeoutms <- deserialize
      fieldvalidateonly <- deserialize
      pure CreateTopicsRequest
        {
        createTopicsRequestTopics = fieldtopics
        ,
        createTopicsRequesttimeoutMs = fieldtimeoutms
        ,
        createTopicsRequestvalidateOnly = fieldvalidateonly
        }

  | version >= 5 && version <= 7 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 5 decodeCreatableTopic
      fieldtimeoutms <- deserialize
      fieldvalidateonly <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure CreateTopicsRequest
        {
        createTopicsRequestTopics = fieldtopics
        ,
        createTopicsRequesttimeoutMs = fieldtimeoutms
        ,
        createTopicsRequestvalidateOnly = fieldvalidateonly
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeCreateTopicsRequest' / 'decodeCreateTopicsRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec CreateTopicsRequest where
  wireCodec = Just (WC.serialShimCodec encodeCreateTopicsRequest decodeCreateTopicsRequest)
  {-# INLINE wireCodec #-}
