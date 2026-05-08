{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.CreatePartitionsRequest
Description : Kafka CreatePartitionsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 37.



Valid versions: 0-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.CreatePartitionsRequest
  (
    CreatePartitionsRequest(..),
    CreatePartitionsTopic(..),
    CreatePartitionsAssignment(..),
    encodeCreatePartitionsRequest,
    decodeCreatePartitionsRequest,
    maxCreatePartitionsRequestVersion
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


-- | The new partition assignments.
data CreatePartitionsAssignment = CreatePartitionsAssignment
  {

  -- | The assigned broker IDs.

  -- Versions: 0+
  createPartitionsAssignmentBrokerIds :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode CreatePartitionsAssignment with version-aware field handling.
encodeCreatePartitionsAssignment :: MonadPut m => E.ApiVersion -> CreatePartitionsAssignment -> m ()
encodeCreatePartitionsAssignment version cmsg =
  do
    E.encodeVersionedArray version 2 (\_ x -> serialize x) (case P.unKafkaArray (createPartitionsAssignmentBrokerIds cmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CreatePartitionsAssignment with version-aware field handling.
decodeCreatePartitionsAssignment :: MonadGet m => E.ApiVersion -> m CreatePartitionsAssignment
decodeCreatePartitionsAssignment version =
  do
    fieldbrokerids <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 (\_ -> deserialize)
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CreatePartitionsAssignment
      {
      createPartitionsAssignmentBrokerIds = fieldbrokerids
      }


-- | Each topic that we want to create new partitions inside.
data CreatePartitionsTopic = CreatePartitionsTopic
  {

  -- | The topic name.

  -- Versions: 0+
  createPartitionsTopicName :: !(KafkaString)
,

  -- | The new partition count.

  -- Versions: 0+
  createPartitionsTopicCount :: !(Int32)
,

  -- | The new partition assignments.

  -- Versions: 0+
  createPartitionsTopicAssignments :: !(KafkaArray (CreatePartitionsAssignment))

  }
  deriving (Eq, Show, Generic)


-- | Encode CreatePartitionsTopic with version-aware field handling.
encodeCreatePartitionsTopic :: MonadPut m => E.ApiVersion -> CreatePartitionsTopic -> m ()
encodeCreatePartitionsTopic version cmsg =
  do
    if version >= 2 then serialize (toCompactString (createPartitionsTopicName cmsg)) else serialize (createPartitionsTopicName cmsg)
    serialize (createPartitionsTopicCount cmsg)
    E.encodeVersionedNullableArray version 2 encodeCreatePartitionsAssignment (createPartitionsTopicAssignments cmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CreatePartitionsTopic with version-aware field handling.
decodeCreatePartitionsTopic :: MonadGet m => E.ApiVersion -> m CreatePartitionsTopic
decodeCreatePartitionsTopic version =
  do
    fieldname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldcount <- deserialize
    fieldassignments <- E.decodeVersionedNullableArray version 2 decodeCreatePartitionsAssignment
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CreatePartitionsTopic
      {
      createPartitionsTopicName = fieldname
      ,
      createPartitionsTopicCount = fieldcount
      ,
      createPartitionsTopicAssignments = fieldassignments
      }



data CreatePartitionsRequest = CreatePartitionsRequest
  {

  -- | Each topic that we want to create new partitions inside.

  -- Versions: 0+
  createPartitionsRequestTopics :: !(KafkaArray (CreatePartitionsTopic))
,

  -- | The time in ms to wait for the partitions to be created.

  -- Versions: 0+
  createPartitionsRequestTimeoutMs :: !(Int32)
,

  -- | If true, then validate the request, but don't actually increase the number of partitions.

  -- Versions: 0+
  createPartitionsRequestValidateOnly :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for CreatePartitionsRequest.
maxCreatePartitionsRequestVersion :: Int16
maxCreatePartitionsRequestVersion = 3

-- | Encode CreatePartitionsRequest with the given API version.
encodeCreatePartitionsRequest :: MonadPut m => E.ApiVersion -> CreatePartitionsRequest -> m ()
encodeCreatePartitionsRequest version msg
  | version >= 0 && version <= 1 =
    do
      E.encodeVersionedArray version 2 encodeCreatePartitionsTopic (case P.unKafkaArray (createPartitionsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (createPartitionsRequestTimeoutMs msg)
      serialize (createPartitionsRequestValidateOnly msg)


  | version >= 2 && version <= 3 =
    do
      E.encodeVersionedArray version 2 encodeCreatePartitionsTopic (case P.unKafkaArray (createPartitionsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (createPartitionsRequestTimeoutMs msg)
      serialize (createPartitionsRequestValidateOnly msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode CreatePartitionsRequest with the given API version.
decodeCreatePartitionsRequest :: MonadGet m => E.ApiVersion -> m CreatePartitionsRequest
decodeCreatePartitionsRequest version
  | version >= 0 && version <= 1 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeCreatePartitionsTopic
      fieldtimeoutms <- deserialize
      fieldvalidateonly <- deserialize
      pure CreatePartitionsRequest
        {
        createPartitionsRequestTopics = fieldtopics
        ,
        createPartitionsRequestTimeoutMs = fieldtimeoutms
        ,
        createPartitionsRequestValidateOnly = fieldvalidateonly
        }

  | version >= 2 && version <= 3 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeCreatePartitionsTopic
      fieldtimeoutms <- deserialize
      fieldvalidateonly <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure CreatePartitionsRequest
        {
        createPartitionsRequestTopics = fieldtopics
        ,
        createPartitionsRequestTimeoutMs = fieldtimeoutms
        ,
        createPartitionsRequestValidateOnly = fieldvalidateonly
        }
  | otherwise = fail $ "Unsupported version: " ++ show version