{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListPartitionReassignmentsResponse
Description : Kafka ListPartitionReassignmentsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 46.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListPartitionReassignmentsResponse
  (
    ListPartitionReassignmentsResponse(..),
    OngoingTopicReassignment(..),
    OngoingPartitionReassignment(..),
    encodeListPartitionReassignmentsResponse,
    decodeListPartitionReassignmentsResponse,
    maxListPartitionReassignmentsResponseVersion
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


-- | The ongoing reassignments for each partition.
data OngoingPartitionReassignment = OngoingPartitionReassignment
  {

  -- | The index of the partition.

  -- Versions: 0+
  ongoingPartitionReassignmentPartitionIndex :: !(Int32)
,

  -- | The current replica set.

  -- Versions: 0+
  ongoingPartitionReassignmentReplicas :: !(KafkaArray (Int32))
,

  -- | The set of replicas we are currently adding.

  -- Versions: 0+
  ongoingPartitionReassignmentAddingReplicas :: !(KafkaArray (Int32))
,

  -- | The set of replicas we are currently removing.

  -- Versions: 0+
  ongoingPartitionReassignmentRemovingReplicas :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode OngoingPartitionReassignment with version-aware field handling.
encodeOngoingPartitionReassignment :: MonadPut m => E.ApiVersion -> OngoingPartitionReassignment -> m ()
encodeOngoingPartitionReassignment version omsg =
  do
    serialize (ongoingPartitionReassignmentPartitionIndex omsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (ongoingPartitionReassignmentReplicas omsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (ongoingPartitionReassignmentAddingReplicas omsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (ongoingPartitionReassignmentRemovingReplicas omsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OngoingPartitionReassignment with version-aware field handling.
decodeOngoingPartitionReassignment :: MonadGet m => E.ApiVersion -> m OngoingPartitionReassignment
decodeOngoingPartitionReassignment version =
  do
    fieldpartitionindex <- deserialize
    fieldreplicas <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    fieldaddingreplicas <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    fieldremovingreplicas <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OngoingPartitionReassignment
      {
      ongoingPartitionReassignmentPartitionIndex = fieldpartitionindex
      ,
      ongoingPartitionReassignmentReplicas = fieldreplicas
      ,
      ongoingPartitionReassignmentAddingReplicas = fieldaddingreplicas
      ,
      ongoingPartitionReassignmentRemovingReplicas = fieldremovingreplicas
      }


-- | The ongoing reassignments for each topic.
data OngoingTopicReassignment = OngoingTopicReassignment
  {

  -- | The topic name.

  -- Versions: 0+
  ongoingTopicReassignmentName :: !(KafkaString)
,

  -- | The ongoing reassignments for each partition.

  -- Versions: 0+
  ongoingTopicReassignmentPartitions :: !(KafkaArray (OngoingPartitionReassignment))

  }
  deriving (Eq, Show, Generic)


-- | Encode OngoingTopicReassignment with version-aware field handling.
encodeOngoingTopicReassignment :: MonadPut m => E.ApiVersion -> OngoingTopicReassignment -> m ()
encodeOngoingTopicReassignment version omsg =
  do
    if version >= 0 then serialize (toCompactString (ongoingTopicReassignmentName omsg)) else serialize (ongoingTopicReassignmentName omsg)
    E.encodeVersionedArray version 0 encodeOngoingPartitionReassignment (case P.unKafkaArray (ongoingTopicReassignmentPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OngoingTopicReassignment with version-aware field handling.
decodeOngoingTopicReassignment :: MonadGet m => E.ApiVersion -> m OngoingTopicReassignment
decodeOngoingTopicReassignment version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeOngoingPartitionReassignment
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OngoingTopicReassignment
      {
      ongoingTopicReassignmentName = fieldname
      ,
      ongoingTopicReassignmentPartitions = fieldpartitions
      }



data ListPartitionReassignmentsResponse = ListPartitionReassignmentsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  listPartitionReassignmentsResponseThrottleTimeMs :: !(Int32)
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 0+
  listPartitionReassignmentsResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  listPartitionReassignmentsResponseErrorMessage :: !(KafkaString)
,

  -- | The ongoing reassignments for each topic.

  -- Versions: 0+
  listPartitionReassignmentsResponseTopics :: !(KafkaArray (OngoingTopicReassignment))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListPartitionReassignmentsResponse.
maxListPartitionReassignmentsResponseVersion :: Int16
maxListPartitionReassignmentsResponseVersion = 0

-- | Encode ListPartitionReassignmentsResponse with the given API version.
encodeListPartitionReassignmentsResponse :: MonadPut m => E.ApiVersion -> ListPartitionReassignmentsResponse -> m ()
encodeListPartitionReassignmentsResponse version msg
  | version == 0 =
    do
      serialize (listPartitionReassignmentsResponseThrottleTimeMs msg)
      serialize (listPartitionReassignmentsResponseErrorCode msg)
      serialize (toCompactString (listPartitionReassignmentsResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeOngoingTopicReassignment (case P.unKafkaArray (listPartitionReassignmentsResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ListPartitionReassignmentsResponse with the given API version.
decodeListPartitionReassignmentsResponse :: MonadGet m => E.ApiVersion -> m ListPartitionReassignmentsResponse
decodeListPartitionReassignmentsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeOngoingTopicReassignment
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListPartitionReassignmentsResponse
        {
        listPartitionReassignmentsResponseThrottleTimeMs = fieldthrottletimems
        ,
        listPartitionReassignmentsResponseErrorCode = fielderrorcode
        ,
        listPartitionReassignmentsResponseErrorMessage = fielderrormessage
        ,
        listPartitionReassignmentsResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version