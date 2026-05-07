{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterPartitionReassignmentsResponse
Description : Kafka AlterPartitionReassignmentsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 45.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterPartitionReassignmentsResponse
  (
    AlterPartitionReassignmentsResponse(..),
    ReassignableTopicResponse(..),
    ReassignablePartitionResponse(..),
    encodeAlterPartitionReassignmentsResponse,
    decodeAlterPartitionReassignmentsResponse,
    maxAlterPartitionReassignmentsResponseVersion
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


-- | The responses to partitions to reassign.
data ReassignablePartitionResponse = ReassignablePartitionResponse
  {

  -- | The partition index.

  -- Versions: 0+
  reassignablePartitionResponsePartitionIndex :: !(Int32)
,

  -- | The error code for this partition, or 0 if there was no error.

  -- Versions: 0+
  reassignablePartitionResponseErrorCode :: !(Int16)
,

  -- | The error message for this partition, or null if there was no error.

  -- Versions: 0+
  reassignablePartitionResponseErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode ReassignablePartitionResponse with version-aware field handling.
encodeReassignablePartitionResponse :: MonadPut m => E.ApiVersion -> ReassignablePartitionResponse -> m ()
encodeReassignablePartitionResponse version rmsg =
  do
    serialize (reassignablePartitionResponsePartitionIndex rmsg)
    serialize (reassignablePartitionResponseErrorCode rmsg)
    if version >= 0 then serialize (toCompactString (reassignablePartitionResponseErrorMessage rmsg)) else serialize (reassignablePartitionResponseErrorMessage rmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReassignablePartitionResponse with version-aware field handling.
decodeReassignablePartitionResponse :: MonadGet m => E.ApiVersion -> m ReassignablePartitionResponse
decodeReassignablePartitionResponse version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReassignablePartitionResponse
      {
      reassignablePartitionResponsePartitionIndex = fieldpartitionindex
      ,
      reassignablePartitionResponseErrorCode = fielderrorcode
      ,
      reassignablePartitionResponseErrorMessage = fielderrormessage
      }


-- | The responses to topics to reassign.
data ReassignableTopicResponse = ReassignableTopicResponse
  {

  -- | The topic name.

  -- Versions: 0+
  reassignableTopicResponseName :: !(KafkaString)
,

  -- | The responses to partitions to reassign.

  -- Versions: 0+
  reassignableTopicResponsePartitions :: !(KafkaArray (ReassignablePartitionResponse))

  }
  deriving (Eq, Show, Generic)


-- | Encode ReassignableTopicResponse with version-aware field handling.
encodeReassignableTopicResponse :: MonadPut m => E.ApiVersion -> ReassignableTopicResponse -> m ()
encodeReassignableTopicResponse version rmsg =
  do
    if version >= 0 then serialize (toCompactString (reassignableTopicResponseName rmsg)) else serialize (reassignableTopicResponseName rmsg)
    E.encodeVersionedArray version 0 encodeReassignablePartitionResponse (case P.unKafkaArray (reassignableTopicResponsePartitions rmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReassignableTopicResponse with version-aware field handling.
decodeReassignableTopicResponse :: MonadGet m => E.ApiVersion -> m ReassignableTopicResponse
decodeReassignableTopicResponse version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReassignablePartitionResponse
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReassignableTopicResponse
      {
      reassignableTopicResponseName = fieldname
      ,
      reassignableTopicResponsePartitions = fieldpartitions
      }



data AlterPartitionReassignmentsResponse = AlterPartitionReassignmentsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  alterPartitionReassignmentsResponseThrottleTimeMs :: !(Int32)
,

  -- | The option indicating whether changing the replication factor of any given partition as part of the 

  -- Versions: 1+
  alterPartitionReassignmentsResponseAllowReplicationFactorChange :: !(Bool)
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 0+
  alterPartitionReassignmentsResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  alterPartitionReassignmentsResponseErrorMessage :: !(KafkaString)
,

  -- | The responses to topics to reassign.

  -- Versions: 0+
  alterPartitionReassignmentsResponseResponses :: !(KafkaArray (ReassignableTopicResponse))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterPartitionReassignmentsResponse.
maxAlterPartitionReassignmentsResponseVersion :: Int16
maxAlterPartitionReassignmentsResponseVersion = 1

-- | Encode AlterPartitionReassignmentsResponse with the given API version.
encodeAlterPartitionReassignmentsResponse :: MonadPut m => E.ApiVersion -> AlterPartitionReassignmentsResponse -> m ()
encodeAlterPartitionReassignmentsResponse version msg
  | version == 0 =
    do
      serialize (alterPartitionReassignmentsResponseThrottleTimeMs msg)
      serialize (alterPartitionReassignmentsResponseErrorCode msg)
      serialize (toCompactString (alterPartitionReassignmentsResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeReassignableTopicResponse (case P.unKafkaArray (alterPartitionReassignmentsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 1 =
    do
      serialize (alterPartitionReassignmentsResponseThrottleTimeMs msg)
      serialize (alterPartitionReassignmentsResponseAllowReplicationFactorChange msg)
      serialize (alterPartitionReassignmentsResponseErrorCode msg)
      serialize (toCompactString (alterPartitionReassignmentsResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeReassignableTopicResponse (case P.unKafkaArray (alterPartitionReassignmentsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterPartitionReassignmentsResponse with the given API version.
decodeAlterPartitionReassignmentsResponse :: MonadGet m => E.ApiVersion -> m AlterPartitionReassignmentsResponse
decodeAlterPartitionReassignmentsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReassignableTopicResponse
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterPartitionReassignmentsResponse
        {
        alterPartitionReassignmentsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterPartitionReassignmentsResponseAllowReplicationFactorChange = True
        ,
        alterPartitionReassignmentsResponseErrorCode = fielderrorcode
        ,
        alterPartitionReassignmentsResponseErrorMessage = fielderrormessage
        ,
        alterPartitionReassignmentsResponseResponses = fieldresponses
        }

  | version == 1 =
    do
      fieldthrottletimems <- deserialize
      fieldallowreplicationfactorchange <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeReassignableTopicResponse
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterPartitionReassignmentsResponse
        {
        alterPartitionReassignmentsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterPartitionReassignmentsResponseAllowReplicationFactorChange = fieldallowreplicationfactorchange
        ,
        alterPartitionReassignmentsResponseErrorCode = fielderrorcode
        ,
        alterPartitionReassignmentsResponseErrorMessage = fielderrormessage
        ,
        alterPartitionReassignmentsResponseResponses = fieldresponses
        }
  | otherwise = fail $ "Unsupported version: " ++ show version