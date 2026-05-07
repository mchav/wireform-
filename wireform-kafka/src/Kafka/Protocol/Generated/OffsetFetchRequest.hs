{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetFetchRequest
Description : Kafka OffsetFetchRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 9.



Valid versions: 1-10
Flexible versions: 6+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetFetchRequest
  (
    OffsetFetchRequest(..),
    OffsetFetchRequestTopic(..),
    OffsetFetchRequestGroup(..),
    OffsetFetchRequestTopics(..),
    encodeOffsetFetchRequest,
    decodeOffsetFetchRequest,
    maxOffsetFetchRequestVersion
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


-- | Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
data OffsetFetchRequestTopic = OffsetFetchRequestTopic
  {

  -- | The topic name.

  -- Versions: 0-7
  offsetFetchRequestTopicName :: !(KafkaString)
,

  -- | The partition indexes we would like to fetch offsets for.

  -- Versions: 0-7
  offsetFetchRequestTopicPartitionIndexes :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetFetchRequestTopic with version-aware field handling.
encodeOffsetFetchRequestTopic :: MonadPut m => E.ApiVersion -> OffsetFetchRequestTopic -> m ()
encodeOffsetFetchRequestTopic version omsg =
  do
    when (version >= 0 && version <= 7) $
      if version >= 6 then serialize (toCompactString (offsetFetchRequestTopicName omsg)) else serialize (offsetFetchRequestTopicName omsg)
    when (version >= 0 && version <= 7) $
      E.encodeVersionedArray version 6 (\_ x -> serialize x) (case P.unKafkaArray (offsetFetchRequestTopicPartitionIndexes omsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchRequestTopic with version-aware field handling.
decodeOffsetFetchRequestTopic :: MonadGet m => E.ApiVersion -> m OffsetFetchRequestTopic
decodeOffsetFetchRequestTopic version =
  do
    fieldname <- if version >= 0 && version <= 7
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldpartitionindexes <- if version >= 0 && version <= 7
      then P.mkKafkaArray <$> E.decodeVersionedArray version 6 (\_ -> deserialize)
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchRequestTopic
      {
      offsetFetchRequestTopicName = fieldname
      ,
      offsetFetchRequestTopicPartitionIndexes = fieldpartitionindexes
      }


-- | Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.
data OffsetFetchRequestTopics = OffsetFetchRequestTopics
  {

  -- | The topic name.

  -- Versions: 8-9
  offsetFetchRequestTopicsName :: !(KafkaString)
,

  -- | The topic ID.

  -- Versions: 10+
  offsetFetchRequestTopicsTopicId :: !(KafkaUuid)
,

  -- | The partition indexes we would like to fetch offsets for.

  -- Versions: 8+
  offsetFetchRequestTopicsPartitionIndexes :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetFetchRequestTopics with version-aware field handling.
encodeOffsetFetchRequestTopics :: MonadPut m => E.ApiVersion -> OffsetFetchRequestTopics -> m ()
encodeOffsetFetchRequestTopics version omsg =
  do
    when (version >= 8 && version <= 9) $
      if version >= 6 then serialize (toCompactString (offsetFetchRequestTopicsName omsg)) else serialize (offsetFetchRequestTopicsName omsg)
    when (version >= 10) $
      serialize (offsetFetchRequestTopicsTopicId omsg)
    when (version >= 8) $
      E.encodeVersionedArray version 6 (\_ x -> serialize x) (case P.unKafkaArray (offsetFetchRequestTopicsPartitionIndexes omsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchRequestTopics with version-aware field handling.
decodeOffsetFetchRequestTopics :: MonadGet m => E.ApiVersion -> m OffsetFetchRequestTopics
decodeOffsetFetchRequestTopics version =
  do
    fieldname <- if version >= 8 && version <= 9
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 10
      then deserialize
      else pure (P.nullUuid)
    fieldpartitionindexes <- if version >= 8
      then P.mkKafkaArray <$> E.decodeVersionedArray version 6 (\_ -> deserialize)
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchRequestTopics
      {
      offsetFetchRequestTopicsName = fieldname
      ,
      offsetFetchRequestTopicsTopicId = fieldtopicid
      ,
      offsetFetchRequestTopicsPartitionIndexes = fieldpartitionindexes
      }


-- | Each group we would like to fetch offsets for.
data OffsetFetchRequestGroup = OffsetFetchRequestGroup
  {

  -- | The group ID.

  -- Versions: 8+
  offsetFetchRequestGroupGroupId :: !(KafkaString)
,

  -- | The member id.

  -- Versions: 9+
  offsetFetchRequestGroupMemberId :: !(KafkaString)
,

  -- | The member epoch if using the new consumer protocol (KIP-848).

  -- Versions: 9+
  offsetFetchRequestGroupMemberEpoch :: !(Int32)
,

  -- | Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.

  -- Versions: 8+
  offsetFetchRequestGroupTopics :: !(KafkaArray (OffsetFetchRequestTopics))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetFetchRequestGroup with version-aware field handling.
encodeOffsetFetchRequestGroup :: MonadPut m => E.ApiVersion -> OffsetFetchRequestGroup -> m ()
encodeOffsetFetchRequestGroup version omsg =
  do
    when (version >= 8) $
      if version >= 6 then serialize (toCompactString (offsetFetchRequestGroupGroupId omsg)) else serialize (offsetFetchRequestGroupGroupId omsg)
    when (version >= 9) $
      if version >= 6 then serialize (toCompactString (offsetFetchRequestGroupMemberId omsg)) else serialize (offsetFetchRequestGroupMemberId omsg)
    when (version >= 9) $
      serialize (offsetFetchRequestGroupMemberEpoch omsg)
    when (version >= 8) $
      E.encodeVersionedNullableArray version 6 encodeOffsetFetchRequestTopics (offsetFetchRequestGroupTopics omsg)
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchRequestGroup with version-aware field handling.
decodeOffsetFetchRequestGroup :: MonadGet m => E.ApiVersion -> m OffsetFetchRequestGroup
decodeOffsetFetchRequestGroup version =
  do
    fieldgroupid <- if version >= 8
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldmemberid <- if version >= 9
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldmemberepoch <- if version >= 9
      then deserialize
      else pure ((-1))
    fieldtopics <- if version >= 8
      then E.decodeVersionedNullableArray version 6 decodeOffsetFetchRequestTopics
      else pure (P.KafkaArray P.Null)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchRequestGroup
      {
      offsetFetchRequestGroupGroupId = fieldgroupid
      ,
      offsetFetchRequestGroupMemberId = fieldmemberid
      ,
      offsetFetchRequestGroupMemberEpoch = fieldmemberepoch
      ,
      offsetFetchRequestGroupTopics = fieldtopics
      }



data OffsetFetchRequest = OffsetFetchRequest
  {

  -- | The group to fetch offsets for.

  -- Versions: 0-7
  offsetFetchRequestGroupId :: !(KafkaString)
,

  -- | Each topic we would like to fetch offsets for, or null to fetch offsets for all topics.

  -- Versions: 0-7
  offsetFetchRequestTopics :: !(KafkaArray (OffsetFetchRequestTopic))
,

  -- | Each group we would like to fetch offsets for.

  -- Versions: 8+
  offsetFetchRequestGroups :: !(KafkaArray (OffsetFetchRequestGroup))
,

  -- | Whether broker should hold on returning unstable offsets but set a retriable error code for the part

  -- Versions: 7+
  offsetFetchRequestRequireStable :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetFetchRequest.
maxOffsetFetchRequestVersion :: Int16
maxOffsetFetchRequestVersion = 10

-- | Encode OffsetFetchRequest with the given API version.
encodeOffsetFetchRequest :: MonadPut m => E.ApiVersion -> OffsetFetchRequest -> m ()
encodeOffsetFetchRequest version msg
  | version == 6 =
    do
      serialize (toCompactString (offsetFetchRequestGroupId msg))
      E.encodeVersionedNullableArray version 6 encodeOffsetFetchRequestTopic (offsetFetchRequestTopics msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 7 =
    do
      serialize (toCompactString (offsetFetchRequestGroupId msg))
      E.encodeVersionedNullableArray version 6 encodeOffsetFetchRequestTopic (offsetFetchRequestTopics msg)
      serialize (offsetFetchRequestRequireStable msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 8 && version <= 10 =
    do
      E.encodeVersionedArray version 6 encodeOffsetFetchRequestGroup (case P.unKafkaArray (offsetFetchRequestGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (offsetFetchRequestRequireStable msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 5 =
    do
      serialize (offsetFetchRequestGroupId msg)
      E.encodeVersionedNullableArray version 6 encodeOffsetFetchRequestTopic (offsetFetchRequestTopics msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetFetchRequest with the given API version.
decodeOffsetFetchRequest :: MonadGet m => E.ApiVersion -> m OffsetFetchRequest
decodeOffsetFetchRequest version
  | version == 6 =
    do
      fieldgroupid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- E.decodeVersionedNullableArray version 6 decodeOffsetFetchRequestTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetFetchRequest
        {
        offsetFetchRequestGroupId = fieldgroupid
        ,
        offsetFetchRequestTopics = fieldtopics
        ,
        offsetFetchRequestGroups = P.mkKafkaArray V.empty
        ,
        offsetFetchRequestRequireStable = False
        }

  | version == 7 =
    do
      fieldgroupid <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- E.decodeVersionedNullableArray version 6 decodeOffsetFetchRequestTopic
      fieldrequirestable <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetFetchRequest
        {
        offsetFetchRequestGroupId = fieldgroupid
        ,
        offsetFetchRequestTopics = fieldtopics
        ,
        offsetFetchRequestGroups = P.mkKafkaArray V.empty
        ,
        offsetFetchRequestRequireStable = fieldrequirestable
        }

  | version >= 8 && version <= 10 =
    do
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchRequestGroup
      fieldrequirestable <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetFetchRequest
        {
        offsetFetchRequestGroupId = P.KafkaString Null
        ,
        offsetFetchRequestTopics = P.KafkaArray P.Null
        ,
        offsetFetchRequestGroups = fieldgroups
        ,
        offsetFetchRequestRequireStable = fieldrequirestable
        }

  | version >= 1 && version <= 5 =
    do
      fieldgroupid <- deserialize
      fieldtopics <- E.decodeVersionedNullableArray version 6 decodeOffsetFetchRequestTopic
      pure OffsetFetchRequest
        {
        offsetFetchRequestGroupId = fieldgroupid
        ,
        offsetFetchRequestTopics = fieldtopics
        ,
        offsetFetchRequestGroups = P.mkKafkaArray V.empty
        ,
        offsetFetchRequestRequireStable = False
        }
  | otherwise = fail $ "Unsupported version: " ++ show version