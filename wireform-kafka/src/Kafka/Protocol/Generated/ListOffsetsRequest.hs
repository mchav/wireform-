{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListOffsetsRequest
Description : Kafka ListOffsetsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 2.



Valid versions: 1-10
Flexible versions: 6+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListOffsetsRequest
  (
    ListOffsetsRequest(..),
    ListOffsetsTopic(..),
    ListOffsetsPartition(..),
    encodeListOffsetsRequest,
    decodeListOffsetsRequest,
    maxListOffsetsRequestVersion
  ) where

import Control.Monad (when)
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
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
import qualified Kafka.Protocol.Wire.Codec as WC


-- | Each partition in the request.
data ListOffsetsPartition = ListOffsetsPartition
  {

  -- | The partition index.

  -- Versions: 0+
  listOffsetsPartitionPartitionIndex :: !(Int32)
,

  -- | The current leader epoch.

  -- Versions: 4+
  listOffsetsPartitionCurrentLeaderEpoch :: !(Int32)
,

  -- | The current timestamp.

  -- Versions: 0+
  listOffsetsPartitionTimestamp :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode ListOffsetsPartition with version-aware field handling.
encodeListOffsetsPartition :: MonadPut m => E.ApiVersion -> ListOffsetsPartition -> m ()
encodeListOffsetsPartition version lmsg =
  do
    serialize (listOffsetsPartitionPartitionIndex lmsg)
    when (version >= 4) $
      serialize (listOffsetsPartitionCurrentLeaderEpoch lmsg)
    serialize (listOffsetsPartitionTimestamp lmsg)
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ListOffsetsPartition with version-aware field handling.
decodeListOffsetsPartition :: MonadGet m => E.ApiVersion -> m ListOffsetsPartition
decodeListOffsetsPartition version =
  do
    fieldpartitionindex <- deserialize
    fieldcurrentleaderepoch <- if version >= 4
      then deserialize
      else pure ((-1))
    fieldtimestamp <- deserialize
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ListOffsetsPartition
      {
      listOffsetsPartitionPartitionIndex = fieldpartitionindex
      ,
      listOffsetsPartitionCurrentLeaderEpoch = fieldcurrentleaderepoch
      ,
      listOffsetsPartitionTimestamp = fieldtimestamp
      }


-- | Each topic in the request.
data ListOffsetsTopic = ListOffsetsTopic
  {

  -- | The topic name.

  -- Versions: 0+
  listOffsetsTopicName :: !(KafkaString)
,

  -- | Each partition in the request.

  -- Versions: 0+
  listOffsetsTopicPartitions :: !(KafkaArray (ListOffsetsPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode ListOffsetsTopic with version-aware field handling.
encodeListOffsetsTopic :: MonadPut m => E.ApiVersion -> ListOffsetsTopic -> m ()
encodeListOffsetsTopic version lmsg =
  do
    if version >= 6 then serialize (toCompactString (listOffsetsTopicName lmsg)) else serialize (listOffsetsTopicName lmsg)
    E.encodeVersionedArray version 6 encodeListOffsetsPartition (case P.unKafkaArray (listOffsetsTopicPartitions lmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ListOffsetsTopic with version-aware field handling.
decodeListOffsetsTopic :: MonadGet m => E.ApiVersion -> m ListOffsetsTopic
decodeListOffsetsTopic version =
  do
    fieldname <- if version >= 6 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeListOffsetsPartition
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ListOffsetsTopic
      {
      listOffsetsTopicName = fieldname
      ,
      listOffsetsTopicPartitions = fieldpartitions
      }



data ListOffsetsRequest = ListOffsetsRequest
  {

  -- | The broker ID of the requester, or -1 if this request is being made by a normal consumer.

  -- Versions: 0+
  listOffsetsRequestReplicaId :: !(Int32)
,

  -- | This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_lev

  -- Versions: 2+
  listOffsetsRequestIsolationLevel :: !(Int8)
,

  -- | Each topic in the request.

  -- Versions: 0+
  listOffsetsRequestTopics :: !(KafkaArray (ListOffsetsTopic))
,

  -- | The timeout to await a response in milliseconds for requests that require reading from remote storag

  -- Versions: 10+
  listOffsetsRequestTimeoutMs :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListOffsetsRequest.
maxListOffsetsRequestVersion :: Int16
maxListOffsetsRequestVersion = 10

-- | KafkaMessage instance for ListOffsetsRequest.
instance KafkaMessage ListOffsetsRequest where
  messageApiKey = 2
  messageMinVersion = 1
  messageMaxVersion = 10
  messageFlexibleVersion = Just 6

-- | Encode ListOffsetsRequest with the given API version.
encodeListOffsetsRequest :: MonadPut m => E.ApiVersion -> ListOffsetsRequest -> m ()
encodeListOffsetsRequest version msg
  | version == 1 =
    do
      serialize (listOffsetsRequestReplicaId msg)
      E.encodeVersionedArray version 6 encodeListOffsetsTopic (case P.unKafkaArray (listOffsetsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 10 =
    do
      serialize (listOffsetsRequestReplicaId msg)
      serialize (listOffsetsRequestIsolationLevel msg)
      E.encodeVersionedArray version 6 encodeListOffsetsTopic (case P.unKafkaArray (listOffsetsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (listOffsetsRequestTimeoutMs msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 2 && version <= 5 =
    do
      serialize (listOffsetsRequestReplicaId msg)
      serialize (listOffsetsRequestIsolationLevel msg)
      E.encodeVersionedArray version 6 encodeListOffsetsTopic (case P.unKafkaArray (listOffsetsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 6 && version <= 9 =
    do
      serialize (listOffsetsRequestReplicaId msg)
      serialize (listOffsetsRequestIsolationLevel msg)
      E.encodeVersionedArray version 6 encodeListOffsetsTopic (case P.unKafkaArray (listOffsetsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ListOffsetsRequest with the given API version.
decodeListOffsetsRequest :: MonadGet m => E.ApiVersion -> m ListOffsetsRequest
decodeListOffsetsRequest version
  | version == 1 =
    do
      fieldreplicaid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeListOffsetsTopic
      pure ListOffsetsRequest
        {
        listOffsetsRequestReplicaId = fieldreplicaid
        ,
        listOffsetsRequestIsolationLevel = 0
        ,
        listOffsetsRequestTopics = fieldtopics
        ,
        listOffsetsRequestTimeoutMs = 0
        }

  | version == 10 =
    do
      fieldreplicaid <- deserialize
      fieldisolationlevel <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeListOffsetsTopic
      fieldtimeoutms <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListOffsetsRequest
        {
        listOffsetsRequestReplicaId = fieldreplicaid
        ,
        listOffsetsRequestIsolationLevel = fieldisolationlevel
        ,
        listOffsetsRequestTopics = fieldtopics
        ,
        listOffsetsRequestTimeoutMs = fieldtimeoutms
        }

  | version >= 2 && version <= 5 =
    do
      fieldreplicaid <- deserialize
      fieldisolationlevel <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeListOffsetsTopic
      pure ListOffsetsRequest
        {
        listOffsetsRequestReplicaId = fieldreplicaid
        ,
        listOffsetsRequestIsolationLevel = fieldisolationlevel
        ,
        listOffsetsRequestTopics = fieldtopics
        ,
        listOffsetsRequestTimeoutMs = 0
        }

  | version >= 6 && version <= 9 =
    do
      fieldreplicaid <- deserialize
      fieldisolationlevel <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeListOffsetsTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListOffsetsRequest
        {
        listOffsetsRequestReplicaId = fieldreplicaid
        ,
        listOffsetsRequestIsolationLevel = fieldisolationlevel
        ,
        listOffsetsRequestTopics = fieldtopics
        ,
        listOffsetsRequestTimeoutMs = 0
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ListOffsetsRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
