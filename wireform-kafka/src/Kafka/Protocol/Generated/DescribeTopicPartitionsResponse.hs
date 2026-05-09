{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeTopicPartitionsResponse
Description : Kafka DescribeTopicPartitionsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 75.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeTopicPartitionsResponse
  (
    DescribeTopicPartitionsResponse(..),
    DescribeTopicPartitionsResponseTopic(..),
    DescribeTopicPartitionsResponsePartition(..),
    Cursor(..),
    encodeDescribeTopicPartitionsResponse,
    decodeDescribeTopicPartitionsResponse,
    maxDescribeTopicPartitionsResponseVersion
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


-- | Each partition in the topic.
data DescribeTopicPartitionsResponsePartition = DescribeTopicPartitionsResponsePartition
  {

  -- | The partition error, or 0 if there was no error.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionErrorCode :: !(Int16)
,

  -- | The partition index.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The ID of the leader broker.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionLeaderId :: !(Int32)
,

  -- | The leader epoch of this partition.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionLeaderEpoch :: !(Int32)
,

  -- | The set of all nodes that host this partition.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionReplicaNodes :: !(KafkaArray (Int32))
,

  -- | The set of nodes that are in sync with the leader for this partition.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionIsrNodes :: !(KafkaArray (Int32))
,

  -- | The new eligible leader replicas otherwise.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionEligibleLeaderReplicas :: !(KafkaArray (Int32))
,

  -- | The last known ELR.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionLastKnownElr :: !(KafkaArray (Int32))
,

  -- | The set of offline replicas of this partition.

  -- Versions: 0+
  describeTopicPartitionsResponsePartitionOfflineReplicas :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeTopicPartitionsResponsePartition with version-aware field handling.
encodeDescribeTopicPartitionsResponsePartition :: MonadPut m => E.ApiVersion -> DescribeTopicPartitionsResponsePartition -> m ()
encodeDescribeTopicPartitionsResponsePartition version dmsg =
  do
    serialize (describeTopicPartitionsResponsePartitionErrorCode dmsg)
    serialize (describeTopicPartitionsResponsePartitionPartitionIndex dmsg)
    serialize (describeTopicPartitionsResponsePartitionLeaderId dmsg)
    serialize (describeTopicPartitionsResponsePartitionLeaderEpoch dmsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (describeTopicPartitionsResponsePartitionReplicaNodes dmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (describeTopicPartitionsResponsePartitionIsrNodes dmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    E.encodeVersionedNullableArray version 0 (\_ x -> serialize x) (describeTopicPartitionsResponsePartitionEligibleLeaderReplicas dmsg) -- ArrayType: PrimitiveType "int32"
    E.encodeVersionedNullableArray version 0 (\_ x -> serialize x) (describeTopicPartitionsResponsePartitionLastKnownElr dmsg) -- ArrayType: PrimitiveType "int32"
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (describeTopicPartitionsResponsePartitionOfflineReplicas dmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeTopicPartitionsResponsePartition with version-aware field handling.
decodeDescribeTopicPartitionsResponsePartition :: MonadGet m => E.ApiVersion -> m DescribeTopicPartitionsResponsePartition
decodeDescribeTopicPartitionsResponsePartition version =
  do
    fielderrorcode <- deserialize
    fieldpartitionindex <- deserialize
    fieldleaderid <- deserialize
    fieldleaderepoch <- deserialize
    fieldreplicanodes <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    fieldisrnodes <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    fieldeligibleleaderreplicas <- E.decodeVersionedNullableArray version 0 (\_ -> deserialize)
    fieldlastknownelr <- E.decodeVersionedNullableArray version 0 (\_ -> deserialize)
    fieldofflinereplicas <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeTopicPartitionsResponsePartition
      {
      describeTopicPartitionsResponsePartitionErrorCode = fielderrorcode
      ,
      describeTopicPartitionsResponsePartitionPartitionIndex = fieldpartitionindex
      ,
      describeTopicPartitionsResponsePartitionLeaderId = fieldleaderid
      ,
      describeTopicPartitionsResponsePartitionLeaderEpoch = fieldleaderepoch
      ,
      describeTopicPartitionsResponsePartitionReplicaNodes = fieldreplicanodes
      ,
      describeTopicPartitionsResponsePartitionIsrNodes = fieldisrnodes
      ,
      describeTopicPartitionsResponsePartitionEligibleLeaderReplicas = fieldeligibleleaderreplicas
      ,
      describeTopicPartitionsResponsePartitionLastKnownElr = fieldlastknownelr
      ,
      describeTopicPartitionsResponsePartitionOfflineReplicas = fieldofflinereplicas
      }


-- | Each topic in the response.
data DescribeTopicPartitionsResponseTopic = DescribeTopicPartitionsResponseTopic
  {

  -- | The topic error, or 0 if there was no error.

  -- Versions: 0+
  describeTopicPartitionsResponseTopicErrorCode :: !(Int16)
,

  -- | The topic name.

  -- Versions: 0+
  describeTopicPartitionsResponseTopicName :: !(KafkaString)
,

  -- | The topic id.

  -- Versions: 0+
  describeTopicPartitionsResponseTopicTopicId :: !(KafkaUuid)
,

  -- | True if the topic is internal.

  -- Versions: 0+
  describeTopicPartitionsResponseTopicIsInternal :: !(Bool)
,

  -- | Each partition in the topic.

  -- Versions: 0+
  describeTopicPartitionsResponseTopicPartitions :: !(KafkaArray (DescribeTopicPartitionsResponsePartition))
,

  -- | 32-bit bitfield to represent authorized operations for this topic.

  -- Versions: 0+
  describeTopicPartitionsResponseTopicTopicAuthorizedOperations :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeTopicPartitionsResponseTopic with version-aware field handling.
encodeDescribeTopicPartitionsResponseTopic :: MonadPut m => E.ApiVersion -> DescribeTopicPartitionsResponseTopic -> m ()
encodeDescribeTopicPartitionsResponseTopic version dmsg =
  do
    serialize (describeTopicPartitionsResponseTopicErrorCode dmsg)
    if version >= 0 then serialize (toCompactString (describeTopicPartitionsResponseTopicName dmsg)) else serialize (describeTopicPartitionsResponseTopicName dmsg)
    serialize (describeTopicPartitionsResponseTopicTopicId dmsg)
    serialize (describeTopicPartitionsResponseTopicIsInternal dmsg)
    E.encodeVersionedArray version 0 encodeDescribeTopicPartitionsResponsePartition (case P.unKafkaArray (describeTopicPartitionsResponseTopicPartitions dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    serialize (describeTopicPartitionsResponseTopicTopicAuthorizedOperations dmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeTopicPartitionsResponseTopic with version-aware field handling.
decodeDescribeTopicPartitionsResponseTopic :: MonadGet m => E.ApiVersion -> m DescribeTopicPartitionsResponseTopic
decodeDescribeTopicPartitionsResponseTopic version =
  do
    fielderrorcode <- deserialize
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldtopicid <- deserialize
    fieldisinternal <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDescribeTopicPartitionsResponsePartition
    fieldtopicauthorizedoperations <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeTopicPartitionsResponseTopic
      {
      describeTopicPartitionsResponseTopicErrorCode = fielderrorcode
      ,
      describeTopicPartitionsResponseTopicName = fieldname
      ,
      describeTopicPartitionsResponseTopicTopicId = fieldtopicid
      ,
      describeTopicPartitionsResponseTopicIsInternal = fieldisinternal
      ,
      describeTopicPartitionsResponseTopicPartitions = fieldpartitions
      ,
      describeTopicPartitionsResponseTopicTopicAuthorizedOperations = fieldtopicauthorizedoperations
      }


-- | The next topic and partition index to fetch details for.
data Cursor = Cursor
  {

  -- | The name for the first topic to process.

  -- Versions: 0+
  cursorTopicName :: !(KafkaString)
,

  -- | The partition index to start with.

  -- Versions: 0+
  cursorPartitionIndex :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode Cursor with version-aware field handling.
encodeCursor :: MonadPut m => E.ApiVersion -> Cursor -> m ()
encodeCursor version cmsg =
  do
    if version >= 0 then serialize (toCompactString (cursorTopicName cmsg)) else serialize (cursorTopicName cmsg)
    serialize (cursorPartitionIndex cmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Cursor with version-aware field handling.
decodeCursor :: MonadGet m => E.ApiVersion -> m Cursor
decodeCursor version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitionindex <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Cursor
      {
      cursorTopicName = fieldtopicname
      ,
      cursorPartitionIndex = fieldpartitionindex
      }



data DescribeTopicPartitionsResponse = DescribeTopicPartitionsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeTopicPartitionsResponseThrottleTimeMs :: !(Int32)
,

  -- | Each topic in the response.

  -- Versions: 0+
  describeTopicPartitionsResponseTopics :: !(KafkaArray (DescribeTopicPartitionsResponseTopic))
,

  -- | The next topic and partition index to fetch details for.

  -- Versions: 0+
  describeTopicPartitionsResponseNextCursor :: !(Nullable (Cursor))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeTopicPartitionsResponse.
maxDescribeTopicPartitionsResponseVersion :: Int16
maxDescribeTopicPartitionsResponseVersion = 0

-- | Encode DescribeTopicPartitionsResponse with the given API version.
encodeDescribeTopicPartitionsResponse :: MonadPut m => E.ApiVersion -> DescribeTopicPartitionsResponse -> m ()
encodeDescribeTopicPartitionsResponse version msg
  | version == 0 =
    do
      serialize (describeTopicPartitionsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 0 encodeDescribeTopicPartitionsResponseTopic (case P.unKafkaArray (describeTopicPartitionsResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      case (describeTopicPartitionsResponseNextCursor msg) of { P.Null -> serialize (0 :: Int8); P.NotNull val -> do { serialize (1 :: Int8); encodeCursor version val } }
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeTopicPartitionsResponse with the given API version.
decodeDescribeTopicPartitionsResponse :: MonadGet m => E.ApiVersion -> m DescribeTopicPartitionsResponse
decodeDescribeTopicPartitionsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDescribeTopicPartitionsResponseTopic
      fieldnextcursor <- do { flag <- deserialize :: (MonadGet m) => m Int8; case flag of { 0 -> pure P.Null; 1 -> P.NotNull <$> decodeCursor version; _ -> fail "Invalid nullable flag" } }
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeTopicPartitionsResponse
        {
        describeTopicPartitionsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeTopicPartitionsResponseTopics = fieldtopics
        ,
        describeTopicPartitionsResponseNextCursor = fieldnextcursor
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeTopicPartitionsResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
