{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.FetchRequest
Description : Kafka FetchRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 1.



Valid versions: 4-18
Flexible versions: 12+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.FetchRequest
  (
    FetchRequest(..),
    ReplicaState(..),
    FetchTopic(..),
    FetchPartition(..),
    ForgottenTopic(..),
    encodeFetchRequest,
    decodeFetchRequest,
    maxFetchRequestVersion
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
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


-- | The state of the replica in the follower.
data ReplicaState = ReplicaState
  {

  -- | The replica ID of the follower, or -1 if this request is from a consumer.

  -- Versions: 15+
  replicaStateReplicaId :: !(Int32)
,

  -- | The epoch of this follower, or -1 if not available.

  -- Versions: 15+
  replicaStateReplicaEpoch :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode ReplicaState with version-aware field handling.
encodeReplicaState :: MonadPut m => E.ApiVersion -> ReplicaState -> m ()
encodeReplicaState version rmsg =
  do
    when (version >= 15) $
      serialize (replicaStateReplicaId rmsg)
    when (version >= 15) $
      serialize (replicaStateReplicaEpoch rmsg)
    when (version >= 12) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ReplicaState with version-aware field handling.
decodeReplicaState :: MonadGet m => E.ApiVersion -> m ReplicaState
decodeReplicaState version =
  do
    fieldreplicaid <- if version >= 15
      then deserialize
      else pure ((-1))
    fieldreplicaepoch <- if version >= 15
      then deserialize
      else pure ((-1))
    _ <- if version >= 12 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ReplicaState
      {
      replicaStateReplicaId = fieldreplicaid
      ,
      replicaStateReplicaEpoch = fieldreplicaepoch
      }


-- | The partitions to fetch.
data FetchPartition = FetchPartition
  {

  -- | The partition index.

  -- Versions: 0+
  fetchPartitionPartition :: !(Int32)
,

  -- | The current leader epoch of the partition.

  -- Versions: 9+
  fetchPartitionCurrentLeaderEpoch :: !(Int32)
,

  -- | The message offset.

  -- Versions: 0+
  fetchPartitionFetchOffset :: !(Int64)
,

  -- | The epoch of the last fetched record or -1 if there is none.

  -- Versions: 12+
  fetchPartitionLastFetchedEpoch :: !(Int32)
,

  -- | The earliest available offset of the follower replica.  The field is only used when the request is s

  -- Versions: 5+
  fetchPartitionLogStartOffset :: !(Int64)
,

  -- | The maximum bytes to fetch from this partition.  See KIP-74 for cases where this limit may not be ho

  -- Versions: 0+
  fetchPartitionPartitionMaxBytes :: !(Int32)
,

  -- | The directory id of the follower fetching.

  -- Versions: 17+
  fetchPartitionReplicaDirectoryId :: !(KafkaUuid)
,

  -- | The high-watermark known by the replica. -1 if the high-watermark is not known and 92233720368547758

  -- Versions: 18+
  fetchPartitionHighWatermark :: !(Int64)

  }
  deriving (Eq, Show, Generic)


-- | Encode FetchPartition with version-aware field handling.
encodeFetchPartition :: MonadPut m => E.ApiVersion -> FetchPartition -> m ()
encodeFetchPartition version fmsg =
  do
    serialize (fetchPartitionPartition fmsg)
    when (version >= 9) $
      serialize (fetchPartitionCurrentLeaderEpoch fmsg)
    serialize (fetchPartitionFetchOffset fmsg)
    when (version >= 12) $
      serialize (fetchPartitionLastFetchedEpoch fmsg)
    when (version >= 5) $
      serialize (fetchPartitionLogStartOffset fmsg)
    serialize (fetchPartitionPartitionMaxBytes fmsg)
    when (version >= 12) $ do
      let _entries = (if version >= 17 then [(0, Data.Bytes.Put.runPutS (serialize (fetchPartitionReplicaDirectoryId fmsg)))] else []) ++ (if version >= 18 then [(1, Data.Bytes.Put.runPutS (serialize (fetchPartitionHighWatermark fmsg)))] else [])
      P.serializeTaggedFieldEntries _entries


-- | Decode FetchPartition with version-aware field handling.
decodeFetchPartition :: MonadGet m => E.ApiVersion -> m FetchPartition
decodeFetchPartition version =
  do
    fieldpartition <- deserialize
    fieldcurrentleaderepoch <- if version >= 9
      then deserialize
      else pure ((-1))
    fieldfetchoffset <- deserialize
    fieldlastfetchedepoch <- if version >= 12
      then deserialize
      else pure ((-1))
    fieldlogstartoffset <- if version >= 5
      then deserialize
      else pure ((-1))
    fieldpartitionmaxbytes <- deserialize
    _taggedFields <- if version >= 12 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    let fieldreplicadirectoryid =
          if version >= 17
            then case P.lookupTaggedField 0 _taggedFields of
              Just _bs -> case Data.Bytes.Get.runGetS (deserialize) _bs of
                  Right _v -> _v
                  Left  _  -> (P.nullUuid)
              Nothing  -> (P.nullUuid)
            else (P.nullUuid)
    let fieldhighwatermark =
          if version >= 18
            then case P.lookupTaggedField 1 _taggedFields of
              Just _bs -> case Data.Bytes.Get.runGetS (deserialize) _bs of
                  Right _v -> _v
                  Left  _  -> (9223372036854775807)
              Nothing  -> (9223372036854775807)
            else (9223372036854775807)
    pure FetchPartition
      {
      fetchPartitionPartition = fieldpartition
      ,
      fetchPartitionCurrentLeaderEpoch = fieldcurrentleaderepoch
      ,
      fetchPartitionFetchOffset = fieldfetchoffset
      ,
      fetchPartitionLastFetchedEpoch = fieldlastfetchedepoch
      ,
      fetchPartitionLogStartOffset = fieldlogstartoffset
      ,
      fetchPartitionPartitionMaxBytes = fieldpartitionmaxbytes
      ,
      fetchPartitionReplicaDirectoryId = fieldreplicadirectoryid
      ,
      fetchPartitionHighWatermark = fieldhighwatermark
      }


-- | The topics to fetch.
data FetchTopic = FetchTopic
  {

  -- | The name of the topic to fetch.

  -- Versions: 0-12
  fetchTopicTopic :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 13+
  fetchTopicTopicId :: !(KafkaUuid)
,

  -- | The partitions to fetch.

  -- Versions: 0+
  fetchTopicPartitions :: !(KafkaArray (FetchPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode FetchTopic with version-aware field handling.
encodeFetchTopic :: MonadPut m => E.ApiVersion -> FetchTopic -> m ()
encodeFetchTopic version fmsg =
  do
    when (version >= 0 && version <= 12) $
      if version >= 12 then serialize (toCompactString (fetchTopicTopic fmsg)) else serialize (fetchTopicTopic fmsg)
    when (version >= 13) $
      serialize (fetchTopicTopicId fmsg)
    E.encodeVersionedArray version 12 encodeFetchPartition (case P.unKafkaArray (fetchTopicPartitions fmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 12) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode FetchTopic with version-aware field handling.
decodeFetchTopic :: MonadGet m => E.ApiVersion -> m FetchTopic
decodeFetchTopic version =
  do
    fieldtopic <- if version >= 0 && version <= 12
      then if version >= 12 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 13
      then deserialize
      else pure (P.nullUuid)
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeFetchPartition
    _ <- if version >= 12 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure FetchTopic
      {
      fetchTopicTopic = fieldtopic
      ,
      fetchTopicTopicId = fieldtopicid
      ,
      fetchTopicPartitions = fieldpartitions
      }


-- | In an incremental fetch request, the partitions to remove.
data ForgottenTopic = ForgottenTopic
  {

  -- | The topic name.

  -- Versions: 7-12
  forgottenTopicTopic :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 13+
  forgottenTopicTopicId :: !(KafkaUuid)
,

  -- | The partitions indexes to forget.

  -- Versions: 7+
  forgottenTopicPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode ForgottenTopic with version-aware field handling.
encodeForgottenTopic :: MonadPut m => E.ApiVersion -> ForgottenTopic -> m ()
encodeForgottenTopic version fmsg =
  do
    when (version >= 7 && version <= 12) $
      if version >= 12 then serialize (toCompactString (forgottenTopicTopic fmsg)) else serialize (forgottenTopicTopic fmsg)
    when (version >= 13) $
      serialize (forgottenTopicTopicId fmsg)
    when (version >= 7) $
      E.encodeVersionedArray version 12 (\_ x -> serialize x) (case P.unKafkaArray (forgottenTopicPartitions fmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 12) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ForgottenTopic with version-aware field handling.
decodeForgottenTopic :: MonadGet m => E.ApiVersion -> m ForgottenTopic
decodeForgottenTopic version =
  do
    fieldtopic <- if version >= 7 && version <= 12
      then if version >= 12 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 13
      then deserialize
      else pure (P.nullUuid)
    fieldpartitions <- if version >= 7
      then P.mkKafkaArray <$> E.decodeVersionedArray version 12 (\_ -> deserialize)
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 12 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ForgottenTopic
      {
      forgottenTopicTopic = fieldtopic
      ,
      forgottenTopicTopicId = fieldtopicid
      ,
      forgottenTopicPartitions = fieldpartitions
      }



data FetchRequest = FetchRequest
  {

  -- | The clusterId if known. This is used to validate metadata fetches prior to broker registration.

  -- Versions: 12+
  fetchRequestClusterId :: !(KafkaString)
,

  -- | The broker ID of the follower, of -1 if this request is from a consumer.

  -- Versions: 0-14
  fetchRequestReplicaId :: !(Int32)
,

  -- | The state of the replica in the follower.

  -- Versions: 15+
  fetchRequestReplicaState :: !(ReplicaState)
,

  -- | The maximum time in milliseconds to wait for the response.

  -- Versions: 0+
  fetchRequestMaxWaitMs :: !(Int32)
,

  -- | The minimum bytes to accumulate in the response.

  -- Versions: 0+
  fetchRequestMinBytes :: !(Int32)
,

  -- | The maximum bytes to fetch.  See KIP-74 for cases where this limit may not be honored.

  -- Versions: 3+
  fetchRequestMaxBytes :: !(Int32)
,

  -- | This setting controls the visibility of transactional records. Using READ_UNCOMMITTED (isolation_lev

  -- Versions: 4+
  fetchRequestIsolationLevel :: !(Int8)
,

  -- | The fetch session ID.

  -- Versions: 7+
  fetchRequestSessionId :: !(Int32)
,

  -- | The fetch session epoch, which is used for ordering requests in a session.

  -- Versions: 7+
  fetchRequestSessionEpoch :: !(Int32)
,

  -- | The topics to fetch.

  -- Versions: 0+
  fetchRequestTopics :: !(KafkaArray (FetchTopic))
,

  -- | In an incremental fetch request, the partitions to remove.

  -- Versions: 7+
  fetchRequestForgottenTopicsData :: !(KafkaArray (ForgottenTopic))
,

  -- | Rack ID of the consumer making this request.

  -- Versions: 11+
  fetchRequestRackId :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for FetchRequest.
maxFetchRequestVersion :: Int16
maxFetchRequestVersion = 18

-- | KafkaMessage instance for FetchRequest.
instance KafkaMessage FetchRequest where
  messageApiKey = 1
  messageMinVersion = 4
  messageMaxVersion = 18
  messageFlexibleVersion = Just 12

-- | Encode FetchRequest with the given API version.
encodeFetchRequest :: MonadPut m => E.ApiVersion -> FetchRequest -> m ()
encodeFetchRequest version msg
  | version == 11 =
    do
      serialize (fetchRequestReplicaId msg)
      serialize (fetchRequestMaxWaitMs msg)
      serialize (fetchRequestMinBytes msg)
      serialize (fetchRequestMaxBytes msg)
      serialize (fetchRequestIsolationLevel msg)
      serialize (fetchRequestSessionId msg)
      serialize (fetchRequestSessionEpoch msg)
      E.encodeVersionedArray version 12 encodeFetchTopic (case P.unKafkaArray (fetchRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 12 encodeForgottenTopic (case P.unKafkaArray (fetchRequestForgottenTopicsData msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (fetchRequestRackId msg)


  | version >= 4 && version <= 6 =
    do
      serialize (fetchRequestReplicaId msg)
      serialize (fetchRequestMaxWaitMs msg)
      serialize (fetchRequestMinBytes msg)
      serialize (fetchRequestMaxBytes msg)
      serialize (fetchRequestIsolationLevel msg)
      E.encodeVersionedArray version 12 encodeFetchTopic (case P.unKafkaArray (fetchRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 12 && version <= 14 =
    do
      serialize (fetchRequestReplicaId msg)
      serialize (fetchRequestMaxWaitMs msg)
      serialize (fetchRequestMinBytes msg)
      serialize (fetchRequestMaxBytes msg)
      serialize (fetchRequestIsolationLevel msg)
      serialize (fetchRequestSessionId msg)
      serialize (fetchRequestSessionEpoch msg)
      E.encodeVersionedArray version 12 encodeFetchTopic (case P.unKafkaArray (fetchRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 12 encodeForgottenTopic (case P.unKafkaArray (fetchRequestForgottenTopicsData msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (toCompactString (fetchRequestRackId msg))
      do
        let _entries = (if version >= 12 then [(0, Data.Bytes.Put.runPutS (serialize (toCompactString (fetchRequestClusterId msg))))] else []) ++ (if version >= 15 then [(1, Data.Bytes.Put.runPutS (encodeReplicaState version (fetchRequestReplicaState msg)))] else [])
        P.serializeTaggedFieldEntries _entries

  | version >= 7 && version <= 10 =
    do
      serialize (fetchRequestReplicaId msg)
      serialize (fetchRequestMaxWaitMs msg)
      serialize (fetchRequestMinBytes msg)
      serialize (fetchRequestMaxBytes msg)
      serialize (fetchRequestIsolationLevel msg)
      serialize (fetchRequestSessionId msg)
      serialize (fetchRequestSessionEpoch msg)
      E.encodeVersionedArray version 12 encodeFetchTopic (case P.unKafkaArray (fetchRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 12 encodeForgottenTopic (case P.unKafkaArray (fetchRequestForgottenTopicsData msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 15 && version <= 18 =
    do
      serialize (fetchRequestMaxWaitMs msg)
      serialize (fetchRequestMinBytes msg)
      serialize (fetchRequestMaxBytes msg)
      serialize (fetchRequestIsolationLevel msg)
      serialize (fetchRequestSessionId msg)
      serialize (fetchRequestSessionEpoch msg)
      E.encodeVersionedArray version 12 encodeFetchTopic (case P.unKafkaArray (fetchRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 12 encodeForgottenTopic (case P.unKafkaArray (fetchRequestForgottenTopicsData msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (toCompactString (fetchRequestRackId msg))
      do
        let _entries = (if version >= 12 then [(0, Data.Bytes.Put.runPutS (serialize (toCompactString (fetchRequestClusterId msg))))] else []) ++ (if version >= 15 then [(1, Data.Bytes.Put.runPutS (encodeReplicaState version (fetchRequestReplicaState msg)))] else [])
        P.serializeTaggedFieldEntries _entries
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode FetchRequest with the given API version.
decodeFetchRequest :: MonadGet m => E.ApiVersion -> m FetchRequest
decodeFetchRequest version
  | version == 11 =
    do
      fieldreplicaid <- deserialize
      fieldmaxwaitms <- deserialize
      fieldminbytes <- deserialize
      fieldmaxbytes <- deserialize
      fieldisolationlevel <- deserialize
      fieldsessionid <- deserialize
      fieldsessionepoch <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeFetchTopic
      fieldforgottentopicsdata <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeForgottenTopic
      fieldrackid <- deserialize
      pure FetchRequest
        {
        fetchRequestClusterId = P.KafkaString Null
        ,
        fetchRequestReplicaId = fieldreplicaid
        ,
        fetchRequestReplicaState = ReplicaState { replicaStateReplicaId = (-1), replicaStateReplicaEpoch = (-1) }
        ,
        fetchRequestMaxWaitMs = fieldmaxwaitms
        ,
        fetchRequestMinBytes = fieldminbytes
        ,
        fetchRequestMaxBytes = fieldmaxbytes
        ,
        fetchRequestIsolationLevel = fieldisolationlevel
        ,
        fetchRequestSessionId = fieldsessionid
        ,
        fetchRequestSessionEpoch = fieldsessionepoch
        ,
        fetchRequestTopics = fieldtopics
        ,
        fetchRequestForgottenTopicsData = fieldforgottentopicsdata
        ,
        fetchRequestRackId = fieldrackid
        }

  | version >= 4 && version <= 6 =
    do
      fieldreplicaid <- deserialize
      fieldmaxwaitms <- deserialize
      fieldminbytes <- deserialize
      fieldmaxbytes <- deserialize
      fieldisolationlevel <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeFetchTopic
      pure FetchRequest
        {
        fetchRequestClusterId = P.KafkaString Null
        ,
        fetchRequestReplicaId = fieldreplicaid
        ,
        fetchRequestReplicaState = ReplicaState { replicaStateReplicaId = (-1), replicaStateReplicaEpoch = (-1) }
        ,
        fetchRequestMaxWaitMs = fieldmaxwaitms
        ,
        fetchRequestMinBytes = fieldminbytes
        ,
        fetchRequestMaxBytes = fieldmaxbytes
        ,
        fetchRequestIsolationLevel = fieldisolationlevel
        ,
        fetchRequestSessionId = 0
        ,
        fetchRequestSessionEpoch = (-1)
        ,
        fetchRequestTopics = fieldtopics
        ,
        fetchRequestForgottenTopicsData = P.mkKafkaArray V.empty
        ,
        fetchRequestRackId = P.KafkaString Null
        }

  | version >= 12 && version <= 14 =
    do
      fieldreplicaid <- deserialize
      fieldmaxwaitms <- deserialize
      fieldminbytes <- deserialize
      fieldmaxbytes <- deserialize
      fieldisolationlevel <- deserialize
      fieldsessionid <- deserialize
      fieldsessionepoch <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeFetchTopic
      fieldforgottentopicsdata <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeForgottenTopic
      fieldrackid <- if version >= 12 then P.fromCompactString <$> deserialize else deserialize
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldclusterid =
            if version >= 12
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (P.fromCompactString <$> deserialize) _bs of
                    Right _v -> _v
                    Left  _  -> (P.KafkaString Null)
                Nothing  -> (P.KafkaString Null)
              else (P.KafkaString Null)
      let fieldreplicastate =
            if version >= 15
              then case P.lookupTaggedField 1 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (decodeReplicaState version) _bs of
                    Right _v -> _v
                    Left  _  -> (ReplicaState { replicaStateReplicaId = (-1), replicaStateReplicaEpoch = (-1) })
                Nothing  -> (ReplicaState { replicaStateReplicaId = (-1), replicaStateReplicaEpoch = (-1) })
              else (ReplicaState { replicaStateReplicaId = (-1), replicaStateReplicaEpoch = (-1) })
      pure FetchRequest
        {
        fetchRequestClusterId = fieldclusterid
        ,
        fetchRequestReplicaId = fieldreplicaid
        ,
        fetchRequestReplicaState = fieldreplicastate
        ,
        fetchRequestMaxWaitMs = fieldmaxwaitms
        ,
        fetchRequestMinBytes = fieldminbytes
        ,
        fetchRequestMaxBytes = fieldmaxbytes
        ,
        fetchRequestIsolationLevel = fieldisolationlevel
        ,
        fetchRequestSessionId = fieldsessionid
        ,
        fetchRequestSessionEpoch = fieldsessionepoch
        ,
        fetchRequestTopics = fieldtopics
        ,
        fetchRequestForgottenTopicsData = fieldforgottentopicsdata
        ,
        fetchRequestRackId = fieldrackid
        }

  | version >= 7 && version <= 10 =
    do
      fieldreplicaid <- deserialize
      fieldmaxwaitms <- deserialize
      fieldminbytes <- deserialize
      fieldmaxbytes <- deserialize
      fieldisolationlevel <- deserialize
      fieldsessionid <- deserialize
      fieldsessionepoch <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeFetchTopic
      fieldforgottentopicsdata <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeForgottenTopic
      pure FetchRequest
        {
        fetchRequestClusterId = P.KafkaString Null
        ,
        fetchRequestReplicaId = fieldreplicaid
        ,
        fetchRequestReplicaState = ReplicaState { replicaStateReplicaId = (-1), replicaStateReplicaEpoch = (-1) }
        ,
        fetchRequestMaxWaitMs = fieldmaxwaitms
        ,
        fetchRequestMinBytes = fieldminbytes
        ,
        fetchRequestMaxBytes = fieldmaxbytes
        ,
        fetchRequestIsolationLevel = fieldisolationlevel
        ,
        fetchRequestSessionId = fieldsessionid
        ,
        fetchRequestSessionEpoch = fieldsessionepoch
        ,
        fetchRequestTopics = fieldtopics
        ,
        fetchRequestForgottenTopicsData = fieldforgottentopicsdata
        ,
        fetchRequestRackId = P.KafkaString Null
        }

  | version >= 15 && version <= 18 =
    do
      fieldmaxwaitms <- deserialize
      fieldminbytes <- deserialize
      fieldmaxbytes <- deserialize
      fieldisolationlevel <- deserialize
      fieldsessionid <- deserialize
      fieldsessionepoch <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeFetchTopic
      fieldforgottentopicsdata <- P.mkKafkaArray <$> E.decodeVersionedArray version 12 decodeForgottenTopic
      fieldrackid <- if version >= 12 then P.fromCompactString <$> deserialize else deserialize
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldclusterid =
            if version >= 12
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (P.fromCompactString <$> deserialize) _bs of
                    Right _v -> _v
                    Left  _  -> (P.KafkaString Null)
                Nothing  -> (P.KafkaString Null)
              else (P.KafkaString Null)
      let fieldreplicastate =
            if version >= 15
              then case P.lookupTaggedField 1 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (decodeReplicaState version) _bs of
                    Right _v -> _v
                    Left  _  -> (ReplicaState { replicaStateReplicaId = (-1), replicaStateReplicaEpoch = (-1) })
                Nothing  -> (ReplicaState { replicaStateReplicaId = (-1), replicaStateReplicaEpoch = (-1) })
              else (ReplicaState { replicaStateReplicaId = (-1), replicaStateReplicaEpoch = (-1) })
      pure FetchRequest
        {
        fetchRequestClusterId = fieldclusterid
        ,
        fetchRequestReplicaId = (-1)
        ,
        fetchRequestReplicaState = fieldreplicastate
        ,
        fetchRequestMaxWaitMs = fieldmaxwaitms
        ,
        fetchRequestMinBytes = fieldminbytes
        ,
        fetchRequestMaxBytes = fieldmaxbytes
        ,
        fetchRequestIsolationLevel = fieldisolationlevel
        ,
        fetchRequestSessionId = fieldsessionid
        ,
        fetchRequestSessionEpoch = fieldsessionepoch
        ,
        fetchRequestTopics = fieldtopics
        ,
        fetchRequestForgottenTopicsData = fieldforgottentopicsdata
        ,
        fetchRequestRackId = fieldrackid
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a ReplicaState.
wireMaxSizeReplicaState :: Int -> ReplicaState -> Int
wireMaxSizeReplicaState _version msg =
  0
  + 4
  + 8
  + 1

-- | Direct-poke encoder for ReplicaState.
wirePokeReplicaState :: Int -> Ptr Word8 -> ReplicaState -> IO (Ptr Word8)
wirePokeReplicaState version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (replicaStateReplicaId msg)
  p2 <- W.pokeInt64BE p1 (replicaStateReplicaEpoch msg)
  if version >= 12 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ReplicaState.
wirePeekReplicaState :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ReplicaState, Ptr Word8)
wirePeekReplicaState version _fp _basePtr p0 endPtr = do
  (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
  (f1_replicaepoch, p2) <- W.peekInt64BE p1 endPtr
  pTagsEnd <- if version >= 12 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ReplicaState { replicaStateReplicaId = f0_replicaid, replicaStateReplicaEpoch = f1_replicaepoch }, pTagsEnd)

-- | Worst-case wire size of a ForgottenTopic.
wireMaxSizeForgottenTopic :: Int -> ForgottenTopic -> Int
wireMaxSizeForgottenTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (forgottenTopicTopic msg))
  + 16
  + (5 + (case P.unKafkaArray (forgottenTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ForgottenTopic.
wirePokeForgottenTopic :: Int -> Ptr Word8 -> ForgottenTopic -> IO (Ptr Word8)
wirePokeForgottenTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (forgottenTopicTopic msg))
  p2 <- WP.pokeKafkaUuid p1 (forgottenTopicTopicId msg)
  p3 <- WP.pokeVersionedArray version 12 W.pokeInt32BE p2 (forgottenTopicPartitions msg)
  if version >= 12 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for ForgottenTopic.
wirePeekForgottenTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ForgottenTopic, Ptr Word8)
wirePeekForgottenTopic version _fp _basePtr p0 endPtr = do
  (f0_topic, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_topicid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_partitions, p3) <- WP.peekVersionedArray version 12 W.peekInt32BE p2 endPtr
  pTagsEnd <- if version >= 12 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (ForgottenTopic { forgottenTopicTopic = f0_topic, forgottenTopicTopicId = f1_topicid, forgottenTopicPartitions = f2_partitions }, pTagsEnd)

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries tagged fields with payloads — KIP-866
-- style — that the generator hasn't been taught yet), so
-- we lift the legacy 'encodeFetchRequest' / 'decodeFetchRequest'
-- pair into a 'WireCodecImpl' via 'WC.serialShimCodec'.
-- The dispatch shape is identical to the native case —
-- every 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through
-- a 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec FetchRequest where
  wireCodec = Just (WC.serialShimCodec encodeFetchRequest decodeFetchRequest)
  {-# INLINE wireCodec #-}