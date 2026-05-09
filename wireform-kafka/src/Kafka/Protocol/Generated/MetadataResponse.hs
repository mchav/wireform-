{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.MetadataResponse
Description : Kafka MetadataResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 3.



Valid versions: 0-13
Flexible versions: 9+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.MetadataResponse
  (
    MetadataResponse(..),
    MetadataResponseBroker(..),
    MetadataResponseTopic(..),
    MetadataResponsePartition(..),
    encodeMetadataResponse,
    decodeMetadataResponse,
    maxMetadataResponseVersion
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
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


-- | A list of brokers present in the cluster.
data MetadataResponseBroker = MetadataResponseBroker
  {

  -- | The broker ID.

  -- Versions: 0+
  metadataResponseBrokerNodeId :: !(Int32)
,

  -- | The broker hostname.

  -- Versions: 0+
  metadataResponseBrokerHost :: !(KafkaString)
,

  -- | The broker port.

  -- Versions: 0+
  metadataResponseBrokerPort :: !(Int32)
,

  -- | The rack of the broker, or null if it has not been assigned to a rack.

  -- Versions: 1+
  metadataResponseBrokerRack :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode MetadataResponseBroker with version-aware field handling.
encodeMetadataResponseBroker :: MonadPut m => E.ApiVersion -> MetadataResponseBroker -> m ()
encodeMetadataResponseBroker version mmsg =
  do
    serialize (metadataResponseBrokerNodeId mmsg)
    if version >= 9 then serialize (toCompactString (metadataResponseBrokerHost mmsg)) else serialize (metadataResponseBrokerHost mmsg)
    serialize (metadataResponseBrokerPort mmsg)
    when (version >= 1) $
      if version >= 9 then serialize (toCompactString (metadataResponseBrokerRack mmsg)) else serialize (metadataResponseBrokerRack mmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode MetadataResponseBroker with version-aware field handling.
decodeMetadataResponseBroker :: MonadGet m => E.ApiVersion -> m MetadataResponseBroker
decodeMetadataResponseBroker version =
  do
    fieldnodeid <- deserialize
    fieldhost <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
    fieldport <- deserialize
    fieldrack <- if version >= 1
      then if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure MetadataResponseBroker
      {
      metadataResponseBrokerNodeId = fieldnodeid
      ,
      metadataResponseBrokerHost = fieldhost
      ,
      metadataResponseBrokerPort = fieldport
      ,
      metadataResponseBrokerRack = fieldrack
      }


-- | Each partition in the topic.
data MetadataResponsePartition = MetadataResponsePartition
  {

  -- | The partition error, or 0 if there was no error.

  -- Versions: 0+
  metadataResponsePartitionErrorCode :: !(Int16)
,

  -- | The partition index.

  -- Versions: 0+
  metadataResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The ID of the leader broker.

  -- Versions: 0+
  metadataResponsePartitionLeaderId :: !(Int32)
,

  -- | The leader epoch of this partition.

  -- Versions: 7+
  metadataResponsePartitionLeaderEpoch :: !(Int32)
,

  -- | The set of all nodes that host this partition.

  -- Versions: 0+
  metadataResponsePartitionReplicaNodes :: !(KafkaArray (Int32))
,

  -- | The set of nodes that are in sync with the leader for this partition.

  -- Versions: 0+
  metadataResponsePartitionIsrNodes :: !(KafkaArray (Int32))
,

  -- | The set of offline replicas of this partition.

  -- Versions: 5+
  metadataResponsePartitionOfflineReplicas :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode MetadataResponsePartition with version-aware field handling.
encodeMetadataResponsePartition :: MonadPut m => E.ApiVersion -> MetadataResponsePartition -> m ()
encodeMetadataResponsePartition version mmsg =
  do
    serialize (metadataResponsePartitionErrorCode mmsg)
    serialize (metadataResponsePartitionPartitionIndex mmsg)
    serialize (metadataResponsePartitionLeaderId mmsg)
    when (version >= 7) $
      serialize (metadataResponsePartitionLeaderEpoch mmsg)
    E.encodeVersionedArray version 9 (\_ x -> serialize x) (case P.unKafkaArray (metadataResponsePartitionReplicaNodes mmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    E.encodeVersionedArray version 9 (\_ x -> serialize x) (case P.unKafkaArray (metadataResponsePartitionIsrNodes mmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 5) $
      E.encodeVersionedArray version 9 (\_ x -> serialize x) (case P.unKafkaArray (metadataResponsePartitionOfflineReplicas mmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode MetadataResponsePartition with version-aware field handling.
decodeMetadataResponsePartition :: MonadGet m => E.ApiVersion -> m MetadataResponsePartition
decodeMetadataResponsePartition version =
  do
    fielderrorcode <- deserialize
    fieldpartitionindex <- deserialize
    fieldleaderid <- deserialize
    fieldleaderepoch <- if version >= 7
      then deserialize
      else pure ((-1))
    fieldreplicanodes <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 (\_ -> deserialize)
    fieldisrnodes <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 (\_ -> deserialize)
    fieldofflinereplicas <- if version >= 5
      then P.mkKafkaArray <$> E.decodeVersionedArray version 9 (\_ -> deserialize)
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure MetadataResponsePartition
      {
      metadataResponsePartitionErrorCode = fielderrorcode
      ,
      metadataResponsePartitionPartitionIndex = fieldpartitionindex
      ,
      metadataResponsePartitionLeaderId = fieldleaderid
      ,
      metadataResponsePartitionLeaderEpoch = fieldleaderepoch
      ,
      metadataResponsePartitionReplicaNodes = fieldreplicanodes
      ,
      metadataResponsePartitionIsrNodes = fieldisrnodes
      ,
      metadataResponsePartitionOfflineReplicas = fieldofflinereplicas
      }


-- | Each topic in the response.
data MetadataResponseTopic = MetadataResponseTopic
  {

  -- | The topic error, or 0 if there was no error.

  -- Versions: 0+
  metadataResponseTopicErrorCode :: !(Int16)
,

  -- | The topic name. Null for non-existing topics queried by ID. This is never null when ErrorCode is zer

  -- Versions: 0+
  metadataResponseTopicName :: !(KafkaString)
,

  -- | The topic id. Zero for non-existing topics queried by name. This is never zero when ErrorCode is zer

  -- Versions: 10+
  metadataResponseTopicTopicId :: !(KafkaUuid)
,

  -- | True if the topic is internal.

  -- Versions: 1+
  metadataResponseTopicIsInternal :: !(Bool)
,

  -- | Each partition in the topic.

  -- Versions: 0+
  metadataResponseTopicPartitions :: !(KafkaArray (MetadataResponsePartition))
,

  -- | 32-bit bitfield to represent authorized operations for this topic.

  -- Versions: 8+
  metadataResponseTopicTopicAuthorizedOperations :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode MetadataResponseTopic with version-aware field handling.
encodeMetadataResponseTopic :: MonadPut m => E.ApiVersion -> MetadataResponseTopic -> m ()
encodeMetadataResponseTopic version mmsg =
  do
    serialize (metadataResponseTopicErrorCode mmsg)
    if version >= 9 then serialize (toCompactString (metadataResponseTopicName mmsg)) else serialize (metadataResponseTopicName mmsg)
    when (version >= 10) $
      serialize (metadataResponseTopicTopicId mmsg)
    when (version >= 1) $
      serialize (metadataResponseTopicIsInternal mmsg)
    E.encodeVersionedArray version 9 encodeMetadataResponsePartition (case P.unKafkaArray (metadataResponseTopicPartitions mmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 8) $
      serialize (metadataResponseTopicTopicAuthorizedOperations mmsg)
    when (version >= 9) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode MetadataResponseTopic with version-aware field handling.
decodeMetadataResponseTopic :: MonadGet m => E.ApiVersion -> m MetadataResponseTopic
decodeMetadataResponseTopic version =
  do
    fielderrorcode <- deserialize
    fieldname <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
    fieldtopicid <- if version >= 10
      then deserialize
      else pure (P.nullUuid)
    fieldisinternal <- if version >= 1
      then deserialize
      else pure (False)
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponsePartition
    fieldtopicauthorizedoperations <- if version >= 8
      then deserialize
      else pure ((-2147483648))
    _ <- if version >= 9 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure MetadataResponseTopic
      {
      metadataResponseTopicErrorCode = fielderrorcode
      ,
      metadataResponseTopicName = fieldname
      ,
      metadataResponseTopicTopicId = fieldtopicid
      ,
      metadataResponseTopicIsInternal = fieldisinternal
      ,
      metadataResponseTopicPartitions = fieldpartitions
      ,
      metadataResponseTopicTopicAuthorizedOperations = fieldtopicauthorizedoperations
      }



data MetadataResponse = MetadataResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 3+
  metadataResponseThrottleTimeMs :: !(Int32)
,

  -- | A list of brokers present in the cluster.

  -- Versions: 0+
  metadataResponseBrokers :: !(KafkaArray (MetadataResponseBroker))
,

  -- | The cluster ID that responding broker belongs to.

  -- Versions: 2+
  metadataResponseClusterId :: !(KafkaString)
,

  -- | The ID of the controller broker.

  -- Versions: 1+
  metadataResponseControllerId :: !(Int32)
,

  -- | Each topic in the response.

  -- Versions: 0+
  metadataResponseTopics :: !(KafkaArray (MetadataResponseTopic))
,

  -- | 32-bit bitfield to represent authorized operations for this cluster.

  -- Versions: 8-10
  metadataResponseClusterAuthorizedOperations :: !(Int32)
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 13+
  metadataResponseErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for MetadataResponse.
maxMetadataResponseVersion :: Int16
maxMetadataResponseVersion = 13

-- | KafkaMessage instance for MetadataResponse.
instance KafkaMessage MetadataResponse where
  messageApiKey = 3
  messageMinVersion = 0
  messageMaxVersion = 13
  messageFlexibleVersion = Just 9

-- | Encode MetadataResponse with the given API version.
encodeMetadataResponse :: MonadPut m => E.ApiVersion -> MetadataResponse -> m ()
encodeMetadataResponse version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 1 =
    do
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 2 =
    do
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseClusterId msg)
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 8 =
    do
      serialize (metadataResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseClusterId msg)
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseClusterAuthorizedOperations msg)


  | version == 13 =
    do
      serialize (metadataResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (toCompactString (metadataResponseClusterId msg))
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseErrorCode msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 9 && version <= 10 =
    do
      serialize (metadataResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (toCompactString (metadataResponseClusterId msg))
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseClusterAuthorizedOperations msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 11 && version <= 12 =
    do
      serialize (metadataResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (toCompactString (metadataResponseClusterId msg))
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 3 && version <= 7 =
    do
      serialize (metadataResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseBroker (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (metadataResponseClusterId msg)
      serialize (metadataResponseControllerId msg)
      E.encodeVersionedArray version 9 encodeMetadataResponseTopic (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode MetadataResponse with the given API version.
decodeMetadataResponse :: MonadGet m => E.ApiVersion -> m MetadataResponse
decodeMetadataResponse version
  | version == 0 =
    do
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = 0
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = P.KafkaString Null
        ,
        metadataResponseControllerId = (-1)
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = (-2147483648)
        ,
        metadataResponseErrorCode = 0
        }

  | version == 1 =
    do
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = 0
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = P.KafkaString Null
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = (-2147483648)
        ,
        metadataResponseErrorCode = 0
        }

  | version == 2 =
    do
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldclusterid <- deserialize
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = 0
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = fieldclusterid
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = (-2147483648)
        ,
        metadataResponseErrorCode = 0
        }

  | version == 8 =
    do
      fieldthrottletimems <- deserialize
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldclusterid <- deserialize
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      fieldclusterauthorizedoperations <- deserialize
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = fieldthrottletimems
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = fieldclusterid
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = fieldclusterauthorizedoperations
        ,
        metadataResponseErrorCode = 0
        }

  | version == 13 =
    do
      fieldthrottletimems <- deserialize
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldclusterid <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      fielderrorcode <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = fieldthrottletimems
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = fieldclusterid
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = (-2147483648)
        ,
        metadataResponseErrorCode = fielderrorcode
        }

  | version >= 9 && version <= 10 =
    do
      fieldthrottletimems <- deserialize
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldclusterid <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      fieldclusterauthorizedoperations <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = fieldthrottletimems
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = fieldclusterid
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = fieldclusterauthorizedoperations
        ,
        metadataResponseErrorCode = 0
        }

  | version >= 11 && version <= 12 =
    do
      fieldthrottletimems <- deserialize
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldclusterid <- if version >= 9 then P.fromCompactString <$> deserialize else deserialize
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = fieldthrottletimems
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = fieldclusterid
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = (-2147483648)
        ,
        metadataResponseErrorCode = 0
        }

  | version >= 3 && version <= 7 =
    do
      fieldthrottletimems <- deserialize
      fieldbrokers <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseBroker
      fieldclusterid <- deserialize
      fieldcontrollerid <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 9 decodeMetadataResponseTopic
      pure MetadataResponse
        {
        metadataResponseThrottleTimeMs = fieldthrottletimems
        ,
        metadataResponseBrokers = fieldbrokers
        ,
        metadataResponseClusterId = fieldclusterid
        ,
        metadataResponseControllerId = fieldcontrollerid
        ,
        metadataResponseTopics = fieldtopics
        ,
        metadataResponseClusterAuthorizedOperations = (-2147483648)
        ,
        metadataResponseErrorCode = 0
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a MetadataResponseBroker.
wireMaxSizeMetadataResponseBroker :: Int -> MetadataResponseBroker -> Int
wireMaxSizeMetadataResponseBroker _version msg =
  0
  + 4
  + WP.compactStringMaxSize (P.toCompactString (metadataResponseBrokerHost msg))
  + 4
  + WP.compactStringMaxSize (P.toCompactString (metadataResponseBrokerRack msg))
  + 1

-- | Direct-poke encoder for MetadataResponseBroker.
wirePokeMetadataResponseBroker :: Int -> Ptr Word8 -> MetadataResponseBroker -> IO (Ptr Word8)
wirePokeMetadataResponseBroker version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (metadataResponseBrokerNodeId msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (metadataResponseBrokerHost msg))
  p3 <- W.pokeInt32BE p2 (metadataResponseBrokerPort msg)
  p4 <- WP.pokeCompactString p3 (P.toCompactString (metadataResponseBrokerRack msg))
  if version >= 9 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for MetadataResponseBroker.
wirePeekMetadataResponseBroker :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (MetadataResponseBroker, Ptr Word8)
wirePeekMetadataResponseBroker version _fp _basePtr p0 endPtr = do
  (f0_nodeid, p1) <- W.peekInt32BE p0 endPtr
  (f1_host, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_port, p3) <- W.peekInt32BE p2 endPtr
  (f3_rack, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  pTagsEnd <- if version >= 9 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (MetadataResponseBroker { metadataResponseBrokerNodeId = f0_nodeid, metadataResponseBrokerHost = f1_host, metadataResponseBrokerPort = f2_port, metadataResponseBrokerRack = f3_rack }, pTagsEnd)

-- | Worst-case wire size of a MetadataResponsePartition.
wireMaxSizeMetadataResponsePartition :: Int -> MetadataResponsePartition -> Int
wireMaxSizeMetadataResponsePartition _version msg =
  0
  + 2
  + 4
  + 4
  + 4
  + (5 + (case P.unKafkaArray (metadataResponsePartitionReplicaNodes msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (metadataResponsePartitionIsrNodes msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (metadataResponsePartitionOfflineReplicas msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for MetadataResponsePartition.
wirePokeMetadataResponsePartition :: Int -> Ptr Word8 -> MetadataResponsePartition -> IO (Ptr Word8)
wirePokeMetadataResponsePartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (metadataResponsePartitionErrorCode msg)
  p2 <- W.pokeInt32BE p1 (metadataResponsePartitionPartitionIndex msg)
  p3 <- W.pokeInt32BE p2 (metadataResponsePartitionLeaderId msg)
  p4 <- W.pokeInt32BE p3 (metadataResponsePartitionLeaderEpoch msg)
  p5 <- WP.pokeVersionedArray version 9 W.pokeInt32BE p4 (metadataResponsePartitionReplicaNodes msg)
  p6 <- WP.pokeVersionedArray version 9 W.pokeInt32BE p5 (metadataResponsePartitionIsrNodes msg)
  p7 <- WP.pokeVersionedArray version 9 W.pokeInt32BE p6 (metadataResponsePartitionOfflineReplicas msg)
  if version >= 9 then WP.pokeEmptyTaggedFields p7 else pure p7

-- | Direct-poke decoder for MetadataResponsePartition.
wirePeekMetadataResponsePartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (MetadataResponsePartition, Ptr Word8)
wirePeekMetadataResponsePartition version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_partitionindex, p2) <- W.peekInt32BE p1 endPtr
  (f2_leaderid, p3) <- W.peekInt32BE p2 endPtr
  (f3_leaderepoch, p4) <- W.peekInt32BE p3 endPtr
  (f4_replicanodes, p5) <- WP.peekVersionedArray version 9 W.peekInt32BE p4 endPtr
  (f5_isrnodes, p6) <- WP.peekVersionedArray version 9 W.peekInt32BE p5 endPtr
  (f6_offlinereplicas, p7) <- WP.peekVersionedArray version 9 W.peekInt32BE p6 endPtr
  pTagsEnd <- if version >= 9 then WP.peekAndSkipTaggedFields p7 endPtr else pure p7
  pure (MetadataResponsePartition { metadataResponsePartitionErrorCode = f0_errorcode, metadataResponsePartitionPartitionIndex = f1_partitionindex, metadataResponsePartitionLeaderId = f2_leaderid, metadataResponsePartitionLeaderEpoch = f3_leaderepoch, metadataResponsePartitionReplicaNodes = f4_replicanodes, metadataResponsePartitionIsrNodes = f5_isrnodes, metadataResponsePartitionOfflineReplicas = f6_offlinereplicas }, pTagsEnd)

-- | Worst-case wire size of a MetadataResponseTopic.
wireMaxSizeMetadataResponseTopic :: Int -> MetadataResponseTopic -> Int
wireMaxSizeMetadataResponseTopic _version msg =
  0
  + 2
  + WP.compactStringMaxSize (P.toCompactString (metadataResponseTopicName msg))
  + 16
  + 1
  + (5 + (case P.unKafkaArray (metadataResponseTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeMetadataResponsePartition _version x ) v); P.Null -> 0 }))
  + 4
  + 1

-- | Direct-poke encoder for MetadataResponseTopic.
wirePokeMetadataResponseTopic :: Int -> Ptr Word8 -> MetadataResponseTopic -> IO (Ptr Word8)
wirePokeMetadataResponseTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (metadataResponseTopicErrorCode msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (metadataResponseTopicName msg))
  p3 <- WP.pokeKafkaUuid p2 (metadataResponseTopicTopicId msg)
  p4 <- W.pokeWord8 p3 (if (metadataResponseTopicIsInternal msg) then 1 else 0)
  p5 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponsePartition version p x) p4 (metadataResponseTopicPartitions msg)
  p6 <- W.pokeInt32BE p5 (metadataResponseTopicTopicAuthorizedOperations msg)
  if version >= 9 then WP.pokeEmptyTaggedFields p6 else pure p6

-- | Direct-poke decoder for MetadataResponseTopic.
wirePeekMetadataResponseTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (MetadataResponseTopic, Ptr Word8)
wirePeekMetadataResponseTopic version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_name, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_topicid, p3) <- WP.peekKafkaUuid p2 endPtr
  (f3_isinternal, p4) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr
  (f4_partitions, p5) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponsePartition version _fp _basePtr p e) p4 endPtr
  (f5_topicauthorizedoperations, p6) <- W.peekInt32BE p5 endPtr
  pTagsEnd <- if version >= 9 then WP.peekAndSkipTaggedFields p6 endPtr else pure p6
  pure (MetadataResponseTopic { metadataResponseTopicErrorCode = f0_errorcode, metadataResponseTopicName = f1_name, metadataResponseTopicTopicId = f2_topicid, metadataResponseTopicIsInternal = f3_isinternal, metadataResponseTopicPartitions = f4_partitions, metadataResponseTopicTopicAuthorizedOperations = f5_topicauthorizedoperations }, pTagsEnd)

-- | Worst-case wire size of a MetadataResponse.
wireMaxSizeMetadataResponse :: Int -> MetadataResponse -> Int
wireMaxSizeMetadataResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (metadataResponseBrokers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeMetadataResponseBroker _version x ) v); P.Null -> 0 }))
  + WP.compactStringMaxSize (P.toCompactString (metadataResponseClusterId msg))
  + 4
  + (5 + (case P.unKafkaArray (metadataResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeMetadataResponseTopic _version x ) v); P.Null -> 0 }))
  + 4
  + 2
  + 1

-- | Direct-poke encoder for MetadataResponse.
wirePokeMetadataResponse :: Int -> Ptr Word8 -> MetadataResponse -> IO (Ptr Word8)
wirePokeMetadataResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseBroker version p x) p0 (metadataResponseBrokers msg)
    p2 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseTopic version p x) p1 (metadataResponseTopics msg)
    pure p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseBroker version p x) p0 (metadataResponseBrokers msg)
    p2 <- W.pokeInt32BE p1 (metadataResponseControllerId msg)
    p3 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseTopic version p x) p2 (metadataResponseTopics msg)
    pure p3
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseBroker version p x) p0 (metadataResponseBrokers msg)
    p2 <- WP.pokeCompactString p1 (P.toCompactString (metadataResponseClusterId msg))
    p3 <- W.pokeInt32BE p2 (metadataResponseControllerId msg)
    p4 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseTopic version p x) p3 (metadataResponseTopics msg)
    pure p4
  | version == 8 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (metadataResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseBroker version p x) p1 (metadataResponseBrokers msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (metadataResponseClusterId msg))
    p4 <- W.pokeInt32BE p3 (metadataResponseControllerId msg)
    p5 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseTopic version p x) p4 (metadataResponseTopics msg)
    p6 <- W.pokeInt32BE p5 (metadataResponseClusterAuthorizedOperations msg)
    pure p6
  | version == 13 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (metadataResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseBroker version p x) p1 (metadataResponseBrokers msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (metadataResponseClusterId msg))
    p4 <- W.pokeInt32BE p3 (metadataResponseControllerId msg)
    p5 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseTopic version p x) p4 (metadataResponseTopics msg)
    p6 <- W.pokeInt16BE p5 (metadataResponseErrorCode msg)
    WP.pokeEmptyTaggedFields p6
  | version >= 9 && version <= 10 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (metadataResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseBroker version p x) p1 (metadataResponseBrokers msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (metadataResponseClusterId msg))
    p4 <- W.pokeInt32BE p3 (metadataResponseControllerId msg)
    p5 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseTopic version p x) p4 (metadataResponseTopics msg)
    p6 <- W.pokeInt32BE p5 (metadataResponseClusterAuthorizedOperations msg)
    WP.pokeEmptyTaggedFields p6
  | version >= 11 && version <= 12 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (metadataResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseBroker version p x) p1 (metadataResponseBrokers msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (metadataResponseClusterId msg))
    p4 <- W.pokeInt32BE p3 (metadataResponseControllerId msg)
    p5 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseTopic version p x) p4 (metadataResponseTopics msg)
    WP.pokeEmptyTaggedFields p5
  | version >= 3 && version <= 7 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (metadataResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseBroker version p x) p1 (metadataResponseBrokers msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (metadataResponseClusterId msg))
    p4 <- W.pokeInt32BE p3 (metadataResponseControllerId msg)
    p5 <- WP.pokeVersionedArray version 9 (\p x -> wirePokeMetadataResponseTopic version p x) p4 (metadataResponseTopics msg)
    pure p5
  | otherwise = error $ "wirePoke MetadataResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for MetadataResponse.
wirePeekMetadataResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (MetadataResponse, Ptr Word8)
wirePeekMetadataResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_brokers, p1) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseBroker version _fp _basePtr p e) p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseTopic version _fp _basePtr p e) p1 endPtr
    pure (MetadataResponse { metadataResponseThrottleTimeMs = 0, metadataResponseBrokers = f0_brokers, metadataResponseClusterId = P.KafkaString Null, metadataResponseControllerId = 0, metadataResponseTopics = f1_topics, metadataResponseClusterAuthorizedOperations = 0, metadataResponseErrorCode = 0 }, p2)
  | version == 1 = do
    (f0_brokers, p1) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseBroker version _fp _basePtr p e) p0 endPtr
    (f1_controllerid, p2) <- W.peekInt32BE p1 endPtr
    (f2_topics, p3) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseTopic version _fp _basePtr p e) p2 endPtr
    pure (MetadataResponse { metadataResponseThrottleTimeMs = 0, metadataResponseBrokers = f0_brokers, metadataResponseClusterId = P.KafkaString Null, metadataResponseControllerId = f1_controllerid, metadataResponseTopics = f2_topics, metadataResponseClusterAuthorizedOperations = 0, metadataResponseErrorCode = 0 }, p3)
  | version == 2 = do
    (f0_brokers, p1) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseBroker version _fp _basePtr p e) p0 endPtr
    (f1_clusterid, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_controllerid, p3) <- W.peekInt32BE p2 endPtr
    (f3_topics, p4) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseTopic version _fp _basePtr p e) p3 endPtr
    pure (MetadataResponse { metadataResponseThrottleTimeMs = 0, metadataResponseBrokers = f0_brokers, metadataResponseClusterId = f1_clusterid, metadataResponseControllerId = f2_controllerid, metadataResponseTopics = f3_topics, metadataResponseClusterAuthorizedOperations = 0, metadataResponseErrorCode = 0 }, p4)
  | version == 8 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_brokers, p2) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseBroker version _fp _basePtr p e) p1 endPtr
    (f2_clusterid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_controllerid, p4) <- W.peekInt32BE p3 endPtr
    (f4_topics, p5) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseTopic version _fp _basePtr p e) p4 endPtr
    (f5_clusterauthorizedoperations, p6) <- W.peekInt32BE p5 endPtr
    pure (MetadataResponse { metadataResponseThrottleTimeMs = f0_throttletimems, metadataResponseBrokers = f1_brokers, metadataResponseClusterId = f2_clusterid, metadataResponseControllerId = f3_controllerid, metadataResponseTopics = f4_topics, metadataResponseClusterAuthorizedOperations = f5_clusterauthorizedoperations, metadataResponseErrorCode = 0 }, p6)
  | version == 13 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_brokers, p2) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseBroker version _fp _basePtr p e) p1 endPtr
    (f2_clusterid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_controllerid, p4) <- W.peekInt32BE p3 endPtr
    (f4_topics, p5) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseTopic version _fp _basePtr p e) p4 endPtr
    (f5_errorcode, p6) <- W.peekInt16BE p5 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p6 endPtr
    pure (MetadataResponse { metadataResponseThrottleTimeMs = f0_throttletimems, metadataResponseBrokers = f1_brokers, metadataResponseClusterId = f2_clusterid, metadataResponseControllerId = f3_controllerid, metadataResponseTopics = f4_topics, metadataResponseClusterAuthorizedOperations = 0, metadataResponseErrorCode = f5_errorcode }, pTagsEnd)
  | version >= 9 && version <= 10 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_brokers, p2) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseBroker version _fp _basePtr p e) p1 endPtr
    (f2_clusterid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_controllerid, p4) <- W.peekInt32BE p3 endPtr
    (f4_topics, p5) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseTopic version _fp _basePtr p e) p4 endPtr
    (f5_clusterauthorizedoperations, p6) <- W.peekInt32BE p5 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p6 endPtr
    pure (MetadataResponse { metadataResponseThrottleTimeMs = f0_throttletimems, metadataResponseBrokers = f1_brokers, metadataResponseClusterId = f2_clusterid, metadataResponseControllerId = f3_controllerid, metadataResponseTopics = f4_topics, metadataResponseClusterAuthorizedOperations = f5_clusterauthorizedoperations, metadataResponseErrorCode = 0 }, pTagsEnd)
  | version >= 11 && version <= 12 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_brokers, p2) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseBroker version _fp _basePtr p e) p1 endPtr
    (f2_clusterid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_controllerid, p4) <- W.peekInt32BE p3 endPtr
    (f4_topics, p5) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseTopic version _fp _basePtr p e) p4 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (MetadataResponse { metadataResponseThrottleTimeMs = f0_throttletimems, metadataResponseBrokers = f1_brokers, metadataResponseClusterId = f2_clusterid, metadataResponseControllerId = f3_controllerid, metadataResponseTopics = f4_topics, metadataResponseClusterAuthorizedOperations = 0, metadataResponseErrorCode = 0 }, pTagsEnd)
  | version >= 3 && version <= 7 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_brokers, p2) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseBroker version _fp _basePtr p e) p1 endPtr
    (f2_clusterid, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_controllerid, p4) <- W.peekInt32BE p3 endPtr
    (f4_topics, p5) <- WP.peekVersionedArray version 9 (\p e -> wirePeekMetadataResponseTopic version _fp _basePtr p e) p4 endPtr
    pure (MetadataResponse { metadataResponseThrottleTimeMs = f0_throttletimems, metadataResponseBrokers = f1_brokers, metadataResponseClusterId = f2_clusterid, metadataResponseControllerId = f3_controllerid, metadataResponseTopics = f4_topics, metadataResponseClusterAuthorizedOperations = 0, metadataResponseErrorCode = 0 }, p5)
  | otherwise = error $ "wirePeek MetadataResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec MetadataResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeMetadataResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeMetadataResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekMetadataResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}