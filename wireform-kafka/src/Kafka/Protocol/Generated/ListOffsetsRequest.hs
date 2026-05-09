{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListOffsetsRequest
Description : Kafka ListOffsetsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 2.



Valid versions: 1-11
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
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


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
maxListOffsetsRequestVersion = 11

-- | KafkaMessage instance for ListOffsetsRequest.
instance KafkaMessage ListOffsetsRequest where
  messageApiKey = 2
  messageMinVersion = 1
  messageMaxVersion = 11
  messageFlexibleVersion = Just 6

-- | Encode ListOffsetsRequest with the given API version.
encodeListOffsetsRequest :: MonadPut m => E.ApiVersion -> ListOffsetsRequest -> m ()
encodeListOffsetsRequest version msg
  | version == 1 =
    do
      serialize (listOffsetsRequestReplicaId msg)
      E.encodeVersionedArray version 6 encodeListOffsetsTopic (case P.unKafkaArray (listOffsetsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 10 && version <= 11 =
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

  | version >= 10 && version <= 11 =
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

-- | Worst-case wire size of a ListOffsetsPartition.
wireMaxSizeListOffsetsPartition :: Int -> ListOffsetsPartition -> Int
wireMaxSizeListOffsetsPartition _version msg =
  0
  + 4
  + 4
  + 8
  + 1

-- | Direct-poke encoder for ListOffsetsPartition.
wirePokeListOffsetsPartition :: Int -> Ptr Word8 -> ListOffsetsPartition -> IO (Ptr Word8)
wirePokeListOffsetsPartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (listOffsetsPartitionPartitionIndex msg)
  p2 <- W.pokeInt32BE p1 (listOffsetsPartitionCurrentLeaderEpoch msg)
  p3 <- W.pokeInt64BE p2 (listOffsetsPartitionTimestamp msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for ListOffsetsPartition.
wirePeekListOffsetsPartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListOffsetsPartition, Ptr Word8)
wirePeekListOffsetsPartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_currentleaderepoch, p2) <- W.peekInt32BE p1 endPtr
  (f2_timestamp, p3) <- W.peekInt64BE p2 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (ListOffsetsPartition { listOffsetsPartitionPartitionIndex = f0_partitionindex, listOffsetsPartitionCurrentLeaderEpoch = f1_currentleaderepoch, listOffsetsPartitionTimestamp = f2_timestamp }, pTagsEnd)

-- | Worst-case wire size of a ListOffsetsTopic.
wireMaxSizeListOffsetsTopic :: Int -> ListOffsetsTopic -> Int
wireMaxSizeListOffsetsTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (listOffsetsTopicName msg))
  + (5 + (case P.unKafkaArray (listOffsetsTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeListOffsetsPartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ListOffsetsTopic.
wirePokeListOffsetsTopic :: Int -> Ptr Word8 -> ListOffsetsTopic -> IO (Ptr Word8)
wirePokeListOffsetsTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (listOffsetsTopicName msg))
  p2 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsPartition version p x) p1 (listOffsetsTopicPartitions msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for ListOffsetsTopic.
wirePeekListOffsetsTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListOffsetsTopic, Ptr Word8)
wirePeekListOffsetsTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsPartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (ListOffsetsTopic { listOffsetsTopicName = f0_name, listOffsetsTopicPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a ListOffsetsRequest.
wireMaxSizeListOffsetsRequest :: Int -> ListOffsetsRequest -> Int
wireMaxSizeListOffsetsRequest _version msg =
  0
  + 4
  + 1
  + (5 + (case P.unKafkaArray (listOffsetsRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeListOffsetsTopic _version x ) v); P.Null -> 0 }))
  + 4
  + 1

-- | Direct-poke encoder for ListOffsetsRequest.
wirePokeListOffsetsRequest :: Int -> Ptr Word8 -> ListOffsetsRequest -> IO (Ptr Word8)
wirePokeListOffsetsRequest version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listOffsetsRequestReplicaId msg)
    p2 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsTopic version p x) p1 (listOffsetsRequestTopics msg)
    pure p2
  | version >= 10 && version <= 11 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listOffsetsRequestReplicaId msg)
    p2 <- W.pokeWord8 p1 (fromIntegral (listOffsetsRequestIsolationLevel msg))
    p3 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsTopic version p x) p2 (listOffsetsRequestTopics msg)
    p4 <- W.pokeInt32BE p3 (listOffsetsRequestTimeoutMs msg)
    WP.pokeEmptyTaggedFields p4
  | version >= 2 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listOffsetsRequestReplicaId msg)
    p2 <- W.pokeWord8 p1 (fromIntegral (listOffsetsRequestIsolationLevel msg))
    p3 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsTopic version p x) p2 (listOffsetsRequestTopics msg)
    pure p3
  | version >= 6 && version <= 9 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listOffsetsRequestReplicaId msg)
    p2 <- W.pokeWord8 p1 (fromIntegral (listOffsetsRequestIsolationLevel msg))
    p3 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeListOffsetsTopic version p x) p2 (listOffsetsRequestTopics msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke ListOffsetsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ListOffsetsRequest.
wirePeekListOffsetsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListOffsetsRequest, Ptr Word8)
wirePeekListOffsetsRequest version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsTopic version _fp _basePtr p e) p1 endPtr
    pure (ListOffsetsRequest { listOffsetsRequestReplicaId = f0_replicaid, listOffsetsRequestIsolationLevel = 0, listOffsetsRequestTopics = f1_topics, listOffsetsRequestTimeoutMs = 0 }, p2)
  | version >= 10 && version <= 11 = do
    (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
    (f1_isolationlevel, p2) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p1 endPtr
    (f2_topics, p3) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsTopic version _fp _basePtr p e) p2 endPtr
    (f3_timeoutms, p4) <- W.peekInt32BE p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (ListOffsetsRequest { listOffsetsRequestReplicaId = f0_replicaid, listOffsetsRequestIsolationLevel = f1_isolationlevel, listOffsetsRequestTopics = f2_topics, listOffsetsRequestTimeoutMs = f3_timeoutms }, pTagsEnd)
  | version >= 2 && version <= 5 = do
    (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
    (f1_isolationlevel, p2) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p1 endPtr
    (f2_topics, p3) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsTopic version _fp _basePtr p e) p2 endPtr
    pure (ListOffsetsRequest { listOffsetsRequestReplicaId = f0_replicaid, listOffsetsRequestIsolationLevel = f1_isolationlevel, listOffsetsRequestTopics = f2_topics, listOffsetsRequestTimeoutMs = 0 }, p3)
  | version >= 6 && version <= 9 = do
    (f0_replicaid, p1) <- W.peekInt32BE p0 endPtr
    (f1_isolationlevel, p2) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p1 endPtr
    (f2_topics, p3) <- WP.peekVersionedArray version 6 (\p e -> wirePeekListOffsetsTopic version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (ListOffsetsRequest { listOffsetsRequestReplicaId = f0_replicaid, listOffsetsRequestIsolationLevel = f1_isolationlevel, listOffsetsRequestTopics = f2_topics, listOffsetsRequestTimeoutMs = 0 }, pTagsEnd)
  | otherwise = error $ "wirePeek ListOffsetsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ListOffsetsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeListOffsetsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeListOffsetsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekListOffsetsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}