{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetFetchResponse
Description : Kafka OffsetFetchResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 9.



Valid versions: 1-10
Flexible versions: 6+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetFetchResponse
  (
    OffsetFetchResponse(..),
    OffsetFetchResponseTopic(..),
    OffsetFetchResponsePartition(..),
    OffsetFetchResponseGroup(..),
    OffsetFetchResponseTopics(..),
    OffsetFetchResponsePartitions(..),
    encodeOffsetFetchResponse,
    decodeOffsetFetchResponse,
    maxOffsetFetchResponseVersion
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


-- | The responses per partition.
data OffsetFetchResponsePartition = OffsetFetchResponsePartition
  {

  -- | The partition index.

  -- Versions: 0-7
  offsetFetchResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The committed message offset.

  -- Versions: 0-7
  offsetFetchResponsePartitionCommittedOffset :: !(Int64)
,

  -- | The leader epoch.

  -- Versions: 5-7
  offsetFetchResponsePartitionCommittedLeaderEpoch :: !(Int32)
,

  -- | The partition metadata.

  -- Versions: 0-7
  offsetFetchResponsePartitionMetadata :: !(KafkaString)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0-7
  offsetFetchResponsePartitionErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetFetchResponsePartition with version-aware field handling.
encodeOffsetFetchResponsePartition :: MonadPut m => E.ApiVersion -> OffsetFetchResponsePartition -> m ()
encodeOffsetFetchResponsePartition version omsg =
  do
    when (version >= 0 && version <= 7) $
      serialize (offsetFetchResponsePartitionPartitionIndex omsg)
    when (version >= 0 && version <= 7) $
      serialize (offsetFetchResponsePartitionCommittedOffset omsg)
    when (version >= 5 && version <= 7) $
      serialize (offsetFetchResponsePartitionCommittedLeaderEpoch omsg)
    when (version >= 0 && version <= 7) $
      if version >= 6 then serialize (toCompactString (offsetFetchResponsePartitionMetadata omsg)) else serialize (offsetFetchResponsePartitionMetadata omsg)
    when (version >= 0 && version <= 7) $
      serialize (offsetFetchResponsePartitionErrorCode omsg)
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchResponsePartition with version-aware field handling.
decodeOffsetFetchResponsePartition :: MonadGet m => E.ApiVersion -> m OffsetFetchResponsePartition
decodeOffsetFetchResponsePartition version =
  do
    fieldpartitionindex <- if version >= 0 && version <= 7
      then deserialize
      else pure (0)
    fieldcommittedoffset <- if version >= 0 && version <= 7
      then deserialize
      else pure (0)
    fieldcommittedleaderepoch <- if version >= 5 && version <= 7
      then deserialize
      else pure ((-1))
    fieldmetadata <- if version >= 0 && version <= 7
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fielderrorcode <- if version >= 0 && version <= 7
      then deserialize
      else pure (0)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchResponsePartition
      {
      offsetFetchResponsePartitionPartitionIndex = fieldpartitionindex
      ,
      offsetFetchResponsePartitionCommittedOffset = fieldcommittedoffset
      ,
      offsetFetchResponsePartitionCommittedLeaderEpoch = fieldcommittedleaderepoch
      ,
      offsetFetchResponsePartitionMetadata = fieldmetadata
      ,
      offsetFetchResponsePartitionErrorCode = fielderrorcode
      }


-- | The responses per topic.
data OffsetFetchResponseTopic = OffsetFetchResponseTopic
  {

  -- | The topic name.

  -- Versions: 0-7
  offsetFetchResponseTopicName :: !(KafkaString)
,

  -- | The responses per partition.

  -- Versions: 0-7
  offsetFetchResponseTopicPartitions :: !(KafkaArray (OffsetFetchResponsePartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetFetchResponseTopic with version-aware field handling.
encodeOffsetFetchResponseTopic :: MonadPut m => E.ApiVersion -> OffsetFetchResponseTopic -> m ()
encodeOffsetFetchResponseTopic version omsg =
  do
    when (version >= 0 && version <= 7) $
      if version >= 6 then serialize (toCompactString (offsetFetchResponseTopicName omsg)) else serialize (offsetFetchResponseTopicName omsg)
    when (version >= 0 && version <= 7) $
      E.encodeVersionedArray version 6 encodeOffsetFetchResponsePartition (case P.unKafkaArray (offsetFetchResponseTopicPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchResponseTopic with version-aware field handling.
decodeOffsetFetchResponseTopic :: MonadGet m => E.ApiVersion -> m OffsetFetchResponseTopic
decodeOffsetFetchResponseTopic version =
  do
    fieldname <- if version >= 0 && version <= 7
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldpartitions <- if version >= 0 && version <= 7
      then P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponsePartition
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchResponseTopic
      {
      offsetFetchResponseTopicName = fieldname
      ,
      offsetFetchResponseTopicPartitions = fieldpartitions
      }


-- | The responses per partition.
data OffsetFetchResponsePartitions = OffsetFetchResponsePartitions
  {

  -- | The partition index.

  -- Versions: 8+
  offsetFetchResponsePartitionsPartitionIndex :: !(Int32)
,

  -- | The committed message offset.

  -- Versions: 8+
  offsetFetchResponsePartitionsCommittedOffset :: !(Int64)
,

  -- | The leader epoch.

  -- Versions: 8+
  offsetFetchResponsePartitionsCommittedLeaderEpoch :: !(Int32)
,

  -- | The partition metadata.

  -- Versions: 8+
  offsetFetchResponsePartitionsMetadata :: !(KafkaString)
,

  -- | The partition-level error code, or 0 if there was no error.

  -- Versions: 8+
  offsetFetchResponsePartitionsErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetFetchResponsePartitions with version-aware field handling.
encodeOffsetFetchResponsePartitions :: MonadPut m => E.ApiVersion -> OffsetFetchResponsePartitions -> m ()
encodeOffsetFetchResponsePartitions version omsg =
  do
    when (version >= 8) $
      serialize (offsetFetchResponsePartitionsPartitionIndex omsg)
    when (version >= 8) $
      serialize (offsetFetchResponsePartitionsCommittedOffset omsg)
    when (version >= 8) $
      serialize (offsetFetchResponsePartitionsCommittedLeaderEpoch omsg)
    when (version >= 8) $
      if version >= 6 then serialize (toCompactString (offsetFetchResponsePartitionsMetadata omsg)) else serialize (offsetFetchResponsePartitionsMetadata omsg)
    when (version >= 8) $
      serialize (offsetFetchResponsePartitionsErrorCode omsg)
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchResponsePartitions with version-aware field handling.
decodeOffsetFetchResponsePartitions :: MonadGet m => E.ApiVersion -> m OffsetFetchResponsePartitions
decodeOffsetFetchResponsePartitions version =
  do
    fieldpartitionindex <- if version >= 8
      then deserialize
      else pure (0)
    fieldcommittedoffset <- if version >= 8
      then deserialize
      else pure (0)
    fieldcommittedleaderepoch <- if version >= 8
      then deserialize
      else pure ((-1))
    fieldmetadata <- if version >= 8
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fielderrorcode <- if version >= 8
      then deserialize
      else pure (0)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchResponsePartitions
      {
      offsetFetchResponsePartitionsPartitionIndex = fieldpartitionindex
      ,
      offsetFetchResponsePartitionsCommittedOffset = fieldcommittedoffset
      ,
      offsetFetchResponsePartitionsCommittedLeaderEpoch = fieldcommittedleaderepoch
      ,
      offsetFetchResponsePartitionsMetadata = fieldmetadata
      ,
      offsetFetchResponsePartitionsErrorCode = fielderrorcode
      }


-- | The responses per topic.
data OffsetFetchResponseTopics = OffsetFetchResponseTopics
  {

  -- | The topic name.

  -- Versions: 8-9
  offsetFetchResponseTopicsName :: !(KafkaString)
,

  -- | The topic ID.

  -- Versions: 10+
  offsetFetchResponseTopicsTopicId :: !(KafkaUuid)
,

  -- | The responses per partition.

  -- Versions: 8+
  offsetFetchResponseTopicsPartitions :: !(KafkaArray (OffsetFetchResponsePartitions))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetFetchResponseTopics with version-aware field handling.
encodeOffsetFetchResponseTopics :: MonadPut m => E.ApiVersion -> OffsetFetchResponseTopics -> m ()
encodeOffsetFetchResponseTopics version omsg =
  do
    when (version >= 8 && version <= 9) $
      if version >= 6 then serialize (toCompactString (offsetFetchResponseTopicsName omsg)) else serialize (offsetFetchResponseTopicsName omsg)
    when (version >= 10) $
      serialize (offsetFetchResponseTopicsTopicId omsg)
    when (version >= 8) $
      E.encodeVersionedArray version 6 encodeOffsetFetchResponsePartitions (case P.unKafkaArray (offsetFetchResponseTopicsPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchResponseTopics with version-aware field handling.
decodeOffsetFetchResponseTopics :: MonadGet m => E.ApiVersion -> m OffsetFetchResponseTopics
decodeOffsetFetchResponseTopics version =
  do
    fieldname <- if version >= 8 && version <= 9
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 10
      then deserialize
      else pure (P.nullUuid)
    fieldpartitions <- if version >= 8
      then P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponsePartitions
      else pure (P.mkKafkaArray V.empty)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchResponseTopics
      {
      offsetFetchResponseTopicsName = fieldname
      ,
      offsetFetchResponseTopicsTopicId = fieldtopicid
      ,
      offsetFetchResponseTopicsPartitions = fieldpartitions
      }


-- | The responses per group id.
data OffsetFetchResponseGroup = OffsetFetchResponseGroup
  {

  -- | The group ID.

  -- Versions: 8+
  offsetFetchResponseGroupGroupId :: !(KafkaString)
,

  -- | The responses per topic.

  -- Versions: 8+
  offsetFetchResponseGroupTopics :: !(KafkaArray (OffsetFetchResponseTopics))
,

  -- | The group-level error code, or 0 if there was no error.

  -- Versions: 8+
  offsetFetchResponseGroupErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetFetchResponseGroup with version-aware field handling.
encodeOffsetFetchResponseGroup :: MonadPut m => E.ApiVersion -> OffsetFetchResponseGroup -> m ()
encodeOffsetFetchResponseGroup version omsg =
  do
    when (version >= 8) $
      if version >= 6 then serialize (toCompactString (offsetFetchResponseGroupGroupId omsg)) else serialize (offsetFetchResponseGroupGroupId omsg)
    when (version >= 8) $
      E.encodeVersionedArray version 6 encodeOffsetFetchResponseTopics (case P.unKafkaArray (offsetFetchResponseGroupTopics omsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 8) $
      serialize (offsetFetchResponseGroupErrorCode omsg)
    when (version >= 6) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode OffsetFetchResponseGroup with version-aware field handling.
decodeOffsetFetchResponseGroup :: MonadGet m => E.ApiVersion -> m OffsetFetchResponseGroup
decodeOffsetFetchResponseGroup version =
  do
    fieldgroupid <- if version >= 8
      then if version >= 6 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopics <- if version >= 8
      then P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponseTopics
      else pure (P.mkKafkaArray V.empty)
    fielderrorcode <- if version >= 8
      then deserialize
      else pure (0)
    _ <- if version >= 6 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure OffsetFetchResponseGroup
      {
      offsetFetchResponseGroupGroupId = fieldgroupid
      ,
      offsetFetchResponseGroupTopics = fieldtopics
      ,
      offsetFetchResponseGroupErrorCode = fielderrorcode
      }



data OffsetFetchResponse = OffsetFetchResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 3+
  offsetFetchResponseThrottleTimeMs :: !(Int32)
,

  -- | The responses per topic.

  -- Versions: 0-7
  offsetFetchResponseTopics :: !(KafkaArray (OffsetFetchResponseTopic))
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 2-7
  offsetFetchResponseErrorCode :: !(Int16)
,

  -- | The responses per group id.

  -- Versions: 8+
  offsetFetchResponseGroups :: !(KafkaArray (OffsetFetchResponseGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetFetchResponse.
maxOffsetFetchResponseVersion :: Int16
maxOffsetFetchResponseVersion = 10

-- | KafkaMessage instance for OffsetFetchResponse.
instance KafkaMessage OffsetFetchResponse where
  messageApiKey = 9
  messageMinVersion = 1
  messageMaxVersion = 10
  messageFlexibleVersion = Just 6

-- | Encode OffsetFetchResponse with the given API version.
encodeOffsetFetchResponse :: MonadPut m => E.ApiVersion -> OffsetFetchResponse -> m ()
encodeOffsetFetchResponse version msg
  | version == 1 =
    do
      E.encodeVersionedArray version 6 encodeOffsetFetchResponseTopic (case P.unKafkaArray (offsetFetchResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 2 =
    do
      E.encodeVersionedArray version 6 encodeOffsetFetchResponseTopic (case P.unKafkaArray (offsetFetchResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (offsetFetchResponseErrorCode msg)


  | version >= 6 && version <= 7 =
    do
      serialize (offsetFetchResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 6 encodeOffsetFetchResponseTopic (case P.unKafkaArray (offsetFetchResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (offsetFetchResponseErrorCode msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 3 && version <= 5 =
    do
      serialize (offsetFetchResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 6 encodeOffsetFetchResponseTopic (case P.unKafkaArray (offsetFetchResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (offsetFetchResponseErrorCode msg)


  | version >= 8 && version <= 10 =
    do
      serialize (offsetFetchResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 6 encodeOffsetFetchResponseGroup (case P.unKafkaArray (offsetFetchResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetFetchResponse with the given API version.
decodeOffsetFetchResponse :: MonadGet m => E.ApiVersion -> m OffsetFetchResponse
decodeOffsetFetchResponse version
  | version == 1 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponseTopic
      pure OffsetFetchResponse
        {
        offsetFetchResponseThrottleTimeMs = 0
        ,
        offsetFetchResponseTopics = fieldtopics
        ,
        offsetFetchResponseErrorCode = 0
        ,
        offsetFetchResponseGroups = P.mkKafkaArray V.empty
        }

  | version == 2 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponseTopic
      fielderrorcode <- deserialize
      pure OffsetFetchResponse
        {
        offsetFetchResponseThrottleTimeMs = 0
        ,
        offsetFetchResponseTopics = fieldtopics
        ,
        offsetFetchResponseErrorCode = fielderrorcode
        ,
        offsetFetchResponseGroups = P.mkKafkaArray V.empty
        }

  | version >= 6 && version <= 7 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponseTopic
      fielderrorcode <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetFetchResponse
        {
        offsetFetchResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetFetchResponseTopics = fieldtopics
        ,
        offsetFetchResponseErrorCode = fielderrorcode
        ,
        offsetFetchResponseGroups = P.mkKafkaArray V.empty
        }

  | version >= 3 && version <= 5 =
    do
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponseTopic
      fielderrorcode <- deserialize
      pure OffsetFetchResponse
        {
        offsetFetchResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetFetchResponseTopics = fieldtopics
        ,
        offsetFetchResponseErrorCode = fielderrorcode
        ,
        offsetFetchResponseGroups = P.mkKafkaArray V.empty
        }

  | version >= 8 && version <= 10 =
    do
      fieldthrottletimems <- deserialize
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 6 decodeOffsetFetchResponseGroup
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure OffsetFetchResponse
        {
        offsetFetchResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetFetchResponseTopics = P.mkKafkaArray V.empty
        ,
        offsetFetchResponseErrorCode = 0
        ,
        offsetFetchResponseGroups = fieldgroups
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a OffsetFetchResponsePartition.
wireMaxSizeOffsetFetchResponsePartition :: Int -> OffsetFetchResponsePartition -> Int
wireMaxSizeOffsetFetchResponsePartition _version msg =
  0
  + 4
  + 8
  + 4
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchResponsePartitionMetadata msg))
  + 2
  + 1

-- | Direct-poke encoder for OffsetFetchResponsePartition.
wirePokeOffsetFetchResponsePartition :: Int -> Ptr Word8 -> OffsetFetchResponsePartition -> IO (Ptr Word8)
wirePokeOffsetFetchResponsePartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (offsetFetchResponsePartitionPartitionIndex msg)
  p2 <- W.pokeInt64BE p1 (offsetFetchResponsePartitionCommittedOffset msg)
  p3 <- W.pokeInt32BE p2 (offsetFetchResponsePartitionCommittedLeaderEpoch msg)
  p4 <- WP.pokeCompactString p3 (P.toCompactString (offsetFetchResponsePartitionMetadata msg))
  p5 <- W.pokeInt16BE p4 (offsetFetchResponsePartitionErrorCode msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for OffsetFetchResponsePartition.
wirePeekOffsetFetchResponsePartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchResponsePartition, Ptr Word8)
wirePeekOffsetFetchResponsePartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_committedoffset, p2) <- W.peekInt64BE p1 endPtr
  (f2_committedleaderepoch, p3) <- W.peekInt32BE p2 endPtr
  (f3_metadata, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  (f4_errorcode, p5) <- W.peekInt16BE p4 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (OffsetFetchResponsePartition { offsetFetchResponsePartitionPartitionIndex = f0_partitionindex, offsetFetchResponsePartitionCommittedOffset = f1_committedoffset, offsetFetchResponsePartitionCommittedLeaderEpoch = f2_committedleaderepoch, offsetFetchResponsePartitionMetadata = f3_metadata, offsetFetchResponsePartitionErrorCode = f4_errorcode }, pTagsEnd)

-- | Worst-case wire size of a OffsetFetchResponseTopic.
wireMaxSizeOffsetFetchResponseTopic :: Int -> OffsetFetchResponseTopic -> Int
wireMaxSizeOffsetFetchResponseTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchResponseTopicName msg))
  + (5 + (case P.unKafkaArray (offsetFetchResponseTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchResponsePartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetFetchResponseTopic.
wirePokeOffsetFetchResponseTopic :: Int -> Ptr Word8 -> OffsetFetchResponseTopic -> IO (Ptr Word8)
wirePokeOffsetFetchResponseTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (offsetFetchResponseTopicName msg))
  p2 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponsePartition version p x) p1 (offsetFetchResponseTopicPartitions msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for OffsetFetchResponseTopic.
wirePeekOffsetFetchResponseTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchResponseTopic, Ptr Word8)
wirePeekOffsetFetchResponseTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponsePartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (OffsetFetchResponseTopic { offsetFetchResponseTopicName = f0_name, offsetFetchResponseTopicPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a OffsetFetchResponsePartitions.
wireMaxSizeOffsetFetchResponsePartitions :: Int -> OffsetFetchResponsePartitions -> Int
wireMaxSizeOffsetFetchResponsePartitions _version msg =
  0
  + 4
  + 8
  + 4
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchResponsePartitionsMetadata msg))
  + 2
  + 1

-- | Direct-poke encoder for OffsetFetchResponsePartitions.
wirePokeOffsetFetchResponsePartitions :: Int -> Ptr Word8 -> OffsetFetchResponsePartitions -> IO (Ptr Word8)
wirePokeOffsetFetchResponsePartitions version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (offsetFetchResponsePartitionsPartitionIndex msg)
  p2 <- W.pokeInt64BE p1 (offsetFetchResponsePartitionsCommittedOffset msg)
  p3 <- W.pokeInt32BE p2 (offsetFetchResponsePartitionsCommittedLeaderEpoch msg)
  p4 <- WP.pokeCompactString p3 (P.toCompactString (offsetFetchResponsePartitionsMetadata msg))
  p5 <- W.pokeInt16BE p4 (offsetFetchResponsePartitionsErrorCode msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for OffsetFetchResponsePartitions.
wirePeekOffsetFetchResponsePartitions :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchResponsePartitions, Ptr Word8)
wirePeekOffsetFetchResponsePartitions version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_committedoffset, p2) <- W.peekInt64BE p1 endPtr
  (f2_committedleaderepoch, p3) <- W.peekInt32BE p2 endPtr
  (f3_metadata, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  (f4_errorcode, p5) <- W.peekInt16BE p4 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (OffsetFetchResponsePartitions { offsetFetchResponsePartitionsPartitionIndex = f0_partitionindex, offsetFetchResponsePartitionsCommittedOffset = f1_committedoffset, offsetFetchResponsePartitionsCommittedLeaderEpoch = f2_committedleaderepoch, offsetFetchResponsePartitionsMetadata = f3_metadata, offsetFetchResponsePartitionsErrorCode = f4_errorcode }, pTagsEnd)

-- | Worst-case wire size of a OffsetFetchResponseTopics.
wireMaxSizeOffsetFetchResponseTopics :: Int -> OffsetFetchResponseTopics -> Int
wireMaxSizeOffsetFetchResponseTopics _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchResponseTopicsName msg))
  + 16
  + (5 + (case P.unKafkaArray (offsetFetchResponseTopicsPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchResponsePartitions _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetFetchResponseTopics.
wirePokeOffsetFetchResponseTopics :: Int -> Ptr Word8 -> OffsetFetchResponseTopics -> IO (Ptr Word8)
wirePokeOffsetFetchResponseTopics version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (offsetFetchResponseTopicsName msg))
  p2 <- WP.pokeKafkaUuid p1 (offsetFetchResponseTopicsTopicId msg)
  p3 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponsePartitions version p x) p2 (offsetFetchResponseTopicsPartitions msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for OffsetFetchResponseTopics.
wirePeekOffsetFetchResponseTopics :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchResponseTopics, Ptr Word8)
wirePeekOffsetFetchResponseTopics version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_topicid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_partitions, p3) <- WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponsePartitions version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (OffsetFetchResponseTopics { offsetFetchResponseTopicsName = f0_name, offsetFetchResponseTopicsTopicId = f1_topicid, offsetFetchResponseTopicsPartitions = f2_partitions }, pTagsEnd)

-- | Worst-case wire size of a OffsetFetchResponseGroup.
wireMaxSizeOffsetFetchResponseGroup :: Int -> OffsetFetchResponseGroup -> Int
wireMaxSizeOffsetFetchResponseGroup _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (offsetFetchResponseGroupGroupId msg))
  + (5 + (case P.unKafkaArray (offsetFetchResponseGroupTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchResponseTopics _version x ) v); P.Null -> 0 }))
  + 2
  + 1

-- | Direct-poke encoder for OffsetFetchResponseGroup.
wirePokeOffsetFetchResponseGroup :: Int -> Ptr Word8 -> OffsetFetchResponseGroup -> IO (Ptr Word8)
wirePokeOffsetFetchResponseGroup version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (offsetFetchResponseGroupGroupId msg))
  p2 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponseTopics version p x) p1 (offsetFetchResponseGroupTopics msg)
  p3 <- W.pokeInt16BE p2 (offsetFetchResponseGroupErrorCode msg)
  if version >= 6 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for OffsetFetchResponseGroup.
wirePeekOffsetFetchResponseGroup :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchResponseGroup, Ptr Word8)
wirePeekOffsetFetchResponseGroup version _fp _basePtr p0 endPtr = do
  (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_topics, p2) <- WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponseTopics version _fp _basePtr p e) p1 endPtr
  (f2_errorcode, p3) <- W.peekInt16BE p2 endPtr
  pTagsEnd <- if version >= 6 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (OffsetFetchResponseGroup { offsetFetchResponseGroupGroupId = f0_groupid, offsetFetchResponseGroupTopics = f1_topics, offsetFetchResponseGroupErrorCode = f2_errorcode }, pTagsEnd)

-- | Worst-case wire size of a OffsetFetchResponse.
wireMaxSizeOffsetFetchResponse :: Int -> OffsetFetchResponse -> Int
wireMaxSizeOffsetFetchResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (offsetFetchResponseTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchResponseTopic _version x ) v); P.Null -> 0 }))
  + 2
  + (5 + (case P.unKafkaArray (offsetFetchResponseGroups msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeOffsetFetchResponseGroup _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for OffsetFetchResponse.
wirePokeOffsetFetchResponse :: Int -> Ptr Word8 -> OffsetFetchResponse -> IO (Ptr Word8)
wirePokeOffsetFetchResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponseTopic version p x) p0 (offsetFetchResponseTopics msg)
    pure p1
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponseTopic version p x) p0 (offsetFetchResponseTopics msg)
    p2 <- W.pokeInt16BE p1 (offsetFetchResponseErrorCode msg)
    pure p2
  | version >= 6 && version <= 7 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (offsetFetchResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponseTopic version p x) p1 (offsetFetchResponseTopics msg)
    p3 <- W.pokeInt16BE p2 (offsetFetchResponseErrorCode msg)
    WP.pokeEmptyTaggedFields p3
  | version >= 3 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (offsetFetchResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponseTopic version p x) p1 (offsetFetchResponseTopics msg)
    p3 <- W.pokeInt16BE p2 (offsetFetchResponseErrorCode msg)
    pure p3
  | version >= 8 && version <= 10 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (offsetFetchResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 6 (\p x -> wirePokeOffsetFetchResponseGroup version p x) p1 (offsetFetchResponseGroups msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke OffsetFetchResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for OffsetFetchResponse.
wirePeekOffsetFetchResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (OffsetFetchResponse, Ptr Word8)
wirePeekOffsetFetchResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponseTopic version _fp _basePtr p e) p0 endPtr
    pure (OffsetFetchResponse { offsetFetchResponseThrottleTimeMs = 0, offsetFetchResponseTopics = f0_topics, offsetFetchResponseErrorCode = 0, offsetFetchResponseGroups = P.mkKafkaArray V.empty }, p1)
  | version == 2 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponseTopic version _fp _basePtr p e) p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    pure (OffsetFetchResponse { offsetFetchResponseThrottleTimeMs = 0, offsetFetchResponseTopics = f0_topics, offsetFetchResponseErrorCode = f1_errorcode, offsetFetchResponseGroups = P.mkKafkaArray V.empty }, p2)
  | version >= 6 && version <= 7 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponseTopic version _fp _basePtr p e) p1 endPtr
    (f2_errorcode, p3) <- W.peekInt16BE p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (OffsetFetchResponse { offsetFetchResponseThrottleTimeMs = f0_throttletimems, offsetFetchResponseTopics = f1_topics, offsetFetchResponseErrorCode = f2_errorcode, offsetFetchResponseGroups = P.mkKafkaArray V.empty }, pTagsEnd)
  | version >= 3 && version <= 5 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponseTopic version _fp _basePtr p e) p1 endPtr
    (f2_errorcode, p3) <- W.peekInt16BE p2 endPtr
    pure (OffsetFetchResponse { offsetFetchResponseThrottleTimeMs = f0_throttletimems, offsetFetchResponseTopics = f1_topics, offsetFetchResponseErrorCode = f2_errorcode, offsetFetchResponseGroups = P.mkKafkaArray V.empty }, p3)
  | version >= 8 && version <= 10 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_groups, p2) <- WP.peekVersionedArray version 6 (\p e -> wirePeekOffsetFetchResponseGroup version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (OffsetFetchResponse { offsetFetchResponseThrottleTimeMs = f0_throttletimems, offsetFetchResponseTopics = P.mkKafkaArray V.empty, offsetFetchResponseErrorCode = 0, offsetFetchResponseGroups = f1_groups }, pTagsEnd)
  | otherwise = error $ "wirePeek OffsetFetchResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec OffsetFetchResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeOffsetFetchResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeOffsetFetchResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekOffsetFetchResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}