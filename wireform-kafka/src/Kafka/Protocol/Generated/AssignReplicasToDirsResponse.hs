{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AssignReplicasToDirsResponse
Description : Kafka AssignReplicasToDirsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 73.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AssignReplicasToDirsResponse
  (
    AssignReplicasToDirsResponse(..),
    DirectoryData(..),
    TopicData(..),
    PartitionData(..),
    encodeAssignReplicasToDirsResponse,
    decodeAssignReplicasToDirsResponse,
    maxAssignReplicasToDirsResponseVersion
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


-- | The list of assigned partitions.
data PartitionData = PartitionData
  {

  -- | The partition index.

  -- Versions: 0+
  partitionDataPartitionIndex :: !(Int32)
,

  -- | The partition level error code.

  -- Versions: 0+
  partitionDataErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode PartitionData with version-aware field handling.
encodePartitionData :: MonadPut m => E.ApiVersion -> PartitionData -> m ()
encodePartitionData version pmsg =
  do
    serialize (partitionDataPartitionIndex pmsg)
    serialize (partitionDataErrorCode pmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode PartitionData with version-aware field handling.
decodePartitionData :: MonadGet m => E.ApiVersion -> m PartitionData
decodePartitionData version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure PartitionData
      {
      partitionDataPartitionIndex = fieldpartitionindex
      ,
      partitionDataErrorCode = fielderrorcode
      }


-- | The list of topics and their assigned partitions.
data TopicData = TopicData
  {

  -- | The ID of the assigned topic.

  -- Versions: 0+
  topicDataTopicId :: !(KafkaUuid)
,

  -- | The list of assigned partitions.

  -- Versions: 0+
  topicDataPartitions :: !(KafkaArray (PartitionData))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicData with version-aware field handling.
encodeTopicData :: MonadPut m => E.ApiVersion -> TopicData -> m ()
encodeTopicData version tmsg =
  do
    serialize (topicDataTopicId tmsg)
    E.encodeVersionedArray version 0 encodePartitionData (case P.unKafkaArray (topicDataPartitions tmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicData with version-aware field handling.
decodeTopicData :: MonadGet m => E.ApiVersion -> m TopicData
decodeTopicData version =
  do
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodePartitionData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicData
      {
      topicDataTopicId = fieldtopicid
      ,
      topicDataPartitions = fieldpartitions
      }


-- | The list of directories and their assigned partitions.
data DirectoryData = DirectoryData
  {

  -- | The ID of the directory.

  -- Versions: 0+
  directoryDataId :: !(KafkaUuid)
,

  -- | The list of topics and their assigned partitions.

  -- Versions: 0+
  directoryDataTopics :: !(KafkaArray (TopicData))

  }
  deriving (Eq, Show, Generic)


-- | Encode DirectoryData with version-aware field handling.
encodeDirectoryData :: MonadPut m => E.ApiVersion -> DirectoryData -> m ()
encodeDirectoryData version dmsg =
  do
    serialize (directoryDataId dmsg)
    E.encodeVersionedArray version 0 encodeTopicData (case P.unKafkaArray (directoryDataTopics dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DirectoryData with version-aware field handling.
decodeDirectoryData :: MonadGet m => E.ApiVersion -> m DirectoryData
decodeDirectoryData version =
  do
    fieldid <- deserialize
    fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicData
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DirectoryData
      {
      directoryDataId = fieldid
      ,
      directoryDataTopics = fieldtopics
      }



data AssignReplicasToDirsResponse = AssignReplicasToDirsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  assignReplicasToDirsResponseThrottleTimeMs :: !(Int32)
,

  -- | The top level response error code.

  -- Versions: 0+
  assignReplicasToDirsResponseErrorCode :: !(Int16)
,

  -- | The list of directories and their assigned partitions.

  -- Versions: 0+
  assignReplicasToDirsResponseDirectories :: !(KafkaArray (DirectoryData))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AssignReplicasToDirsResponse.
maxAssignReplicasToDirsResponseVersion :: Int16
maxAssignReplicasToDirsResponseVersion = 0

-- | KafkaMessage instance for AssignReplicasToDirsResponse.
instance KafkaMessage AssignReplicasToDirsResponse where
  messageApiKey = 73
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode AssignReplicasToDirsResponse with the given API version.
encodeAssignReplicasToDirsResponse :: MonadPut m => E.ApiVersion -> AssignReplicasToDirsResponse -> m ()
encodeAssignReplicasToDirsResponse version msg
  | version == 0 =
    do
      serialize (assignReplicasToDirsResponseThrottleTimeMs msg)
      serialize (assignReplicasToDirsResponseErrorCode msg)
      E.encodeVersionedArray version 0 encodeDirectoryData (case P.unKafkaArray (assignReplicasToDirsResponseDirectories msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AssignReplicasToDirsResponse with the given API version.
decodeAssignReplicasToDirsResponse :: MonadGet m => E.ApiVersion -> m AssignReplicasToDirsResponse
decodeAssignReplicasToDirsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielddirectories <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDirectoryData
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AssignReplicasToDirsResponse
        {
        assignReplicasToDirsResponseThrottleTimeMs = fieldthrottletimems
        ,
        assignReplicasToDirsResponseErrorCode = fielderrorcode
        ,
        assignReplicasToDirsResponseDirectories = fielddirectories
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a PartitionData.
wireMaxSizePartitionData :: Int -> PartitionData -> Int
wireMaxSizePartitionData _version msg =
  0
  + 4
  + 2
  + 1

-- | Direct-poke encoder for PartitionData.
wirePokePartitionData :: Int -> Ptr Word8 -> PartitionData -> IO (Ptr Word8)
wirePokePartitionData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (partitionDataPartitionIndex msg)
  p2 <- W.pokeInt16BE p1 (partitionDataErrorCode msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for PartitionData.
wirePeekPartitionData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (PartitionData, Ptr Word8)
wirePeekPartitionData version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (PartitionData { partitionDataPartitionIndex = f0_partitionindex, partitionDataErrorCode = f1_errorcode }, pTagsEnd)

-- | Worst-case wire size of a TopicData.
wireMaxSizeTopicData :: Int -> TopicData -> Int
wireMaxSizeTopicData _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (topicDataPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizePartitionData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for TopicData.
wirePokeTopicData :: Int -> Ptr Word8 -> TopicData -> IO (Ptr Word8)
wirePokeTopicData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (topicDataTopicId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokePartitionData version p x) p1 (topicDataPartitions msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for TopicData.
wirePeekTopicData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (TopicData, Ptr Word8)
wirePeekTopicData version _fp _basePtr p0 endPtr = do
  (f0_topicid, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekPartitionData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (TopicData { topicDataTopicId = f0_topicid, topicDataPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a DirectoryData.
wireMaxSizeDirectoryData :: Int -> DirectoryData -> Int
wireMaxSizeDirectoryData _version msg =
  0
  + 16
  + (5 + (case P.unKafkaArray (directoryDataTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeTopicData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DirectoryData.
wirePokeDirectoryData :: Int -> Ptr Word8 -> DirectoryData -> IO (Ptr Word8)
wirePokeDirectoryData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeKafkaUuid p0 (directoryDataId msg)
  p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeTopicData version p x) p1 (directoryDataTopics msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DirectoryData.
wirePeekDirectoryData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DirectoryData, Ptr Word8)
wirePeekDirectoryData version _fp _basePtr p0 endPtr = do
  (f0_id, p1) <- WP.peekKafkaUuid p0 endPtr
  (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekTopicData version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DirectoryData { directoryDataId = f0_id, directoryDataTopics = f1_topics }, pTagsEnd)

-- | Worst-case wire size of a AssignReplicasToDirsResponse.
wireMaxSizeAssignReplicasToDirsResponse :: Int -> AssignReplicasToDirsResponse -> Int
wireMaxSizeAssignReplicasToDirsResponse _version msg =
  0
  + 4
  + 2
  + (5 + (case P.unKafkaArray (assignReplicasToDirsResponseDirectories msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDirectoryData _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AssignReplicasToDirsResponse.
wirePokeAssignReplicasToDirsResponse :: Int -> Ptr Word8 -> AssignReplicasToDirsResponse -> IO (Ptr Word8)
wirePokeAssignReplicasToDirsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (assignReplicasToDirsResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (assignReplicasToDirsResponseErrorCode msg)
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDirectoryData version p x) p2 (assignReplicasToDirsResponseDirectories msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke AssignReplicasToDirsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for AssignReplicasToDirsResponse.
wirePeekAssignReplicasToDirsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AssignReplicasToDirsResponse, Ptr Word8)
wirePeekAssignReplicasToDirsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_directories, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDirectoryData version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (AssignReplicasToDirsResponse { assignReplicasToDirsResponseThrottleTimeMs = f0_throttletimems, assignReplicasToDirsResponseErrorCode = f1_errorcode, assignReplicasToDirsResponseDirectories = f2_directories }, pTagsEnd)
  | otherwise = error $ "wirePeek AssignReplicasToDirsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec AssignReplicasToDirsResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAssignReplicasToDirsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAssignReplicasToDirsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAssignReplicasToDirsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}