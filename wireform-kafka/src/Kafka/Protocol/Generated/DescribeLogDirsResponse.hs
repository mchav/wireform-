{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeLogDirsResponse
Description : Kafka DescribeLogDirsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 35.



Valid versions: 1-5
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeLogDirsResponse
  (
    DescribeLogDirsResponse(..),
    DescribeLogDirsResult(..),
    DescribeLogDirsTopic(..),
    DescribeLogDirsPartition(..),
    encodeDescribeLogDirsResponse,
    decodeDescribeLogDirsResponse,
    maxDescribeLogDirsResponseVersion
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


-- | The partitions.
data DescribeLogDirsPartition = DescribeLogDirsPartition
  {

  -- | The partition index.

  -- Versions: 0+
  describeLogDirsPartitionPartitionIndex :: !(Int32)
,

  -- | The size of the log segments in this partition in bytes.

  -- Versions: 0+
  describeLogDirsPartitionPartitionSize :: !(Int64)
,

  -- | The lag of the log's LEO w.r.t. partition's HW (if it is the current log for the partition) or curre

  -- Versions: 0+
  describeLogDirsPartitionOffsetLag :: !(Int64)
,

  -- | True if this log is created by AlterReplicaLogDirsRequest and will replace the current log of the re

  -- Versions: 0+
  describeLogDirsPartitionIsFutureKey :: !(Bool)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeLogDirsPartition with version-aware field handling.
encodeDescribeLogDirsPartition :: MonadPut m => E.ApiVersion -> DescribeLogDirsPartition -> m ()
encodeDescribeLogDirsPartition version dmsg =
  do
    serialize (describeLogDirsPartitionPartitionIndex dmsg)
    serialize (describeLogDirsPartitionPartitionSize dmsg)
    serialize (describeLogDirsPartitionOffsetLag dmsg)
    serialize (describeLogDirsPartitionIsFutureKey dmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeLogDirsPartition with version-aware field handling.
decodeDescribeLogDirsPartition :: MonadGet m => E.ApiVersion -> m DescribeLogDirsPartition
decodeDescribeLogDirsPartition version =
  do
    fieldpartitionindex <- deserialize
    fieldpartitionsize <- deserialize
    fieldoffsetlag <- deserialize
    fieldisfuturekey <- deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeLogDirsPartition
      {
      describeLogDirsPartitionPartitionIndex = fieldpartitionindex
      ,
      describeLogDirsPartitionPartitionSize = fieldpartitionsize
      ,
      describeLogDirsPartitionOffsetLag = fieldoffsetlag
      ,
      describeLogDirsPartitionIsFutureKey = fieldisfuturekey
      }


-- | The topics.
data DescribeLogDirsTopic = DescribeLogDirsTopic
  {

  -- | The topic name.

  -- Versions: 0+
  describeLogDirsTopicName :: !(KafkaString)
,

  -- | The partitions.

  -- Versions: 0+
  describeLogDirsTopicPartitions :: !(KafkaArray (DescribeLogDirsPartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeLogDirsTopic with version-aware field handling.
encodeDescribeLogDirsTopic :: MonadPut m => E.ApiVersion -> DescribeLogDirsTopic -> m ()
encodeDescribeLogDirsTopic version dmsg =
  do
    if version >= 2 then serialize (toCompactString (describeLogDirsTopicName dmsg)) else serialize (describeLogDirsTopicName dmsg)
    E.encodeVersionedArray version 2 encodeDescribeLogDirsPartition (case P.unKafkaArray (describeLogDirsTopicPartitions dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeLogDirsTopic with version-aware field handling.
decodeDescribeLogDirsTopic :: MonadGet m => E.ApiVersion -> m DescribeLogDirsTopic
decodeDescribeLogDirsTopic version =
  do
    fieldname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDescribeLogDirsPartition
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeLogDirsTopic
      {
      describeLogDirsTopicName = fieldname
      ,
      describeLogDirsTopicPartitions = fieldpartitions
      }


-- | The log directories.
data DescribeLogDirsResult = DescribeLogDirsResult
  {

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  describeLogDirsResultErrorCode :: !(Int16)
,

  -- | The absolute log directory path.

  -- Versions: 0+
  describeLogDirsResultLogDir :: !(KafkaString)
,

  -- | The topics.

  -- Versions: 0+
  describeLogDirsResultTopics :: !(KafkaArray (DescribeLogDirsTopic))
,

  -- | The total size in bytes of the volume the log directory is in. This value does not include the size 

  -- Versions: 4+
  describeLogDirsResultTotalBytes :: !(Int64)
,

  -- | The usable size in bytes of the volume the log directory is in. This value does not include the size

  -- Versions: 4+
  describeLogDirsResultUsableBytes :: !(Int64)
,

  -- | True if this log directory is cordoned.

  -- Versions: 5+
  describeLogDirsResultIsCordoned :: !(Bool)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeLogDirsResult with version-aware field handling.
encodeDescribeLogDirsResult :: MonadPut m => E.ApiVersion -> DescribeLogDirsResult -> m ()
encodeDescribeLogDirsResult version dmsg =
  do
    serialize (describeLogDirsResultErrorCode dmsg)
    if version >= 2 then serialize (toCompactString (describeLogDirsResultLogDir dmsg)) else serialize (describeLogDirsResultLogDir dmsg)
    E.encodeVersionedArray version 2 encodeDescribeLogDirsTopic (case P.unKafkaArray (describeLogDirsResultTopics dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 4) $
      serialize (describeLogDirsResultTotalBytes dmsg)
    when (version >= 4) $
      serialize (describeLogDirsResultUsableBytes dmsg)
    when (version >= 5) $
      serialize (describeLogDirsResultIsCordoned dmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeLogDirsResult with version-aware field handling.
decodeDescribeLogDirsResult :: MonadGet m => E.ApiVersion -> m DescribeLogDirsResult
decodeDescribeLogDirsResult version =
  do
    fielderrorcode <- deserialize
    fieldlogdir <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDescribeLogDirsTopic
    fieldtotalbytes <- if version >= 4
      then deserialize
      else pure ((-1))
    fieldusablebytes <- if version >= 4
      then deserialize
      else pure ((-1))
    fieldiscordoned <- if version >= 5
      then deserialize
      else pure (False)
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeLogDirsResult
      {
      describeLogDirsResultErrorCode = fielderrorcode
      ,
      describeLogDirsResultLogDir = fieldlogdir
      ,
      describeLogDirsResultTopics = fieldtopics
      ,
      describeLogDirsResultTotalBytes = fieldtotalbytes
      ,
      describeLogDirsResultUsableBytes = fieldusablebytes
      ,
      describeLogDirsResultIsCordoned = fieldiscordoned
      }



data DescribeLogDirsResponse = DescribeLogDirsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeLogDirsResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 3+
  describeLogDirsResponseErrorCode :: !(Int16)
,

  -- | The log directories.

  -- Versions: 0+
  describeLogDirsResponseResults :: !(KafkaArray (DescribeLogDirsResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeLogDirsResponse.
maxDescribeLogDirsResponseVersion :: Int16
maxDescribeLogDirsResponseVersion = 5

-- | KafkaMessage instance for DescribeLogDirsResponse.
instance KafkaMessage DescribeLogDirsResponse where
  messageApiKey = 35
  messageMinVersion = 1
  messageMaxVersion = 5
  messageFlexibleVersion = Just 2

-- | Encode DescribeLogDirsResponse with the given API version.
encodeDescribeLogDirsResponse :: MonadPut m => E.ApiVersion -> DescribeLogDirsResponse -> m ()
encodeDescribeLogDirsResponse version msg
  | version == 1 =
    do
      serialize (describeLogDirsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeDescribeLogDirsResult (case P.unKafkaArray (describeLogDirsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 2 =
    do
      serialize (describeLogDirsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeDescribeLogDirsResult (case P.unKafkaArray (describeLogDirsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 3 && version <= 5 =
    do
      serialize (describeLogDirsResponseThrottleTimeMs msg)
      serialize (describeLogDirsResponseErrorCode msg)
      E.encodeVersionedArray version 2 encodeDescribeLogDirsResult (case P.unKafkaArray (describeLogDirsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeLogDirsResponse with the given API version.
decodeDescribeLogDirsResponse :: MonadGet m => E.ApiVersion -> m DescribeLogDirsResponse
decodeDescribeLogDirsResponse version
  | version == 1 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDescribeLogDirsResult
      pure DescribeLogDirsResponse
        {
        describeLogDirsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeLogDirsResponseErrorCode = 0
        ,
        describeLogDirsResponseResults = fieldresults
        }

  | version == 2 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDescribeLogDirsResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeLogDirsResponse
        {
        describeLogDirsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeLogDirsResponseErrorCode = 0
        ,
        describeLogDirsResponseResults = fieldresults
        }

  | version >= 3 && version <= 5 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeDescribeLogDirsResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeLogDirsResponse
        {
        describeLogDirsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeLogDirsResponseErrorCode = fielderrorcode
        ,
        describeLogDirsResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a DescribeLogDirsPartition.
wireMaxSizeDescribeLogDirsPartition :: Int -> DescribeLogDirsPartition -> Int
wireMaxSizeDescribeLogDirsPartition _version msg =
  0
  + 4
  + 8
  + 8
  + 1
  + 1

-- | Direct-poke encoder for DescribeLogDirsPartition.
wirePokeDescribeLogDirsPartition :: Int -> Ptr Word8 -> DescribeLogDirsPartition -> IO (Ptr Word8)
wirePokeDescribeLogDirsPartition version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (describeLogDirsPartitionPartitionIndex msg)
  p2 <- W.pokeInt64BE p1 (describeLogDirsPartitionPartitionSize msg)
  p3 <- W.pokeInt64BE p2 (describeLogDirsPartitionOffsetLag msg)
  p4 <- W.pokeWord8 p3 (if (describeLogDirsPartitionIsFutureKey msg) then 1 else 0)
  if version >= 2 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for DescribeLogDirsPartition.
wirePeekDescribeLogDirsPartition :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeLogDirsPartition, Ptr Word8)
wirePeekDescribeLogDirsPartition version _fp _basePtr p0 endPtr = do
  (f0_partitionindex, p1) <- W.peekInt32BE p0 endPtr
  (f1_partitionsize, p2) <- W.peekInt64BE p1 endPtr
  (f2_offsetlag, p3) <- W.peekInt64BE p2 endPtr
  (f3_isfuturekey, p4) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (DescribeLogDirsPartition { describeLogDirsPartitionPartitionIndex = f0_partitionindex, describeLogDirsPartitionPartitionSize = f1_partitionsize, describeLogDirsPartitionOffsetLag = f2_offsetlag, describeLogDirsPartitionIsFutureKey = f3_isfuturekey }, pTagsEnd)

-- | Worst-case wire size of a DescribeLogDirsTopic.
wireMaxSizeDescribeLogDirsTopic :: Int -> DescribeLogDirsTopic -> Int
wireMaxSizeDescribeLogDirsTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (describeLogDirsTopicName msg))
  + (5 + (case P.unKafkaArray (describeLogDirsTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeLogDirsPartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeLogDirsTopic.
wirePokeDescribeLogDirsTopic :: Int -> Ptr Word8 -> DescribeLogDirsTopic -> IO (Ptr Word8)
wirePokeDescribeLogDirsTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (describeLogDirsTopicName msg))
  p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDescribeLogDirsPartition version p x) p1 (describeLogDirsTopicPartitions msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DescribeLogDirsTopic.
wirePeekDescribeLogDirsTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeLogDirsTopic, Ptr Word8)
wirePeekDescribeLogDirsTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDescribeLogDirsPartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DescribeLogDirsTopic { describeLogDirsTopicName = f0_name, describeLogDirsTopicPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a DescribeLogDirsResult.
wireMaxSizeDescribeLogDirsResult :: Int -> DescribeLogDirsResult -> Int
wireMaxSizeDescribeLogDirsResult _version msg =
  0
  + 2
  + WP.compactStringMaxSize (P.toCompactString (describeLogDirsResultLogDir msg))
  + (5 + (case P.unKafkaArray (describeLogDirsResultTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeLogDirsTopic _version x ) v); P.Null -> 0 }))
  + 8
  + 8
  + 1
  + 1

-- | Direct-poke encoder for DescribeLogDirsResult.
wirePokeDescribeLogDirsResult :: Int -> Ptr Word8 -> DescribeLogDirsResult -> IO (Ptr Word8)
wirePokeDescribeLogDirsResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (describeLogDirsResultErrorCode msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (describeLogDirsResultLogDir msg))
  p3 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDescribeLogDirsTopic version p x) p2 (describeLogDirsResultTopics msg)
  p4 <- W.pokeInt64BE p3 (describeLogDirsResultTotalBytes msg)
  p5 <- W.pokeInt64BE p4 (describeLogDirsResultUsableBytes msg)
  p6 <- W.pokeWord8 p5 (if (describeLogDirsResultIsCordoned msg) then 1 else 0)
  if version >= 2 then WP.pokeEmptyTaggedFields p6 else pure p6

-- | Direct-poke decoder for DescribeLogDirsResult.
wirePeekDescribeLogDirsResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeLogDirsResult, Ptr Word8)
wirePeekDescribeLogDirsResult version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_logdir, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_topics, p3) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDescribeLogDirsTopic version _fp _basePtr p e) p2 endPtr
  (f3_totalbytes, p4) <- W.peekInt64BE p3 endPtr
  (f4_usablebytes, p5) <- W.peekInt64BE p4 endPtr
  (f5_iscordoned, p6) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p5 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p6 endPtr else pure p6
  pure (DescribeLogDirsResult { describeLogDirsResultErrorCode = f0_errorcode, describeLogDirsResultLogDir = f1_logdir, describeLogDirsResultTopics = f2_topics, describeLogDirsResultTotalBytes = f3_totalbytes, describeLogDirsResultUsableBytes = f4_usablebytes, describeLogDirsResultIsCordoned = f5_iscordoned }, pTagsEnd)

-- | Worst-case wire size of a DescribeLogDirsResponse.
wireMaxSizeDescribeLogDirsResponse :: Int -> DescribeLogDirsResponse -> Int
wireMaxSizeDescribeLogDirsResponse _version msg =
  0
  + 4
  + 2
  + (5 + (case P.unKafkaArray (describeLogDirsResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeLogDirsResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeLogDirsResponse.
wirePokeDescribeLogDirsResponse :: Int -> Ptr Word8 -> DescribeLogDirsResponse -> IO (Ptr Word8)
wirePokeDescribeLogDirsResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeLogDirsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDescribeLogDirsResult version p x) p1 (describeLogDirsResponseResults msg)
    pure p2
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeLogDirsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDescribeLogDirsResult version p x) p1 (describeLogDirsResponseResults msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 3 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeLogDirsResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (describeLogDirsResponseErrorCode msg)
    p3 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDescribeLogDirsResult version p x) p2 (describeLogDirsResponseResults msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke DescribeLogDirsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeLogDirsResponse.
wirePeekDescribeLogDirsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeLogDirsResponse, Ptr Word8)
wirePeekDescribeLogDirsResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_results, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDescribeLogDirsResult version _fp _basePtr p e) p1 endPtr
    pure (DescribeLogDirsResponse { describeLogDirsResponseThrottleTimeMs = f0_throttletimems, describeLogDirsResponseErrorCode = 0, describeLogDirsResponseResults = f1_results }, p2)
  | version == 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_results, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDescribeLogDirsResult version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DescribeLogDirsResponse { describeLogDirsResponseThrottleTimeMs = f0_throttletimems, describeLogDirsResponseErrorCode = 0, describeLogDirsResponseResults = f1_results }, pTagsEnd)
  | version >= 3 && version <= 5 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_results, p3) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDescribeLogDirsResult version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (DescribeLogDirsResponse { describeLogDirsResponseThrottleTimeMs = f0_throttletimems, describeLogDirsResponseErrorCode = f1_errorcode, describeLogDirsResponseResults = f2_results }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeLogDirsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeLogDirsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeLogDirsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeLogDirsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeLogDirsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}