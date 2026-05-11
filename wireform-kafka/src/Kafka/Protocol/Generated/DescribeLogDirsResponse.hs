{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeLogDirsResponse
Description : Kafka DescribeLogDirsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 35.



Valid versions: 1-4
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeLogDirsResponse
  (
    DescribeLogDirsResponse(..),
    DescribeLogDirsResult(..),
    DescribeLogDirsTopic(..),
    DescribeLogDirsPartition(..),
    maxDescribeLogDirsResponseVersion
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , Nullable(..)
  )
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

  -- | The total size in bytes of the volume the log directory is in.

  -- Versions: 4+
  describeLogDirsResultTotalBytes :: !(Int64)
,

  -- | The usable size in bytes of the volume the log directory is in.

  -- Versions: 4+
  describeLogDirsResultUsableBytes :: !(Int64)

  }
  deriving (Eq, Show, Generic)


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
maxDescribeLogDirsResponseVersion = 4

-- | KafkaMessage instance for DescribeLogDirsResponse.
instance KafkaMessage DescribeLogDirsResponse where
  messageApiKey = 35
  messageMinVersion = 1
  messageMaxVersion = 4
  messageFlexibleVersion = Just 2

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

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribeLogDirsPartition :: DescribeLogDirsPartition
defaultDescribeLogDirsPartition = DescribeLogDirsPartition { describeLogDirsPartitionPartitionIndex = 0, describeLogDirsPartitionPartitionSize = 0, describeLogDirsPartitionOffsetLag = 0, describeLogDirsPartitionIsFutureKey = False }

-- | Worst-case wire size of a DescribeLogDirsTopic.
wireMaxSizeDescribeLogDirsTopic :: Int -> DescribeLogDirsTopic -> Int
wireMaxSizeDescribeLogDirsTopic _version msg =
  0
  + WP.dualStringMaxSize (describeLogDirsTopicName msg)
  + (5 + (case P.unKafkaArray (describeLogDirsTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeLogDirsPartition _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeLogDirsTopic.
wirePokeDescribeLogDirsTopic :: Int -> Ptr Word8 -> DescribeLogDirsTopic -> IO (Ptr Word8)
wirePokeDescribeLogDirsTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 2 then WP.pokeCompactString p0 (P.toCompactString (describeLogDirsTopicName msg)) else WP.pokeKafkaString p0 (describeLogDirsTopicName msg))
  p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDescribeLogDirsPartition version p x) p1 (describeLogDirsTopicPartitions msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DescribeLogDirsTopic.
wirePeekDescribeLogDirsTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeLogDirsTopic, Ptr Word8)
wirePeekDescribeLogDirsTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_partitions, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDescribeLogDirsPartition version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DescribeLogDirsTopic { describeLogDirsTopicName = f0_name, describeLogDirsTopicPartitions = f1_partitions }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribeLogDirsTopic :: DescribeLogDirsTopic
defaultDescribeLogDirsTopic = DescribeLogDirsTopic { describeLogDirsTopicName = P.KafkaString Null, describeLogDirsTopicPartitions = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a DescribeLogDirsResult.
wireMaxSizeDescribeLogDirsResult :: Int -> DescribeLogDirsResult -> Int
wireMaxSizeDescribeLogDirsResult _version msg =
  0
  + 2
  + WP.dualStringMaxSize (describeLogDirsResultLogDir msg)
  + (5 + (case P.unKafkaArray (describeLogDirsResultTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeLogDirsTopic _version x ) v); P.Null -> 0 }))
  + 8
  + 8
  + 1

-- | Direct-poke encoder for DescribeLogDirsResult.
wirePokeDescribeLogDirsResult :: Int -> Ptr Word8 -> DescribeLogDirsResult -> IO (Ptr Word8)
wirePokeDescribeLogDirsResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (describeLogDirsResultErrorCode msg)
  p2 <- (if version >= 2 then WP.pokeCompactString p1 (P.toCompactString (describeLogDirsResultLogDir msg)) else WP.pokeKafkaString p1 (describeLogDirsResultLogDir msg))
  p3 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDescribeLogDirsTopic version p x) p2 (describeLogDirsResultTopics msg)
  p4 <- (if version >= 4 then W.pokeInt64BE p3 (describeLogDirsResultTotalBytes msg) else pure p3)
  p5 <- (if version >= 4 then W.pokeInt64BE p4 (describeLogDirsResultUsableBytes msg) else pure p4)
  if version >= 2 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for DescribeLogDirsResult.
wirePeekDescribeLogDirsResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeLogDirsResult, Ptr Word8)
wirePeekDescribeLogDirsResult version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_logdir, p2) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_topics, p3) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDescribeLogDirsTopic version _fp _basePtr p e) p2 endPtr
  (f3_totalbytes, p4) <- (if version >= 4 then W.peekInt64BE p3 endPtr else pure (-1, p3))
  (f4_usablebytes, p5) <- (if version >= 4 then W.peekInt64BE p4 endPtr else pure (-1, p4))
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (DescribeLogDirsResult { describeLogDirsResultErrorCode = f0_errorcode, describeLogDirsResultLogDir = f1_logdir, describeLogDirsResultTopics = f2_topics, describeLogDirsResultTotalBytes = f3_totalbytes, describeLogDirsResultUsableBytes = f4_usablebytes }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribeLogDirsResult :: DescribeLogDirsResult
defaultDescribeLogDirsResult = DescribeLogDirsResult { describeLogDirsResultErrorCode = 0, describeLogDirsResultLogDir = P.KafkaString Null, describeLogDirsResultTopics = P.mkKafkaArray V.empty, describeLogDirsResultTotalBytes = -1, describeLogDirsResultUsableBytes = -1 }

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
  | version >= 3 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeLogDirsResponseThrottleTimeMs msg)
    p2 <- (if version >= 3 then W.pokeInt16BE p1 (describeLogDirsResponseErrorCode msg) else pure p1)
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
  | version >= 3 && version <= 4 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- (if version >= 3 then W.peekInt16BE p1 endPtr else pure (0, p1))
    (f2_results, p3) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDescribeLogDirsResult version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (DescribeLogDirsResponse { describeLogDirsResponseThrottleTimeMs = f0_throttletimems, describeLogDirsResponseErrorCode = f1_errorcode, describeLogDirsResponseResults = f2_results }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeLogDirsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec DescribeLogDirsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeLogDirsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeLogDirsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeLogDirsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}