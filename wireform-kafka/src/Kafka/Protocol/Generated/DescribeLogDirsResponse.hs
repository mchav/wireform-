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
    encodeDescribeLogDirsResponse,
    decodeDescribeLogDirsResponse,
    maxDescribeLogDirsResponseVersion
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
maxDescribeLogDirsResponseVersion = 4

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

  | version >= 3 && version <= 4 =
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

  | version >= 3 && version <= 4 =
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

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeDescribeLogDirsResponse' / 'decodeDescribeLogDirsResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec DescribeLogDirsResponse where
  wireCodec = Just (WC.serialShimCodec encodeDescribeLogDirsResponse decodeDescribeLogDirsResponse)
  {-# INLINE wireCodec #-}
