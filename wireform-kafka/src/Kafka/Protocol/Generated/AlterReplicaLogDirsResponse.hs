{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterReplicaLogDirsResponse
Description : Kafka AlterReplicaLogDirsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 34.



Valid versions: 1-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterReplicaLogDirsResponse
  (
    AlterReplicaLogDirsResponse(..),
    AlterReplicaLogDirTopicResult(..),
    AlterReplicaLogDirPartitionResult(..),
    encodeAlterReplicaLogDirsResponse,
    decodeAlterReplicaLogDirsResponse,
    maxAlterReplicaLogDirsResponseVersion
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


-- | The results for each partition.
data AlterReplicaLogDirPartitionResult = AlterReplicaLogDirPartitionResult
  {

  -- | The partition index.

  -- Versions: 0+
  alterReplicaLogDirPartitionResultPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  alterReplicaLogDirPartitionResultErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterReplicaLogDirPartitionResult with version-aware field handling.
encodeAlterReplicaLogDirPartitionResult :: MonadPut m => E.ApiVersion -> AlterReplicaLogDirPartitionResult -> m ()
encodeAlterReplicaLogDirPartitionResult version amsg =
  do
    serialize (alterReplicaLogDirPartitionResultPartitionIndex amsg)
    serialize (alterReplicaLogDirPartitionResultErrorCode amsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterReplicaLogDirPartitionResult with version-aware field handling.
decodeAlterReplicaLogDirPartitionResult :: MonadGet m => E.ApiVersion -> m AlterReplicaLogDirPartitionResult
decodeAlterReplicaLogDirPartitionResult version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterReplicaLogDirPartitionResult
      {
      alterReplicaLogDirPartitionResultPartitionIndex = fieldpartitionindex
      ,
      alterReplicaLogDirPartitionResultErrorCode = fielderrorcode
      }


-- | The results for each topic.
data AlterReplicaLogDirTopicResult = AlterReplicaLogDirTopicResult
  {

  -- | The name of the topic.

  -- Versions: 0+
  alterReplicaLogDirTopicResultTopicName :: !(KafkaString)
,

  -- | The results for each partition.

  -- Versions: 0+
  alterReplicaLogDirTopicResultPartitions :: !(KafkaArray (AlterReplicaLogDirPartitionResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterReplicaLogDirTopicResult with version-aware field handling.
encodeAlterReplicaLogDirTopicResult :: MonadPut m => E.ApiVersion -> AlterReplicaLogDirTopicResult -> m ()
encodeAlterReplicaLogDirTopicResult version amsg =
  do
    if version >= 2 then serialize (toCompactString (alterReplicaLogDirTopicResultTopicName amsg)) else serialize (alterReplicaLogDirTopicResultTopicName amsg)
    E.encodeVersionedArray version 2 encodeAlterReplicaLogDirPartitionResult (case P.unKafkaArray (alterReplicaLogDirTopicResultPartitions amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterReplicaLogDirTopicResult with version-aware field handling.
decodeAlterReplicaLogDirTopicResult :: MonadGet m => E.ApiVersion -> m AlterReplicaLogDirTopicResult
decodeAlterReplicaLogDirTopicResult version =
  do
    fieldtopicname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAlterReplicaLogDirPartitionResult
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterReplicaLogDirTopicResult
      {
      alterReplicaLogDirTopicResultTopicName = fieldtopicname
      ,
      alterReplicaLogDirTopicResultPartitions = fieldpartitions
      }



data AlterReplicaLogDirsResponse = AlterReplicaLogDirsResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  alterReplicaLogDirsResponseThrottleTimeMs :: !(Int32)
,

  -- | The results for each topic.

  -- Versions: 0+
  alterReplicaLogDirsResponseResults :: !(KafkaArray (AlterReplicaLogDirTopicResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterReplicaLogDirsResponse.
maxAlterReplicaLogDirsResponseVersion :: Int16
maxAlterReplicaLogDirsResponseVersion = 2

-- | Encode AlterReplicaLogDirsResponse with the given API version.
encodeAlterReplicaLogDirsResponse :: MonadPut m => E.ApiVersion -> AlterReplicaLogDirsResponse -> m ()
encodeAlterReplicaLogDirsResponse version msg
  | version == 1 =
    do
      serialize (alterReplicaLogDirsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeAlterReplicaLogDirTopicResult (case P.unKafkaArray (alterReplicaLogDirsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 2 =
    do
      serialize (alterReplicaLogDirsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeAlterReplicaLogDirTopicResult (case P.unKafkaArray (alterReplicaLogDirsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterReplicaLogDirsResponse with the given API version.
decodeAlterReplicaLogDirsResponse :: MonadGet m => E.ApiVersion -> m AlterReplicaLogDirsResponse
decodeAlterReplicaLogDirsResponse version
  | version == 1 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAlterReplicaLogDirTopicResult
      pure AlterReplicaLogDirsResponse
        {
        alterReplicaLogDirsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterReplicaLogDirsResponseResults = fieldresults
        }

  | version == 2 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAlterReplicaLogDirTopicResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterReplicaLogDirsResponse
        {
        alterReplicaLogDirsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterReplicaLogDirsResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec AlterReplicaLogDirsResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
