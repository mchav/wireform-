{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterShareGroupOffsetsResponse
Description : Kafka AlterShareGroupOffsetsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 91.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterShareGroupOffsetsResponse
  (
    AlterShareGroupOffsetsResponse(..),
    AlterShareGroupOffsetsResponseTopic(..),
    AlterShareGroupOffsetsResponsePartition(..),
    encodeAlterShareGroupOffsetsResponse,
    decodeAlterShareGroupOffsetsResponse,
    maxAlterShareGroupOffsetsResponseVersion
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



data AlterShareGroupOffsetsResponsePartition = AlterShareGroupOffsetsResponsePartition
  {

  -- | The partition index.

  -- Versions: 0+
  alterShareGroupOffsetsResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  alterShareGroupOffsetsResponsePartitionErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 0+
  alterShareGroupOffsetsResponsePartitionErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterShareGroupOffsetsResponsePartition with version-aware field handling.
encodeAlterShareGroupOffsetsResponsePartition :: MonadPut m => E.ApiVersion -> AlterShareGroupOffsetsResponsePartition -> m ()
encodeAlterShareGroupOffsetsResponsePartition version amsg =
  do
    serialize (alterShareGroupOffsetsResponsePartitionPartitionIndex amsg)
    serialize (alterShareGroupOffsetsResponsePartitionErrorCode amsg)
    if version >= 0 then serialize (toCompactString (alterShareGroupOffsetsResponsePartitionErrorMessage amsg)) else serialize (alterShareGroupOffsetsResponsePartitionErrorMessage amsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterShareGroupOffsetsResponsePartition with version-aware field handling.
decodeAlterShareGroupOffsetsResponsePartition :: MonadGet m => E.ApiVersion -> m AlterShareGroupOffsetsResponsePartition
decodeAlterShareGroupOffsetsResponsePartition version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterShareGroupOffsetsResponsePartition
      {
      alterShareGroupOffsetsResponsePartitionPartitionIndex = fieldpartitionindex
      ,
      alterShareGroupOffsetsResponsePartitionErrorCode = fielderrorcode
      ,
      alterShareGroupOffsetsResponsePartitionErrorMessage = fielderrormessage
      }


-- | The results for each topic.
data AlterShareGroupOffsetsResponseTopic = AlterShareGroupOffsetsResponseTopic
  {

  -- | The topic name.

  -- Versions: 0+
  alterShareGroupOffsetsResponseTopicTopicName :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 0+
  alterShareGroupOffsetsResponseTopicTopicId :: !(KafkaUuid)
,


  -- Versions: 0+
  alterShareGroupOffsetsResponseTopicPartitions :: !(KafkaArray (AlterShareGroupOffsetsResponsePartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterShareGroupOffsetsResponseTopic with version-aware field handling.
encodeAlterShareGroupOffsetsResponseTopic :: MonadPut m => E.ApiVersion -> AlterShareGroupOffsetsResponseTopic -> m ()
encodeAlterShareGroupOffsetsResponseTopic version amsg =
  do
    if version >= 0 then serialize (toCompactString (alterShareGroupOffsetsResponseTopicTopicName amsg)) else serialize (alterShareGroupOffsetsResponseTopicTopicName amsg)
    serialize (alterShareGroupOffsetsResponseTopicTopicId amsg)
    E.encodeVersionedArray version 0 encodeAlterShareGroupOffsetsResponsePartition (case P.unKafkaArray (alterShareGroupOffsetsResponseTopicPartitions amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterShareGroupOffsetsResponseTopic with version-aware field handling.
decodeAlterShareGroupOffsetsResponseTopic :: MonadGet m => E.ApiVersion -> m AlterShareGroupOffsetsResponseTopic
decodeAlterShareGroupOffsetsResponseTopic version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldtopicid <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAlterShareGroupOffsetsResponsePartition
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterShareGroupOffsetsResponseTopic
      {
      alterShareGroupOffsetsResponseTopicTopicName = fieldtopicname
      ,
      alterShareGroupOffsetsResponseTopicTopicId = fieldtopicid
      ,
      alterShareGroupOffsetsResponseTopicPartitions = fieldpartitions
      }



data AlterShareGroupOffsetsResponse = AlterShareGroupOffsetsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  alterShareGroupOffsetsResponseThrottleTimeMs :: !(Int32)
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 0+
  alterShareGroupOffsetsResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  alterShareGroupOffsetsResponseErrorMessage :: !(KafkaString)
,

  -- | The results for each topic.

  -- Versions: 0+
  alterShareGroupOffsetsResponseResponses :: !(KafkaArray (AlterShareGroupOffsetsResponseTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterShareGroupOffsetsResponse.
maxAlterShareGroupOffsetsResponseVersion :: Int16
maxAlterShareGroupOffsetsResponseVersion = 0

-- | Encode AlterShareGroupOffsetsResponse with the given API version.
encodeAlterShareGroupOffsetsResponse :: MonadPut m => E.ApiVersion -> AlterShareGroupOffsetsResponse -> m ()
encodeAlterShareGroupOffsetsResponse version msg
  | version == 0 =
    do
      serialize (alterShareGroupOffsetsResponseThrottleTimeMs msg)
      serialize (alterShareGroupOffsetsResponseErrorCode msg)
      serialize (toCompactString (alterShareGroupOffsetsResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeAlterShareGroupOffsetsResponseTopic (case P.unKafkaArray (alterShareGroupOffsetsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterShareGroupOffsetsResponse with the given API version.
decodeAlterShareGroupOffsetsResponse :: MonadGet m => E.ApiVersion -> m AlterShareGroupOffsetsResponse
decodeAlterShareGroupOffsetsResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeAlterShareGroupOffsetsResponseTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterShareGroupOffsetsResponse
        {
        alterShareGroupOffsetsResponseThrottleTimeMs = fieldthrottletimems
        ,
        alterShareGroupOffsetsResponseErrorCode = fielderrorcode
        ,
        alterShareGroupOffsetsResponseErrorMessage = fielderrormessage
        ,
        alterShareGroupOffsetsResponseResponses = fieldresponses
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec AlterShareGroupOffsetsResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
