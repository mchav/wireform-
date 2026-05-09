{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.OffsetDeleteResponse
Description : Kafka OffsetDeleteResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 47.



Valid versions: 0
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.OffsetDeleteResponse
  (
    OffsetDeleteResponse(..),
    OffsetDeleteResponseTopic(..),
    OffsetDeleteResponsePartition(..),
    encodeOffsetDeleteResponse,
    decodeOffsetDeleteResponse,
    maxOffsetDeleteResponseVersion
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
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC


-- | The responses for each partition in the topic.
data OffsetDeleteResponsePartition = OffsetDeleteResponsePartition
  {

  -- | The partition index.

  -- Versions: 0+
  offsetDeleteResponsePartitionPartitionIndex :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  offsetDeleteResponsePartitionErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetDeleteResponsePartition with version-aware field handling.
encodeOffsetDeleteResponsePartition :: MonadPut m => E.ApiVersion -> OffsetDeleteResponsePartition -> m ()
encodeOffsetDeleteResponsePartition _version omsg =
  do
    serialize (offsetDeleteResponsePartitionPartitionIndex omsg)
    serialize (offsetDeleteResponsePartitionErrorCode omsg)


-- | Decode OffsetDeleteResponsePartition with version-aware field handling.
decodeOffsetDeleteResponsePartition :: MonadGet m => E.ApiVersion -> m OffsetDeleteResponsePartition
decodeOffsetDeleteResponsePartition _version =
  do
    fieldpartitionindex <- deserialize
    fielderrorcode <- deserialize
    pure OffsetDeleteResponsePartition
      {
      offsetDeleteResponsePartitionPartitionIndex = fieldpartitionindex
      ,
      offsetDeleteResponsePartitionErrorCode = fielderrorcode
      }


-- | The responses for each topic.
data OffsetDeleteResponseTopic = OffsetDeleteResponseTopic
  {

  -- | The topic name.

  -- Versions: 0+
  offsetDeleteResponseTopicName :: !(KafkaString)
,

  -- | The responses for each partition in the topic.

  -- Versions: 0+
  offsetDeleteResponseTopicPartitions :: !(KafkaArray (OffsetDeleteResponsePartition))

  }
  deriving (Eq, Show, Generic)


-- | Encode OffsetDeleteResponseTopic with version-aware field handling.
encodeOffsetDeleteResponseTopic :: MonadPut m => E.ApiVersion -> OffsetDeleteResponseTopic -> m ()
encodeOffsetDeleteResponseTopic version omsg =
  do
    serialize (offsetDeleteResponseTopicName omsg)
    E.encodeVersionedArray version 999 encodeOffsetDeleteResponsePartition (case P.unKafkaArray (offsetDeleteResponseTopicPartitions omsg) of { P.NotNull v -> v; P.Null -> V.empty })


-- | Decode OffsetDeleteResponseTopic with version-aware field handling.
decodeOffsetDeleteResponseTopic :: MonadGet m => E.ApiVersion -> m OffsetDeleteResponseTopic
decodeOffsetDeleteResponseTopic version =
  do
    fieldname <- deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeOffsetDeleteResponsePartition
    pure OffsetDeleteResponseTopic
      {
      offsetDeleteResponseTopicName = fieldname
      ,
      offsetDeleteResponseTopicPartitions = fieldpartitions
      }



data OffsetDeleteResponse = OffsetDeleteResponse
  {

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 0+
  offsetDeleteResponseErrorCode :: !(Int16)
,

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  offsetDeleteResponseThrottleTimeMs :: !(Int32)
,

  -- | The responses for each topic.

  -- Versions: 0+
  offsetDeleteResponseTopics :: !(KafkaArray (OffsetDeleteResponseTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for OffsetDeleteResponse.
maxOffsetDeleteResponseVersion :: Int16
maxOffsetDeleteResponseVersion = 0

-- | KafkaMessage instance for OffsetDeleteResponse.
instance KafkaMessage OffsetDeleteResponse where
  messageApiKey = 47
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Nothing

-- | Encode OffsetDeleteResponse with the given API version.
encodeOffsetDeleteResponse :: MonadPut m => E.ApiVersion -> OffsetDeleteResponse -> m ()
encodeOffsetDeleteResponse version msg
  | version == 0 =
    do
      serialize (offsetDeleteResponseErrorCode msg)
      serialize (offsetDeleteResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 999 encodeOffsetDeleteResponseTopic (case P.unKafkaArray (offsetDeleteResponseTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode OffsetDeleteResponse with the given API version.
decodeOffsetDeleteResponse :: MonadGet m => E.ApiVersion -> m OffsetDeleteResponse
decodeOffsetDeleteResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      fieldthrottletimems <- deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeOffsetDeleteResponseTopic
      pure OffsetDeleteResponse
        {
        offsetDeleteResponseErrorCode = fielderrorcode
        ,
        offsetDeleteResponseThrottleTimeMs = fieldthrottletimems
        ,
        offsetDeleteResponseTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec OffsetDeleteResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
