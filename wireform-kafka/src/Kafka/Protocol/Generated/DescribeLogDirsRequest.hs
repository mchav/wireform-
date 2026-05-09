{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeLogDirsRequest
Description : Kafka DescribeLogDirsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 35.



Valid versions: 1-4
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeLogDirsRequest
  (
    DescribeLogDirsRequest(..),
    DescribableLogDirTopic(..),
    encodeDescribeLogDirsRequest,
    decodeDescribeLogDirsRequest,
    maxDescribeLogDirsRequestVersion
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


-- | Each topic that we want to describe log directories for, or null for all topics.
data DescribableLogDirTopic = DescribableLogDirTopic
  {

  -- | The topic name.

  -- Versions: 0+
  describableLogDirTopicTopic :: !(KafkaString)
,

  -- | The partition indexes.

  -- Versions: 0+
  describableLogDirTopicPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribableLogDirTopic with version-aware field handling.
encodeDescribableLogDirTopic :: MonadPut m => E.ApiVersion -> DescribableLogDirTopic -> m ()
encodeDescribableLogDirTopic version dmsg =
  do
    if version >= 2 then serialize (toCompactString (describableLogDirTopicTopic dmsg)) else serialize (describableLogDirTopicTopic dmsg)
    E.encodeVersionedArray version 2 (\_ x -> serialize x) (case P.unKafkaArray (describableLogDirTopicPartitions dmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribableLogDirTopic with version-aware field handling.
decodeDescribableLogDirTopic :: MonadGet m => E.ApiVersion -> m DescribableLogDirTopic
decodeDescribableLogDirTopic version =
  do
    fieldtopic <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 (\_ -> deserialize)
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribableLogDirTopic
      {
      describableLogDirTopicTopic = fieldtopic
      ,
      describableLogDirTopicPartitions = fieldpartitions
      }



data DescribeLogDirsRequest = DescribeLogDirsRequest
  {

  -- | Each topic that we want to describe log directories for, or null for all topics.

  -- Versions: 0+
  describeLogDirsRequestTopics :: !(KafkaArray (DescribableLogDirTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeLogDirsRequest.
maxDescribeLogDirsRequestVersion :: Int16
maxDescribeLogDirsRequestVersion = 4

-- | Encode DescribeLogDirsRequest with the given API version.
encodeDescribeLogDirsRequest :: MonadPut m => E.ApiVersion -> DescribeLogDirsRequest -> m ()
encodeDescribeLogDirsRequest version msg
  | version == 1 =
    do
      E.encodeVersionedNullableArray version 2 encodeDescribableLogDirTopic (describeLogDirsRequestTopics msg)


  | version >= 2 && version <= 4 =
    do
      E.encodeVersionedNullableArray version 2 encodeDescribableLogDirTopic (describeLogDirsRequestTopics msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeLogDirsRequest with the given API version.
decodeDescribeLogDirsRequest :: MonadGet m => E.ApiVersion -> m DescribeLogDirsRequest
decodeDescribeLogDirsRequest version
  | version == 1 =
    do
      fieldtopics <- E.decodeVersionedNullableArray version 2 decodeDescribableLogDirTopic
      pure DescribeLogDirsRequest
        {
        describeLogDirsRequestTopics = fieldtopics
        }

  | version >= 2 && version <= 4 =
    do
      fieldtopics <- E.decodeVersionedNullableArray version 2 decodeDescribableLogDirTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeLogDirsRequest
        {
        describeLogDirsRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeDescribeLogDirsRequest' / 'decodeDescribeLogDirsRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec DescribeLogDirsRequest where
  wireCodec = Just (WC.serialShimCodec encodeDescribeLogDirsRequest decodeDescribeLogDirsRequest)
  {-# INLINE wireCodec #-}
