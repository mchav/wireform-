{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeProducersRequest
Description : Kafka DescribeProducersRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 61.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeProducersRequest
  (
    DescribeProducersRequest(..),
    TopicRequest(..),
    encodeDescribeProducersRequest,
    decodeDescribeProducersRequest,
    maxDescribeProducersRequestVersion
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


-- | The topics to list producers for.
data TopicRequest = TopicRequest
  {

  -- | The topic name.

  -- Versions: 0+
  topicRequestName :: !(KafkaString)
,

  -- | The indexes of the partitions to list producers for.

  -- Versions: 0+
  topicRequestPartitionIndexes :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode TopicRequest with version-aware field handling.
encodeTopicRequest :: MonadPut m => E.ApiVersion -> TopicRequest -> m ()
encodeTopicRequest version tmsg =
  do
    if version >= 0 then serialize (toCompactString (topicRequestName tmsg)) else serialize (topicRequestName tmsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (topicRequestPartitionIndexes tmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode TopicRequest with version-aware field handling.
decodeTopicRequest :: MonadGet m => E.ApiVersion -> m TopicRequest
decodeTopicRequest version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitionindexes <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure TopicRequest
      {
      topicRequestName = fieldname
      ,
      topicRequestPartitionIndexes = fieldpartitionindexes
      }



data DescribeProducersRequest = DescribeProducersRequest
  {

  -- | The topics to list producers for.

  -- Versions: 0+
  describeProducersRequestTopics :: !(KafkaArray (TopicRequest))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeProducersRequest.
maxDescribeProducersRequestVersion :: Int16
maxDescribeProducersRequestVersion = 0

-- | Encode DescribeProducersRequest with the given API version.
encodeDescribeProducersRequest :: MonadPut m => E.ApiVersion -> DescribeProducersRequest -> m ()
encodeDescribeProducersRequest version msg
  | version == 0 =
    do
      E.encodeVersionedArray version 0 encodeTopicRequest (case P.unKafkaArray (describeProducersRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeProducersRequest with the given API version.
decodeDescribeProducersRequest :: MonadGet m => E.ApiVersion -> m DescribeProducersRequest
decodeDescribeProducersRequest version
  | version == 0 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeTopicRequest
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeProducersRequest
        {
        describeProducersRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeProducersRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
