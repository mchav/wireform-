{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteShareGroupOffsetsRequest
Description : Kafka DeleteShareGroupOffsetsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 92.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteShareGroupOffsetsRequest
  (
    DeleteShareGroupOffsetsRequest(..),
    DeleteShareGroupOffsetsRequestTopic(..),
    encodeDeleteShareGroupOffsetsRequest,
    decodeDeleteShareGroupOffsetsRequest,
    maxDeleteShareGroupOffsetsRequestVersion
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


-- | The topics to delete offsets for.
data DeleteShareGroupOffsetsRequestTopic = DeleteShareGroupOffsetsRequestTopic
  {

  -- | The topic name.

  -- Versions: 0+
  deleteShareGroupOffsetsRequestTopicTopicName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteShareGroupOffsetsRequestTopic with version-aware field handling.
encodeDeleteShareGroupOffsetsRequestTopic :: MonadPut m => E.ApiVersion -> DeleteShareGroupOffsetsRequestTopic -> m ()
encodeDeleteShareGroupOffsetsRequestTopic version dmsg =
  do
    if version >= 0 then serialize (toCompactString (deleteShareGroupOffsetsRequestTopicTopicName dmsg)) else serialize (deleteShareGroupOffsetsRequestTopicTopicName dmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteShareGroupOffsetsRequestTopic with version-aware field handling.
decodeDeleteShareGroupOffsetsRequestTopic :: MonadGet m => E.ApiVersion -> m DeleteShareGroupOffsetsRequestTopic
decodeDeleteShareGroupOffsetsRequestTopic version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteShareGroupOffsetsRequestTopic
      {
      deleteShareGroupOffsetsRequestTopicTopicName = fieldtopicname
      }



data DeleteShareGroupOffsetsRequest = DeleteShareGroupOffsetsRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  deleteShareGroupOffsetsRequestGroupId :: !(KafkaString)
,

  -- | The topics to delete offsets for.

  -- Versions: 0+
  deleteShareGroupOffsetsRequestTopics :: !(KafkaArray (DeleteShareGroupOffsetsRequestTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteShareGroupOffsetsRequest.
maxDeleteShareGroupOffsetsRequestVersion :: Int16
maxDeleteShareGroupOffsetsRequestVersion = 0

-- | Encode DeleteShareGroupOffsetsRequest with the given API version.
encodeDeleteShareGroupOffsetsRequest :: MonadPut m => E.ApiVersion -> DeleteShareGroupOffsetsRequest -> m ()
encodeDeleteShareGroupOffsetsRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (deleteShareGroupOffsetsRequestGroupId msg))
      E.encodeVersionedArray version 0 encodeDeleteShareGroupOffsetsRequestTopic (case P.unKafkaArray (deleteShareGroupOffsetsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteShareGroupOffsetsRequest with the given API version.
decodeDeleteShareGroupOffsetsRequest :: MonadGet m => E.ApiVersion -> m DeleteShareGroupOffsetsRequest
decodeDeleteShareGroupOffsetsRequest version
  | version == 0 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDeleteShareGroupOffsetsRequestTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteShareGroupOffsetsRequest
        {
        deleteShareGroupOffsetsRequestGroupId = fieldgroupid
        ,
        deleteShareGroupOffsetsRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeDeleteShareGroupOffsetsRequest' / 'decodeDeleteShareGroupOffsetsRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec DeleteShareGroupOffsetsRequest where
  wireCodec = Just (WC.serialShimCodec encodeDeleteShareGroupOffsetsRequest decodeDeleteShareGroupOffsetsRequest)
  {-# INLINE wireCodec #-}
