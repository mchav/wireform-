{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListPartitionReassignmentsRequest
Description : Kafka ListPartitionReassignmentsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 46.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListPartitionReassignmentsRequest
  (
    ListPartitionReassignmentsRequest(..),
    ListPartitionReassignmentsTopics(..),
    encodeListPartitionReassignmentsRequest,
    decodeListPartitionReassignmentsRequest,
    maxListPartitionReassignmentsRequestVersion
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


-- | The topics to list partition reassignments for, or null to list everything.
data ListPartitionReassignmentsTopics = ListPartitionReassignmentsTopics
  {

  -- | The topic name.

  -- Versions: 0+
  listPartitionReassignmentsTopicsName :: !(KafkaString)
,

  -- | The partitions to list partition reassignments for.

  -- Versions: 0+
  listPartitionReassignmentsTopicsPartitionIndexes :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode ListPartitionReassignmentsTopics with version-aware field handling.
encodeListPartitionReassignmentsTopics :: MonadPut m => E.ApiVersion -> ListPartitionReassignmentsTopics -> m ()
encodeListPartitionReassignmentsTopics version lmsg =
  do
    if version >= 0 then serialize (toCompactString (listPartitionReassignmentsTopicsName lmsg)) else serialize (listPartitionReassignmentsTopicsName lmsg)
    E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (listPartitionReassignmentsTopicsPartitionIndexes lmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ListPartitionReassignmentsTopics with version-aware field handling.
decodeListPartitionReassignmentsTopics :: MonadGet m => E.ApiVersion -> m ListPartitionReassignmentsTopics
decodeListPartitionReassignmentsTopics version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitionindexes <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ListPartitionReassignmentsTopics
      {
      listPartitionReassignmentsTopicsName = fieldname
      ,
      listPartitionReassignmentsTopicsPartitionIndexes = fieldpartitionindexes
      }



data ListPartitionReassignmentsRequest = ListPartitionReassignmentsRequest
  {

  -- | The time in ms to wait for the request to complete.

  -- Versions: 0+
  listPartitionReassignmentsRequestTimeoutMs :: !(Int32)
,

  -- | The topics to list partition reassignments for, or null to list everything.

  -- Versions: 0+
  listPartitionReassignmentsRequestTopics :: !(KafkaArray (ListPartitionReassignmentsTopics))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListPartitionReassignmentsRequest.
maxListPartitionReassignmentsRequestVersion :: Int16
maxListPartitionReassignmentsRequestVersion = 0

-- | Encode ListPartitionReassignmentsRequest with the given API version.
encodeListPartitionReassignmentsRequest :: MonadPut m => E.ApiVersion -> ListPartitionReassignmentsRequest -> m ()
encodeListPartitionReassignmentsRequest version msg
  | version == 0 =
    do
      serialize (listPartitionReassignmentsRequestTimeoutMs msg)
      E.encodeVersionedNullableArray version 0 encodeListPartitionReassignmentsTopics (listPartitionReassignmentsRequestTopics msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ListPartitionReassignmentsRequest with the given API version.
decodeListPartitionReassignmentsRequest :: MonadGet m => E.ApiVersion -> m ListPartitionReassignmentsRequest
decodeListPartitionReassignmentsRequest version
  | version == 0 =
    do
      fieldtimeoutms <- deserialize
      fieldtopics <- E.decodeVersionedNullableArray version 0 decodeListPartitionReassignmentsTopics
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListPartitionReassignmentsRequest
        {
        listPartitionReassignmentsRequestTimeoutMs = fieldtimeoutms
        ,
        listPartitionReassignmentsRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeListPartitionReassignmentsRequest' / 'decodeListPartitionReassignmentsRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec ListPartitionReassignmentsRequest where
  wireCodec = Just (WC.serialShimCodec encodeListPartitionReassignmentsRequest decodeListPartitionReassignmentsRequest)
  {-# INLINE wireCodec #-}
