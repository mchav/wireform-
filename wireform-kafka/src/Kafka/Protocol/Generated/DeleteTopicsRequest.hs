{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteTopicsRequest
Description : Kafka DeleteTopicsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 20.



Valid versions: 1-6
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteTopicsRequest
  (
    DeleteTopicsRequest(..),
    DeleteTopicState(..),
    encodeDeleteTopicsRequest,
    decodeDeleteTopicsRequest,
    maxDeleteTopicsRequestVersion
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


-- | The name or topic ID of the topic.
data DeleteTopicState = DeleteTopicState
  {

  -- | The topic name.

  -- Versions: 6+
  deleteTopicStateName :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 6+
  deleteTopicStateTopicId :: !(KafkaUuid)

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteTopicState with version-aware field handling.
encodeDeleteTopicState :: MonadPut m => E.ApiVersion -> DeleteTopicState -> m ()
encodeDeleteTopicState version dmsg =
  do
    when (version >= 6) $
      if version >= 4 then serialize (toCompactString (deleteTopicStateName dmsg)) else serialize (deleteTopicStateName dmsg)
    when (version >= 6) $
      serialize (deleteTopicStateTopicId dmsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteTopicState with version-aware field handling.
decodeDeleteTopicState :: MonadGet m => E.ApiVersion -> m DeleteTopicState
decodeDeleteTopicState version =
  do
    fieldname <- if version >= 6
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldtopicid <- if version >= 6
      then deserialize
      else pure (P.nullUuid)
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteTopicState
      {
      deleteTopicStateName = fieldname
      ,
      deleteTopicStateTopicId = fieldtopicid
      }



data DeleteTopicsRequest = DeleteTopicsRequest
  {

  -- | The name or topic ID of the topic.

  -- Versions: 6+
  deleteTopicsRequestTopics :: !(KafkaArray (DeleteTopicState))
,

  -- | The names of the topics to delete.

  -- Versions: 0-5
  deleteTopicsRequestTopicNames :: !(KafkaArray (KafkaString))
,

  -- | The length of time in milliseconds to wait for the deletions to complete.

  -- Versions: 0+
  deleteTopicsRequestTimeoutMs :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteTopicsRequest.
maxDeleteTopicsRequestVersion :: Int16
maxDeleteTopicsRequestVersion = 6

-- | Encode DeleteTopicsRequest with the given API version.
encodeDeleteTopicsRequest :: MonadPut m => E.ApiVersion -> DeleteTopicsRequest -> m ()
encodeDeleteTopicsRequest version msg
  | version == 6 =
    do
      E.encodeVersionedArray version 4 encodeDeleteTopicState (case P.unKafkaArray (deleteTopicsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (deleteTopicsRequestTimeoutMs msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 4 && version <= 5 =
    do
      E.encodeVersionedArray version 4 (\v s -> if v >= 4 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (deleteTopicsRequestTopicNames msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (deleteTopicsRequestTimeoutMs msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 3 =
    do
      E.encodeVersionedArray version 4 (\v s -> if v >= 4 then serialize (toCompactString s) else serialize s) (case P.unKafkaArray (deleteTopicsRequestTopicNames msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (deleteTopicsRequestTimeoutMs msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteTopicsRequest with the given API version.
decodeDeleteTopicsRequest :: MonadGet m => E.ApiVersion -> m DeleteTopicsRequest
decodeDeleteTopicsRequest version
  | version == 6 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDeleteTopicState
      fieldtimeoutms <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteTopicsRequest
        {
        deleteTopicsRequestTopics = fieldtopics
        ,
        deleteTopicsRequestTopicNames = P.mkKafkaArray V.empty
        ,
        deleteTopicsRequestTimeoutMs = fieldtimeoutms
        }

  | version >= 4 && version <= 5 =
    do
      fieldtopicnames <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 (\v -> if v >= 4 then P.fromCompactString <$> deserialize else deserialize)
      fieldtimeoutms <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteTopicsRequest
        {
        deleteTopicsRequestTopics = P.mkKafkaArray V.empty
        ,
        deleteTopicsRequestTopicNames = fieldtopicnames
        ,
        deleteTopicsRequestTimeoutMs = fieldtimeoutms
        }

  | version >= 1 && version <= 3 =
    do
      fieldtopicnames <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 (\v -> if v >= 4 then P.fromCompactString <$> deserialize else deserialize)
      fieldtimeoutms <- deserialize
      pure DeleteTopicsRequest
        {
        deleteTopicsRequestTopics = P.mkKafkaArray V.empty
        ,
        deleteTopicsRequestTopicNames = fieldtopicnames
        ,
        deleteTopicsRequestTimeoutMs = fieldtimeoutms
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeDeleteTopicsRequest' / 'decodeDeleteTopicsRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec DeleteTopicsRequest where
  wireCodec = Just (WC.serialShimCodec encodeDeleteTopicsRequest decodeDeleteTopicsRequest)
  {-# INLINE wireCodec #-}
