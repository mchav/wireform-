{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.CreatePartitionsResponse
Description : Kafka CreatePartitionsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 37.



Valid versions: 0-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.CreatePartitionsResponse
  (
    CreatePartitionsResponse(..),
    CreatePartitionsTopicResult(..),
    encodeCreatePartitionsResponse,
    decodeCreatePartitionsResponse,
    maxCreatePartitionsResponseVersion
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


-- | The partition creation results for each topic.
data CreatePartitionsTopicResult = CreatePartitionsTopicResult
  {

  -- | The topic name.

  -- Versions: 0+
  createPartitionsTopicResultName :: !(KafkaString)
,

  -- | The result error, or zero if there was no error.

  -- Versions: 0+
  createPartitionsTopicResultErrorCode :: !(Int16)
,

  -- | The result message, or null if there was no error.

  -- Versions: 0+
  createPartitionsTopicResultErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode CreatePartitionsTopicResult with version-aware field handling.
encodeCreatePartitionsTopicResult :: MonadPut m => E.ApiVersion -> CreatePartitionsTopicResult -> m ()
encodeCreatePartitionsTopicResult version cmsg =
  do
    if version >= 2 then serialize (toCompactString (createPartitionsTopicResultName cmsg)) else serialize (createPartitionsTopicResultName cmsg)
    serialize (createPartitionsTopicResultErrorCode cmsg)
    if version >= 2 then serialize (toCompactString (createPartitionsTopicResultErrorMessage cmsg)) else serialize (createPartitionsTopicResultErrorMessage cmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CreatePartitionsTopicResult with version-aware field handling.
decodeCreatePartitionsTopicResult :: MonadGet m => E.ApiVersion -> m CreatePartitionsTopicResult
decodeCreatePartitionsTopicResult version =
  do
    fieldname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CreatePartitionsTopicResult
      {
      createPartitionsTopicResultName = fieldname
      ,
      createPartitionsTopicResultErrorCode = fielderrorcode
      ,
      createPartitionsTopicResultErrorMessage = fielderrormessage
      }



data CreatePartitionsResponse = CreatePartitionsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  createPartitionsResponseThrottleTimeMs :: !(Int32)
,

  -- | The partition creation results for each topic.

  -- Versions: 0+
  createPartitionsResponseResults :: !(KafkaArray (CreatePartitionsTopicResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for CreatePartitionsResponse.
maxCreatePartitionsResponseVersion :: Int16
maxCreatePartitionsResponseVersion = 3

-- | Encode CreatePartitionsResponse with the given API version.
encodeCreatePartitionsResponse :: MonadPut m => E.ApiVersion -> CreatePartitionsResponse -> m ()
encodeCreatePartitionsResponse version msg
  | version >= 0 && version <= 1 =
    do
      serialize (createPartitionsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeCreatePartitionsTopicResult (case P.unKafkaArray (createPartitionsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 2 && version <= 3 =
    do
      serialize (createPartitionsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 2 encodeCreatePartitionsTopicResult (case P.unKafkaArray (createPartitionsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode CreatePartitionsResponse with the given API version.
decodeCreatePartitionsResponse :: MonadGet m => E.ApiVersion -> m CreatePartitionsResponse
decodeCreatePartitionsResponse version
  | version >= 0 && version <= 1 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeCreatePartitionsTopicResult
      pure CreatePartitionsResponse
        {
        createPartitionsResponseThrottleTimeMs = fieldthrottletimems
        ,
        createPartitionsResponseResults = fieldresults
        }

  | version >= 2 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeCreatePartitionsTopicResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure CreatePartitionsResponse
        {
        createPartitionsResponseThrottleTimeMs = fieldthrottletimems
        ,
        createPartitionsResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeCreatePartitionsResponse' / 'decodeCreatePartitionsResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec CreatePartitionsResponse where
  wireCodec = Just (WC.serialShimCodec encodeCreatePartitionsResponse decodeCreatePartitionsResponse)
  {-# INLINE wireCodec #-}
