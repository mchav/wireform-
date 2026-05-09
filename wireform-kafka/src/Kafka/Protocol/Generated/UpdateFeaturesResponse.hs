{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.UpdateFeaturesResponse
Description : Kafka UpdateFeaturesResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 57.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.UpdateFeaturesResponse
  (
    UpdateFeaturesResponse(..),
    UpdatableFeatureResult(..),
    encodeUpdateFeaturesResponse,
    decodeUpdateFeaturesResponse,
    maxUpdateFeaturesResponseVersion
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


-- | Results for each feature update.
data UpdatableFeatureResult = UpdatableFeatureResult
  {

  -- | The name of the finalized feature.

  -- Versions: 0+
  updatableFeatureResultFeature :: !(KafkaString)
,

  -- | The feature update error code or `0` if the feature update succeeded.

  -- Versions: 0+
  updatableFeatureResultErrorCode :: !(Int16)
,

  -- | The feature update error, or `null` if the feature update succeeded.

  -- Versions: 0+
  updatableFeatureResultErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode UpdatableFeatureResult with version-aware field handling.
encodeUpdatableFeatureResult :: MonadPut m => E.ApiVersion -> UpdatableFeatureResult -> m ()
encodeUpdatableFeatureResult version umsg =
  do
    if version >= 0 then serialize (toCompactString (updatableFeatureResultFeature umsg)) else serialize (updatableFeatureResultFeature umsg)
    serialize (updatableFeatureResultErrorCode umsg)
    if version >= 0 then serialize (toCompactString (updatableFeatureResultErrorMessage umsg)) else serialize (updatableFeatureResultErrorMessage umsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode UpdatableFeatureResult with version-aware field handling.
decodeUpdatableFeatureResult :: MonadGet m => E.ApiVersion -> m UpdatableFeatureResult
decodeUpdatableFeatureResult version =
  do
    fieldfeature <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure UpdatableFeatureResult
      {
      updatableFeatureResultFeature = fieldfeature
      ,
      updatableFeatureResultErrorCode = fielderrorcode
      ,
      updatableFeatureResultErrorMessage = fielderrormessage
      }



data UpdateFeaturesResponse = UpdateFeaturesResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  updateFeaturesResponseThrottleTimeMs :: !(Int32)
,

  -- | The top-level error code, or `0` if there was no top-level error.

  -- Versions: 0+
  updateFeaturesResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or `null` if there was no top-level error.

  -- Versions: 0+
  updateFeaturesResponseErrorMessage :: !(KafkaString)
,

  -- | Results for each feature update.

  -- Versions: 0-1
  updateFeaturesResponseResults :: !(KafkaArray (UpdatableFeatureResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for UpdateFeaturesResponse.
maxUpdateFeaturesResponseVersion :: Int16
maxUpdateFeaturesResponseVersion = 2

-- | Encode UpdateFeaturesResponse with the given API version.
encodeUpdateFeaturesResponse :: MonadPut m => E.ApiVersion -> UpdateFeaturesResponse -> m ()
encodeUpdateFeaturesResponse version msg
  | version == 2 =
    do
      serialize (updateFeaturesResponseThrottleTimeMs msg)
      serialize (updateFeaturesResponseErrorCode msg)
      serialize (toCompactString (updateFeaturesResponseErrorMessage msg))
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      serialize (updateFeaturesResponseThrottleTimeMs msg)
      serialize (updateFeaturesResponseErrorCode msg)
      serialize (toCompactString (updateFeaturesResponseErrorMessage msg))
      E.encodeVersionedArray version 0 encodeUpdatableFeatureResult (case P.unKafkaArray (updateFeaturesResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode UpdateFeaturesResponse with the given API version.
decodeUpdateFeaturesResponse :: MonadGet m => E.ApiVersion -> m UpdateFeaturesResponse
decodeUpdateFeaturesResponse version
  | version == 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure UpdateFeaturesResponse
        {
        updateFeaturesResponseThrottleTimeMs = fieldthrottletimems
        ,
        updateFeaturesResponseErrorCode = fielderrorcode
        ,
        updateFeaturesResponseErrorMessage = fielderrormessage
        ,
        updateFeaturesResponseResults = P.mkKafkaArray V.empty
        }

  | version >= 0 && version <= 1 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeUpdatableFeatureResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure UpdateFeaturesResponse
        {
        updateFeaturesResponseThrottleTimeMs = fieldthrottletimems
        ,
        updateFeaturesResponseErrorCode = fielderrorcode
        ,
        updateFeaturesResponseErrorMessage = fielderrormessage
        ,
        updateFeaturesResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeUpdateFeaturesResponse' / 'decodeUpdateFeaturesResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec UpdateFeaturesResponse where
  wireCodec = Just (WC.serialShimCodec encodeUpdateFeaturesResponse decodeUpdateFeaturesResponse)
  {-# INLINE wireCodec #-}
