{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.UpdateFeaturesRequest
Description : Kafka UpdateFeaturesRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 57.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.UpdateFeaturesRequest
  (
    UpdateFeaturesRequest(..),
    FeatureUpdateKey(..),
    encodeUpdateFeaturesRequest,
    decodeUpdateFeaturesRequest,
    maxUpdateFeaturesRequestVersion
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


-- | The list of updates to finalized features.
data FeatureUpdateKey = FeatureUpdateKey
  {

  -- | The name of the finalized feature to be updated.

  -- Versions: 0+
  featureUpdateKeyFeature :: !(KafkaString)
,

  -- | The new maximum version level for the finalized feature. A value >= 1 is valid. A value < 1, is spec

  -- Versions: 0+
  featureUpdateKeyMaxVersionLevel :: !(Int16)
,

  -- | DEPRECATED in version 1 (see DowngradeType). When set to true, the finalized feature version level i

  -- Versions: 0
  featureUpdateKeyAllowDowngrade :: !(Bool)
,

  -- | Determine which type of upgrade will be performed: 1 will perform an upgrade only (default), 2 is sa

  -- Versions: 1+
  featureUpdateKeyUpgradeType :: !(Int8)

  }
  deriving (Eq, Show, Generic)


-- | Encode FeatureUpdateKey with version-aware field handling.
encodeFeatureUpdateKey :: MonadPut m => E.ApiVersion -> FeatureUpdateKey -> m ()
encodeFeatureUpdateKey version fmsg =
  do
    if version >= 0 then serialize (toCompactString (featureUpdateKeyFeature fmsg)) else serialize (featureUpdateKeyFeature fmsg)
    serialize (featureUpdateKeyMaxVersionLevel fmsg)
    when (version == 0) $
      serialize (featureUpdateKeyAllowDowngrade fmsg)
    when (version >= 1) $
      serialize (featureUpdateKeyUpgradeType fmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode FeatureUpdateKey with version-aware field handling.
decodeFeatureUpdateKey :: MonadGet m => E.ApiVersion -> m FeatureUpdateKey
decodeFeatureUpdateKey version =
  do
    fieldfeature <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldmaxversionlevel <- deserialize
    fieldallowdowngrade <- if version == 0
      then deserialize
      else pure (False)
    fieldupgradetype <- if version >= 1
      then deserialize
      else pure (1)
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure FeatureUpdateKey
      {
      featureUpdateKeyFeature = fieldfeature
      ,
      featureUpdateKeyMaxVersionLevel = fieldmaxversionlevel
      ,
      featureUpdateKeyAllowDowngrade = fieldallowdowngrade
      ,
      featureUpdateKeyUpgradeType = fieldupgradetype
      }



data UpdateFeaturesRequest = UpdateFeaturesRequest
  {

  -- | How long to wait in milliseconds before timing out the request.

  -- Versions: 0+
  updateFeaturesRequesttimeoutMs :: !(Int32)
,

  -- | The list of updates to finalized features.

  -- Versions: 0+
  updateFeaturesRequestFeatureUpdates :: !(KafkaArray (FeatureUpdateKey))
,

  -- | True if we should validate the request, but not perform the upgrade or downgrade.

  -- Versions: 1+
  updateFeaturesRequestValidateOnly :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for UpdateFeaturesRequest.
maxUpdateFeaturesRequestVersion :: Int16
maxUpdateFeaturesRequestVersion = 2

-- | Encode UpdateFeaturesRequest with the given API version.
encodeUpdateFeaturesRequest :: MonadPut m => E.ApiVersion -> UpdateFeaturesRequest -> m ()
encodeUpdateFeaturesRequest version msg
  | version == 0 =
    do
      serialize (updateFeaturesRequesttimeoutMs msg)
      E.encodeVersionedArray version 0 encodeFeatureUpdateKey (case P.unKafkaArray (updateFeaturesRequestFeatureUpdates msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 2 =
    do
      serialize (updateFeaturesRequesttimeoutMs msg)
      E.encodeVersionedArray version 0 encodeFeatureUpdateKey (case P.unKafkaArray (updateFeaturesRequestFeatureUpdates msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (updateFeaturesRequestValidateOnly msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode UpdateFeaturesRequest with the given API version.
decodeUpdateFeaturesRequest :: MonadGet m => E.ApiVersion -> m UpdateFeaturesRequest
decodeUpdateFeaturesRequest version
  | version == 0 =
    do
      fieldtimeoutms <- deserialize
      fieldfeatureupdates <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeFeatureUpdateKey
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure UpdateFeaturesRequest
        {
        updateFeaturesRequesttimeoutMs = fieldtimeoutms
        ,
        updateFeaturesRequestFeatureUpdates = fieldfeatureupdates
        ,
        updateFeaturesRequestValidateOnly = False
        }

  | version >= 1 && version <= 2 =
    do
      fieldtimeoutms <- deserialize
      fieldfeatureupdates <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeFeatureUpdateKey
      fieldvalidateonly <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure UpdateFeaturesRequest
        {
        updateFeaturesRequesttimeoutMs = fieldtimeoutms
        ,
        updateFeaturesRequestFeatureUpdates = fieldfeatureupdates
        ,
        updateFeaturesRequestValidateOnly = fieldvalidateonly
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeUpdateFeaturesRequest' / 'decodeUpdateFeaturesRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec UpdateFeaturesRequest where
  wireCodec = Just (WC.serialShimCodec encodeUpdateFeaturesRequest decodeUpdateFeaturesRequest)
  {-# INLINE wireCodec #-}
