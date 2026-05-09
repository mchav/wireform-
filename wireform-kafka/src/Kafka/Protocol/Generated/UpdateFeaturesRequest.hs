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
    maxUpdateFeaturesRequestVersion
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , Nullable(..)
  )
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


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

-- | KafkaMessage instance for UpdateFeaturesRequest.
instance KafkaMessage UpdateFeaturesRequest where
  messageApiKey = 57
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a FeatureUpdateKey.
wireMaxSizeFeatureUpdateKey :: Int -> FeatureUpdateKey -> Int
wireMaxSizeFeatureUpdateKey _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (featureUpdateKeyFeature msg))
  + 2
  + 1
  + 1
  + 1

-- | Direct-poke encoder for FeatureUpdateKey.
wirePokeFeatureUpdateKey :: Int -> Ptr Word8 -> FeatureUpdateKey -> IO (Ptr Word8)
wirePokeFeatureUpdateKey version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (featureUpdateKeyFeature msg))
  p2 <- W.pokeInt16BE p1 (featureUpdateKeyMaxVersionLevel msg)
  p3 <- W.pokeWord8 p2 (if (featureUpdateKeyAllowDowngrade msg) then 1 else 0)
  p4 <- W.pokeWord8 p3 (fromIntegral (featureUpdateKeyUpgradeType msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for FeatureUpdateKey.
wirePeekFeatureUpdateKey :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FeatureUpdateKey, Ptr Word8)
wirePeekFeatureUpdateKey version _fp _basePtr p0 endPtr = do
  (f0_feature, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_maxversionlevel, p2) <- W.peekInt16BE p1 endPtr
  (f2_allowdowngrade, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
  (f3_upgradetype, p4) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (FeatureUpdateKey { featureUpdateKeyFeature = f0_feature, featureUpdateKeyMaxVersionLevel = f1_maxversionlevel, featureUpdateKeyAllowDowngrade = f2_allowdowngrade, featureUpdateKeyUpgradeType = f3_upgradetype }, pTagsEnd)

-- | Worst-case wire size of a UpdateFeaturesRequest.
wireMaxSizeUpdateFeaturesRequest :: Int -> UpdateFeaturesRequest -> Int
wireMaxSizeUpdateFeaturesRequest _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (updateFeaturesRequestFeatureUpdates msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeFeatureUpdateKey _version x ) v); P.Null -> 0 }))
  + 1
  + 1

-- | Direct-poke encoder for UpdateFeaturesRequest.
wirePokeUpdateFeaturesRequest :: Int -> Ptr Word8 -> UpdateFeaturesRequest -> IO (Ptr Word8)
wirePokeUpdateFeaturesRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (updateFeaturesRequesttimeoutMs msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeFeatureUpdateKey version p x) p1 (updateFeaturesRequestFeatureUpdates msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (updateFeaturesRequesttimeoutMs msg)
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeFeatureUpdateKey version p x) p1 (updateFeaturesRequestFeatureUpdates msg)
    p3 <- W.pokeWord8 p2 (if (updateFeaturesRequestValidateOnly msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke UpdateFeaturesRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for UpdateFeaturesRequest.
wirePeekUpdateFeaturesRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (UpdateFeaturesRequest, Ptr Word8)
wirePeekUpdateFeaturesRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_timeoutms, p1) <- W.peekInt32BE p0 endPtr
    (f1_featureupdates, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekFeatureUpdateKey version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (UpdateFeaturesRequest { updateFeaturesRequesttimeoutMs = f0_timeoutms, updateFeaturesRequestFeatureUpdates = f1_featureupdates, updateFeaturesRequestValidateOnly = False }, pTagsEnd)
  | version >= 1 && version <= 2 = do
    (f0_timeoutms, p1) <- W.peekInt32BE p0 endPtr
    (f1_featureupdates, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekFeatureUpdateKey version _fp _basePtr p e) p1 endPtr
    (f2_validateonly, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (UpdateFeaturesRequest { updateFeaturesRequesttimeoutMs = f0_timeoutms, updateFeaturesRequestFeatureUpdates = f1_featureupdates, updateFeaturesRequestValidateOnly = f2_validateonly }, pTagsEnd)
  | otherwise = error $ "wirePeek UpdateFeaturesRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec UpdateFeaturesRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeUpdateFeaturesRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeUpdateFeaturesRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekUpdateFeaturesRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}