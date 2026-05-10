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
    maxUpdateFeaturesResponseVersion
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

-- | KafkaMessage instance for UpdateFeaturesResponse.
instance KafkaMessage UpdateFeaturesResponse where
  messageApiKey = 57
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a UpdatableFeatureResult.
wireMaxSizeUpdatableFeatureResult :: Int -> UpdatableFeatureResult -> Int
wireMaxSizeUpdatableFeatureResult _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (updatableFeatureResultFeature msg))
  + 2
  + WP.compactStringMaxSize (P.toCompactString (updatableFeatureResultErrorMessage msg))
  + 1

-- | Direct-poke encoder for UpdatableFeatureResult.
wirePokeUpdatableFeatureResult :: Int -> Ptr Word8 -> UpdatableFeatureResult -> IO (Ptr Word8)
wirePokeUpdatableFeatureResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (updatableFeatureResultFeature msg)) else WP.pokeKafkaString p0 (updatableFeatureResultFeature msg))
  p2 <- W.pokeInt16BE p1 (updatableFeatureResultErrorCode msg)
  p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (updatableFeatureResultErrorMessage msg)) else WP.pokeKafkaString p2 (updatableFeatureResultErrorMessage msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for UpdatableFeatureResult.
wirePeekUpdatableFeatureResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (UpdatableFeatureResult, Ptr Word8)
wirePeekUpdatableFeatureResult version _fp _basePtr p0 endPtr = do
  (f0_feature, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (UpdatableFeatureResult { updatableFeatureResultFeature = f0_feature, updatableFeatureResultErrorCode = f1_errorcode, updatableFeatureResultErrorMessage = f2_errormessage }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultUpdatableFeatureResult :: UpdatableFeatureResult
defaultUpdatableFeatureResult = UpdatableFeatureResult { updatableFeatureResultFeature = P.KafkaString Null, updatableFeatureResultErrorCode = 0, updatableFeatureResultErrorMessage = P.KafkaString Null }

-- | Worst-case wire size of a UpdateFeaturesResponse.
wireMaxSizeUpdateFeaturesResponse :: Int -> UpdateFeaturesResponse -> Int
wireMaxSizeUpdateFeaturesResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (updateFeaturesResponseErrorMessage msg))
  + (5 + (case P.unKafkaArray (updateFeaturesResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeUpdatableFeatureResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for UpdateFeaturesResponse.
wirePokeUpdateFeaturesResponse :: Int -> Ptr Word8 -> UpdateFeaturesResponse -> IO (Ptr Word8)
wirePokeUpdateFeaturesResponse version basePtr msg
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (updateFeaturesResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (updateFeaturesResponseErrorCode msg)
    p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (updateFeaturesResponseErrorMessage msg)) else WP.pokeKafkaString p2 (updateFeaturesResponseErrorMessage msg))
    WP.pokeEmptyTaggedFields p3
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (updateFeaturesResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (updateFeaturesResponseErrorCode msg)
    p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (updateFeaturesResponseErrorMessage msg)) else WP.pokeKafkaString p2 (updateFeaturesResponseErrorMessage msg))
    p4 <- (if version <= 1 then WP.pokeVersionedArray version 0 (\p x -> wirePokeUpdatableFeatureResult version p x) p3 (updateFeaturesResponseResults msg) else pure p3)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke UpdateFeaturesResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for UpdateFeaturesResponse.
wirePeekUpdateFeaturesResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (UpdateFeaturesResponse, Ptr Word8)
wirePeekUpdateFeaturesResponse version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (UpdateFeaturesResponse { updateFeaturesResponseThrottleTimeMs = f0_throttletimems, updateFeaturesResponseErrorCode = f1_errorcode, updateFeaturesResponseErrorMessage = f2_errormessage, updateFeaturesResponseResults = P.mkKafkaArray V.empty }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_results, p4) <- (if version <= 1 then WP.peekVersionedArray version 0 (\p e -> wirePeekUpdatableFeatureResult version _fp _basePtr p e) p3 endPtr else pure (P.mkKafkaArray V.empty, p3))
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (UpdateFeaturesResponse { updateFeaturesResponseThrottleTimeMs = f0_throttletimems, updateFeaturesResponseErrorCode = f1_errorcode, updateFeaturesResponseErrorMessage = f2_errormessage, updateFeaturesResponseResults = f3_results }, pTagsEnd)
  | otherwise = error $ "wirePeek UpdateFeaturesResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec UpdateFeaturesResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeUpdateFeaturesResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeUpdateFeaturesResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekUpdateFeaturesResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}