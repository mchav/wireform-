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
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
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
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
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

-- | KafkaMessage instance for UpdateFeaturesResponse.
instance KafkaMessage UpdateFeaturesResponse where
  messageApiKey = 57
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

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
  p1 <- WP.pokeCompactString p0 (P.toCompactString (updatableFeatureResultFeature msg))
  p2 <- W.pokeInt16BE p1 (updatableFeatureResultErrorCode msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (updatableFeatureResultErrorMessage msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for UpdatableFeatureResult.
wirePeekUpdatableFeatureResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (UpdatableFeatureResult, Ptr Word8)
wirePeekUpdatableFeatureResult version _fp _basePtr p0 endPtr = do
  (f0_feature, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (UpdatableFeatureResult { updatableFeatureResultFeature = f0_feature, updatableFeatureResultErrorCode = f1_errorcode, updatableFeatureResultErrorMessage = f2_errormessage }, pTagsEnd)

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
    p3 <- WP.pokeCompactString p2 (P.toCompactString (updateFeaturesResponseErrorMessage msg))
    WP.pokeEmptyTaggedFields p3
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (updateFeaturesResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (updateFeaturesResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (updateFeaturesResponseErrorMessage msg))
    p4 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeUpdatableFeatureResult version p x) p3 (updateFeaturesResponseResults msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke UpdateFeaturesResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for UpdateFeaturesResponse.
wirePeekUpdateFeaturesResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (UpdateFeaturesResponse, Ptr Word8)
wirePeekUpdateFeaturesResponse version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (UpdateFeaturesResponse { updateFeaturesResponseThrottleTimeMs = f0_throttletimems, updateFeaturesResponseErrorCode = f1_errorcode, updateFeaturesResponseErrorMessage = f2_errormessage, updateFeaturesResponseResults = P.mkKafkaArray V.empty }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_results, p4) <- WP.peekVersionedArray version 0 (\p e -> wirePeekUpdatableFeatureResult version _fp _basePtr p e) p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (UpdateFeaturesResponse { updateFeaturesResponseThrottleTimeMs = f0_throttletimems, updateFeaturesResponseErrorCode = f1_errorcode, updateFeaturesResponseErrorMessage = f2_errormessage, updateFeaturesResponseResults = f3_results }, pTagsEnd)
  | otherwise = error $ "wirePeek UpdateFeaturesResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec UpdateFeaturesResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeUpdateFeaturesResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeUpdateFeaturesResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekUpdateFeaturesResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}