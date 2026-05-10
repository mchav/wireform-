{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterConfigsRequest
Description : Kafka AlterConfigsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 33.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterConfigsRequest
  (
    AlterConfigsRequest(..),
    AlterConfigsResource(..),
    AlterableConfig(..),
    maxAlterConfigsRequestVersion
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


-- | The configurations.
data AlterableConfig = AlterableConfig
  {

  -- | The configuration key name.

  -- Versions: 0+
  alterableConfigName :: !(KafkaString)
,

  -- | The value to set for the configuration key.

  -- Versions: 0+
  alterableConfigValue :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | The updates for each resource.
data AlterConfigsResource = AlterConfigsResource
  {

  -- | The resource type.

  -- Versions: 0+
  alterConfigsResourceResourceType :: !(Int8)
,

  -- | The resource name.

  -- Versions: 0+
  alterConfigsResourceResourceName :: !(KafkaString)
,

  -- | The configurations.

  -- Versions: 0+
  alterConfigsResourceConfigs :: !(KafkaArray (AlterableConfig))

  }
  deriving (Eq, Show, Generic)


data AlterConfigsRequest = AlterConfigsRequest
  {

  -- | The updates for each resource.

  -- Versions: 0+
  alterConfigsRequestResources :: !(KafkaArray (AlterConfigsResource))
,

  -- | True if we should validate the request, but not change the configurations.

  -- Versions: 0+
  alterConfigsRequestValidateOnly :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterConfigsRequest.
maxAlterConfigsRequestVersion :: Int16
maxAlterConfigsRequestVersion = 2

-- | KafkaMessage instance for AlterConfigsRequest.
instance KafkaMessage AlterConfigsRequest where
  messageApiKey = 33
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2

-- | Worst-case wire size of a AlterableConfig.
wireMaxSizeAlterableConfig :: Int -> AlterableConfig -> Int
wireMaxSizeAlterableConfig _version msg =
  0
  + WP.dualStringMaxSize (alterableConfigName msg)
  + WP.dualStringMaxSize (alterableConfigValue msg)
  + 1

-- | Direct-poke encoder for AlterableConfig.
wirePokeAlterableConfig :: Int -> Ptr Word8 -> AlterableConfig -> IO (Ptr Word8)
wirePokeAlterableConfig version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 2 then WP.pokeCompactString p0 (P.toCompactString (alterableConfigName msg)) else WP.pokeKafkaString p0 (alterableConfigName msg))
  p2 <- (if version >= 2 then WP.pokeCompactString p1 (P.toCompactString (alterableConfigValue msg)) else WP.pokeKafkaString p1 (alterableConfigValue msg))
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AlterableConfig.
wirePeekAlterableConfig :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterableConfig, Ptr Word8)
wirePeekAlterableConfig version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_value, p2) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AlterableConfig { alterableConfigName = f0_name, alterableConfigValue = f1_value }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAlterableConfig :: AlterableConfig
defaultAlterableConfig = AlterableConfig { alterableConfigName = P.KafkaString Null, alterableConfigValue = P.KafkaString Null }

-- | Worst-case wire size of a AlterConfigsResource.
wireMaxSizeAlterConfigsResource :: Int -> AlterConfigsResource -> Int
wireMaxSizeAlterConfigsResource _version msg =
  0
  + 1
  + WP.dualStringMaxSize (alterConfigsResourceResourceName msg)
  + (5 + (case P.unKafkaArray (alterConfigsResourceConfigs msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterableConfig _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterConfigsResource.
wirePokeAlterConfigsResource :: Int -> Ptr Word8 -> AlterConfigsResource -> IO (Ptr Word8)
wirePokeAlterConfigsResource version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeWord8 p0 (fromIntegral (alterConfigsResourceResourceType msg))
  p2 <- (if version >= 2 then WP.pokeCompactString p1 (P.toCompactString (alterConfigsResourceResourceName msg)) else WP.pokeKafkaString p1 (alterConfigsResourceResourceName msg))
  p3 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAlterableConfig version p x) p2 (alterConfigsResourceConfigs msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for AlterConfigsResource.
wirePeekAlterConfigsResource :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterConfigsResource, Ptr Word8)
wirePeekAlterConfigsResource version _fp _basePtr p0 endPtr = do
  (f0_resourcetype, p1) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p0 endPtr
  (f1_resourcename, p2) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_configs, p3) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAlterableConfig version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (AlterConfigsResource { alterConfigsResourceResourceType = f0_resourcetype, alterConfigsResourceResourceName = f1_resourcename, alterConfigsResourceConfigs = f2_configs }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultAlterConfigsResource :: AlterConfigsResource
defaultAlterConfigsResource = AlterConfigsResource { alterConfigsResourceResourceType = 0, alterConfigsResourceResourceName = P.KafkaString Null, alterConfigsResourceConfigs = P.mkKafkaArray V.empty }

-- | Worst-case wire size of a AlterConfigsRequest.
wireMaxSizeAlterConfigsRequest :: Int -> AlterConfigsRequest -> Int
wireMaxSizeAlterConfigsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (alterConfigsRequestResources msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterConfigsResource _version x ) v); P.Null -> 0 }))
  + 1
  + 1

-- | Direct-poke encoder for AlterConfigsRequest.
wirePokeAlterConfigsRequest :: Int -> Ptr Word8 -> AlterConfigsRequest -> IO (Ptr Word8)
wirePokeAlterConfigsRequest version basePtr msg
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAlterConfigsResource version p x) p0 (alterConfigsRequestResources msg)
    p2 <- W.pokeWord8 p1 (if (alterConfigsRequestValidateOnly msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p2
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAlterConfigsResource version p x) p0 (alterConfigsRequestResources msg)
    p2 <- W.pokeWord8 p1 (if (alterConfigsRequestValidateOnly msg) then 1 else 0)
    pure p2
  | otherwise = error $ "wirePoke AlterConfigsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterConfigsRequest.
wirePeekAlterConfigsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterConfigsRequest, Ptr Word8)
wirePeekAlterConfigsRequest version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_resources, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAlterConfigsResource version _fp _basePtr p e) p0 endPtr
    (f1_validateonly, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (AlterConfigsRequest { alterConfigsRequestResources = f0_resources, alterConfigsRequestValidateOnly = f1_validateonly }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_resources, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAlterConfigsResource version _fp _basePtr p e) p0 endPtr
    (f1_validateonly, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    pure (AlterConfigsRequest { alterConfigsRequestResources = f0_resources, alterConfigsRequestValidateOnly = f1_validateonly }, p2)
  | otherwise = error $ "wirePeek AlterConfigsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AlterConfigsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterConfigsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterConfigsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterConfigsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}