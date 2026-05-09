{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.IncrementalAlterConfigsRequest
Description : Kafka IncrementalAlterConfigsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 44.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.IncrementalAlterConfigsRequest
  (
    IncrementalAlterConfigsRequest(..),
    AlterConfigsResource(..),
    AlterableConfig(..),
    maxIncrementalAlterConfigsRequestVersion
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

  -- | The type (Set, Delete, Append, Subtract) of operation.

  -- Versions: 0+
  alterableConfigConfigOperation :: !(Int8)
,

  -- | The value to set for the configuration key.

  -- Versions: 0+
  alterableConfigValue :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | The incremental updates for each resource.
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


data IncrementalAlterConfigsRequest = IncrementalAlterConfigsRequest
  {

  -- | The incremental updates for each resource.

  -- Versions: 0+
  incrementalAlterConfigsRequestResources :: !(KafkaArray (AlterConfigsResource))
,

  -- | True if we should validate the request, but not change the configurations.

  -- Versions: 0+
  incrementalAlterConfigsRequestValidateOnly :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for IncrementalAlterConfigsRequest.
maxIncrementalAlterConfigsRequestVersion :: Int16
maxIncrementalAlterConfigsRequestVersion = 1

-- | KafkaMessage instance for IncrementalAlterConfigsRequest.
instance KafkaMessage IncrementalAlterConfigsRequest where
  messageApiKey = 44
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 1

-- | Worst-case wire size of a AlterableConfig.
wireMaxSizeAlterableConfig :: Int -> AlterableConfig -> Int
wireMaxSizeAlterableConfig _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (alterableConfigName msg))
  + 1
  + WP.compactStringMaxSize (P.toCompactString (alterableConfigValue msg))
  + 1

-- | Direct-poke encoder for AlterableConfig.
wirePokeAlterableConfig :: Int -> Ptr Word8 -> AlterableConfig -> IO (Ptr Word8)
wirePokeAlterableConfig version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (alterableConfigName msg))
  p2 <- W.pokeWord8 p1 (fromIntegral (alterableConfigConfigOperation msg))
  p3 <- WP.pokeCompactString p2 (P.toCompactString (alterableConfigValue msg))
  if version >= 1 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for AlterableConfig.
wirePeekAlterableConfig :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterableConfig, Ptr Word8)
wirePeekAlterableConfig version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_configoperation, p2) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p1 endPtr
  (f2_value, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (AlterableConfig { alterableConfigName = f0_name, alterableConfigConfigOperation = f1_configoperation, alterableConfigValue = f2_value }, pTagsEnd)

-- | Worst-case wire size of a AlterConfigsResource.
wireMaxSizeAlterConfigsResource :: Int -> AlterConfigsResource -> Int
wireMaxSizeAlterConfigsResource _version msg =
  0
  + 1
  + WP.compactStringMaxSize (P.toCompactString (alterConfigsResourceResourceName msg))
  + (5 + (case P.unKafkaArray (alterConfigsResourceConfigs msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterableConfig _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterConfigsResource.
wirePokeAlterConfigsResource :: Int -> Ptr Word8 -> AlterConfigsResource -> IO (Ptr Word8)
wirePokeAlterConfigsResource version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeWord8 p0 (fromIntegral (alterConfigsResourceResourceType msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (alterConfigsResourceResourceName msg))
  p3 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeAlterableConfig version p x) p2 (alterConfigsResourceConfigs msg)
  if version >= 1 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for AlterConfigsResource.
wirePeekAlterConfigsResource :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterConfigsResource, Ptr Word8)
wirePeekAlterConfigsResource version _fp _basePtr p0 endPtr = do
  (f0_resourcetype, p1) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p0 endPtr
  (f1_resourcename, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_configs, p3) <- WP.peekVersionedArray version 1 (\p e -> wirePeekAlterableConfig version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (AlterConfigsResource { alterConfigsResourceResourceType = f0_resourcetype, alterConfigsResourceResourceName = f1_resourcename, alterConfigsResourceConfigs = f2_configs }, pTagsEnd)

-- | Worst-case wire size of a IncrementalAlterConfigsRequest.
wireMaxSizeIncrementalAlterConfigsRequest :: Int -> IncrementalAlterConfigsRequest -> Int
wireMaxSizeIncrementalAlterConfigsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (incrementalAlterConfigsRequestResources msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterConfigsResource _version x ) v); P.Null -> 0 }))
  + 1
  + 1

-- | Direct-poke encoder for IncrementalAlterConfigsRequest.
wirePokeIncrementalAlterConfigsRequest :: Int -> Ptr Word8 -> IncrementalAlterConfigsRequest -> IO (Ptr Word8)
wirePokeIncrementalAlterConfigsRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeAlterConfigsResource version p x) p0 (incrementalAlterConfigsRequestResources msg)
    p2 <- W.pokeWord8 p1 (if (incrementalAlterConfigsRequestValidateOnly msg) then 1 else 0)
    pure p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeAlterConfigsResource version p x) p0 (incrementalAlterConfigsRequestResources msg)
    p2 <- W.pokeWord8 p1 (if (incrementalAlterConfigsRequestValidateOnly msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke IncrementalAlterConfigsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for IncrementalAlterConfigsRequest.
wirePeekIncrementalAlterConfigsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (IncrementalAlterConfigsRequest, Ptr Word8)
wirePeekIncrementalAlterConfigsRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_resources, p1) <- WP.peekVersionedArray version 1 (\p e -> wirePeekAlterConfigsResource version _fp _basePtr p e) p0 endPtr
    (f1_validateonly, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    pure (IncrementalAlterConfigsRequest { incrementalAlterConfigsRequestResources = f0_resources, incrementalAlterConfigsRequestValidateOnly = f1_validateonly }, p2)
  | version == 1 = do
    (f0_resources, p1) <- WP.peekVersionedArray version 1 (\p e -> wirePeekAlterConfigsResource version _fp _basePtr p e) p0 endPtr
    (f1_validateonly, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (IncrementalAlterConfigsRequest { incrementalAlterConfigsRequestResources = f0_resources, incrementalAlterConfigsRequestValidateOnly = f1_validateonly }, pTagsEnd)
  | otherwise = error $ "wirePeek IncrementalAlterConfigsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec IncrementalAlterConfigsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeIncrementalAlterConfigsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeIncrementalAlterConfigsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekIncrementalAlterConfigsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}