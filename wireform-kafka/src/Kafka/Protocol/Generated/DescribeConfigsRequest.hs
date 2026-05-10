{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeConfigsRequest
Description : Kafka DescribeConfigsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 32.



Valid versions: 1-4
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeConfigsRequest
  (
    DescribeConfigsRequest(..),
    DescribeConfigsResource(..),
    maxDescribeConfigsRequestVersion
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


-- | The resources whose configurations we want to describe.
data DescribeConfigsResource = DescribeConfigsResource
  {

  -- | The resource type.

  -- Versions: 0+
  describeConfigsResourceResourceType :: !(Int8)
,

  -- | The resource name.

  -- Versions: 0+
  describeConfigsResourceResourceName :: !(KafkaString)
,

  -- | The configuration keys to list, or null to list all configuration keys.

  -- Versions: 0+
  describeConfigsResourceConfigurationKeys :: !(KafkaArray (KafkaString))

  }
  deriving (Eq, Show, Generic)


data DescribeConfigsRequest = DescribeConfigsRequest
  {

  -- | The resources whose configurations we want to describe.

  -- Versions: 0+
  describeConfigsRequestResources :: !(KafkaArray (DescribeConfigsResource))
,

  -- | True if we should include all synonyms.

  -- Versions: 1+
  describeConfigsRequestIncludeSynonyms :: !(Bool)
,

  -- | True if we should include configuration documentation.

  -- Versions: 3+
  describeConfigsRequestIncludeDocumentation :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeConfigsRequest.
maxDescribeConfigsRequestVersion :: Int16
maxDescribeConfigsRequestVersion = 4

-- | KafkaMessage instance for DescribeConfigsRequest.
instance KafkaMessage DescribeConfigsRequest where
  messageApiKey = 32
  messageMinVersion = 1
  messageMaxVersion = 4
  messageFlexibleVersion = Just 4

-- | Worst-case wire size of a DescribeConfigsResource.
wireMaxSizeDescribeConfigsResource :: Int -> DescribeConfigsResource -> Int
wireMaxSizeDescribeConfigsResource _version msg =
  0
  + 1
  + WP.dualStringMaxSize (describeConfigsResourceResourceName msg)
  + (5 + (case P.unKafkaArray (describeConfigsResourceConfigurationKeys msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeConfigsResource.
wirePokeDescribeConfigsResource :: Int -> Ptr Word8 -> DescribeConfigsResource -> IO (Ptr Word8)
wirePokeDescribeConfigsResource version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeWord8 p0 (fromIntegral (describeConfigsResourceResourceType msg))
  p2 <- (if version >= 4 then WP.pokeCompactString p1 (P.toCompactString (describeConfigsResourceResourceName msg)) else WP.pokeKafkaString p1 (describeConfigsResourceResourceName msg))
  p3 <- WP.pokeVersionedNullableArray version 4 (\p s -> if version >= 4 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p2 (describeConfigsResourceConfigurationKeys msg)
  if version >= 4 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for DescribeConfigsResource.
wirePeekDescribeConfigsResource :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeConfigsResource, Ptr Word8)
wirePeekDescribeConfigsResource version _fp _basePtr p0 endPtr = do
  (f0_resourcetype, p1) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p0 endPtr
  (f1_resourcename, p2) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_configurationkeys, p3) <- WP.peekVersionedNullableArray version 4 (\p e -> if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p2 endPtr
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (DescribeConfigsResource { describeConfigsResourceResourceType = f0_resourcetype, describeConfigsResourceResourceName = f1_resourcename, describeConfigsResourceConfigurationKeys = f2_configurationkeys }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribeConfigsResource :: DescribeConfigsResource
defaultDescribeConfigsResource = DescribeConfigsResource { describeConfigsResourceResourceType = 0, describeConfigsResourceResourceName = P.KafkaString Null, describeConfigsResourceConfigurationKeys = P.KafkaArray P.Null }

-- | Worst-case wire size of a DescribeConfigsRequest.
wireMaxSizeDescribeConfigsRequest :: Int -> DescribeConfigsRequest -> Int
wireMaxSizeDescribeConfigsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (describeConfigsRequestResources msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeConfigsResource _version x ) v); P.Null -> 0 }))
  + 1
  + 1
  + 1

-- | Direct-poke encoder for DescribeConfigsRequest.
wirePokeDescribeConfigsRequest :: Int -> Ptr Word8 -> DescribeConfigsRequest -> IO (Ptr Word8)
wirePokeDescribeConfigsRequest version basePtr msg
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeDescribeConfigsResource version p x) p0 (describeConfigsRequestResources msg)
    p2 <- (if version >= 1 then W.pokeWord8 p1 (if (describeConfigsRequestIncludeSynonyms msg) then 1 else 0) else pure p1)
    p3 <- (if version >= 3 then W.pokeWord8 p2 (if (describeConfigsRequestIncludeDocumentation msg) then 1 else 0) else pure p2)
    pure p3
  | version == 4 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeDescribeConfigsResource version p x) p0 (describeConfigsRequestResources msg)
    p2 <- (if version >= 1 then W.pokeWord8 p1 (if (describeConfigsRequestIncludeSynonyms msg) then 1 else 0) else pure p1)
    p3 <- (if version >= 3 then W.pokeWord8 p2 (if (describeConfigsRequestIncludeDocumentation msg) then 1 else 0) else pure p2)
    WP.pokeEmptyTaggedFields p3
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeDescribeConfigsResource version p x) p0 (describeConfigsRequestResources msg)
    p2 <- (if version >= 1 then W.pokeWord8 p1 (if (describeConfigsRequestIncludeSynonyms msg) then 1 else 0) else pure p1)
    pure p2
  | otherwise = error $ "wirePoke DescribeConfigsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeConfigsRequest.
wirePeekDescribeConfigsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeConfigsRequest, Ptr Word8)
wirePeekDescribeConfigsRequest version _fp _basePtr p0 endPtr
  | version == 3 = do
    (f0_resources, p1) <- WP.peekVersionedArray version 4 (\p e -> wirePeekDescribeConfigsResource version _fp _basePtr p e) p0 endPtr
    (f1_includesynonyms, p2) <- (if version >= 1 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr else pure (False, p1))
    (f2_includedocumentation, p3) <- (if version >= 3 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr else pure (False, p2))
    pure (DescribeConfigsRequest { describeConfigsRequestResources = f0_resources, describeConfigsRequestIncludeSynonyms = f1_includesynonyms, describeConfigsRequestIncludeDocumentation = f2_includedocumentation }, p3)
  | version == 4 = do
    (f0_resources, p1) <- WP.peekVersionedArray version 4 (\p e -> wirePeekDescribeConfigsResource version _fp _basePtr p e) p0 endPtr
    (f1_includesynonyms, p2) <- (if version >= 1 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr else pure (False, p1))
    (f2_includedocumentation, p3) <- (if version >= 3 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr else pure (False, p2))
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (DescribeConfigsRequest { describeConfigsRequestResources = f0_resources, describeConfigsRequestIncludeSynonyms = f1_includesynonyms, describeConfigsRequestIncludeDocumentation = f2_includedocumentation }, pTagsEnd)
  | version >= 1 && version <= 2 = do
    (f0_resources, p1) <- WP.peekVersionedArray version 4 (\p e -> wirePeekDescribeConfigsResource version _fp _basePtr p e) p0 endPtr
    (f1_includesynonyms, p2) <- (if version >= 1 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr else pure (False, p1))
    pure (DescribeConfigsRequest { describeConfigsRequestResources = f0_resources, describeConfigsRequestIncludeSynonyms = f1_includesynonyms, describeConfigsRequestIncludeDocumentation = False }, p2)
  | otherwise = error $ "wirePeek DescribeConfigsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeConfigsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeConfigsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeConfigsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeConfigsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}