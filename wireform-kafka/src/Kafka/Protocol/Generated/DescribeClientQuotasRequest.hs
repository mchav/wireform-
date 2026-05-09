{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeClientQuotasRequest
Description : Kafka DescribeClientQuotasRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 48.



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeClientQuotasRequest
  (
    DescribeClientQuotasRequest(..),
    ComponentData(..),
    maxDescribeClientQuotasRequestVersion
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


-- | Filter components to apply to quota entities.
data ComponentData = ComponentData
  {

  -- | The entity type that the filter component applies to.

  -- Versions: 0+
  componentDataEntityType :: !(KafkaString)
,

  -- | How to match the entity {0 = exact name, 1 = default name, 2 = any specified name}.

  -- Versions: 0+
  componentDataMatchType :: !(Int8)
,

  -- | The string to match against, or null if unused for the match type.

  -- Versions: 0+
  componentDataMatch :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


data DescribeClientQuotasRequest = DescribeClientQuotasRequest
  {

  -- | Filter components to apply to quota entities.

  -- Versions: 0+
  describeClientQuotasRequestComponents :: !(KafkaArray (ComponentData))
,

  -- | Whether the match is strict, i.e. should exclude entities with unspecified entity types.

  -- Versions: 0+
  describeClientQuotasRequestStrict :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeClientQuotasRequest.
maxDescribeClientQuotasRequestVersion :: Int16
maxDescribeClientQuotasRequestVersion = 1

-- | KafkaMessage instance for DescribeClientQuotasRequest.
instance KafkaMessage DescribeClientQuotasRequest where
  messageApiKey = 48
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 1

-- | Worst-case wire size of a ComponentData.
wireMaxSizeComponentData :: Int -> ComponentData -> Int
wireMaxSizeComponentData _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (componentDataEntityType msg))
  + 1
  + WP.compactStringMaxSize (P.toCompactString (componentDataMatch msg))
  + 1

-- | Direct-poke encoder for ComponentData.
wirePokeComponentData :: Int -> Ptr Word8 -> ComponentData -> IO (Ptr Word8)
wirePokeComponentData version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (componentDataEntityType msg))
  p2 <- W.pokeWord8 p1 (fromIntegral (componentDataMatchType msg))
  p3 <- WP.pokeCompactString p2 (P.toCompactString (componentDataMatch msg))
  if version >= 1 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for ComponentData.
wirePeekComponentData :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ComponentData, Ptr Word8)
wirePeekComponentData version _fp _basePtr p0 endPtr = do
  (f0_entitytype, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_matchtype, p2) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p1 endPtr
  (f2_match, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  pTagsEnd <- if version >= 1 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (ComponentData { componentDataEntityType = f0_entitytype, componentDataMatchType = f1_matchtype, componentDataMatch = f2_match }, pTagsEnd)

-- | Worst-case wire size of a DescribeClientQuotasRequest.
wireMaxSizeDescribeClientQuotasRequest :: Int -> DescribeClientQuotasRequest -> Int
wireMaxSizeDescribeClientQuotasRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (describeClientQuotasRequestComponents msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeComponentData _version x ) v); P.Null -> 0 }))
  + 1
  + 1

-- | Direct-poke encoder for DescribeClientQuotasRequest.
wirePokeDescribeClientQuotasRequest :: Int -> Ptr Word8 -> DescribeClientQuotasRequest -> IO (Ptr Word8)
wirePokeDescribeClientQuotasRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeComponentData version p x) p0 (describeClientQuotasRequestComponents msg)
    p2 <- W.pokeWord8 p1 (if (describeClientQuotasRequestStrict msg) then 1 else 0)
    pure p2
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 1 (\p x -> wirePokeComponentData version p x) p0 (describeClientQuotasRequestComponents msg)
    p2 <- W.pokeWord8 p1 (if (describeClientQuotasRequestStrict msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke DescribeClientQuotasRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeClientQuotasRequest.
wirePeekDescribeClientQuotasRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeClientQuotasRequest, Ptr Word8)
wirePeekDescribeClientQuotasRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_components, p1) <- WP.peekVersionedArray version 1 (\p e -> wirePeekComponentData version _fp _basePtr p e) p0 endPtr
    (f1_strict, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    pure (DescribeClientQuotasRequest { describeClientQuotasRequestComponents = f0_components, describeClientQuotasRequestStrict = f1_strict }, p2)
  | version == 1 = do
    (f0_components, p1) <- WP.peekVersionedArray version 1 (\p e -> wirePeekComponentData version _fp _basePtr p e) p0 endPtr
    (f1_strict, p2) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DescribeClientQuotasRequest { describeClientQuotasRequestComponents = f0_components, describeClientQuotasRequestStrict = f1_strict }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeClientQuotasRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeClientQuotasRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeClientQuotasRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeClientQuotasRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeClientQuotasRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}