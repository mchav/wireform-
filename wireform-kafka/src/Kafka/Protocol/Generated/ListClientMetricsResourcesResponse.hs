{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListClientMetricsResourcesResponse
Description : Kafka ListClientMetricsResourcesResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 74.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListClientMetricsResourcesResponse
  (
    ListClientMetricsResourcesResponse(..),
    ClientMetricsResource(..),
    maxListClientMetricsResourcesResponseVersion
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


-- | Each client metrics resource in the response.
data ClientMetricsResource = ClientMetricsResource
  {

  -- | The resource name.

  -- Versions: 0+
  clientMetricsResourceName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


data ListClientMetricsResourcesResponse = ListClientMetricsResourcesResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  listClientMetricsResourcesResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  listClientMetricsResourcesResponseErrorCode :: !(Int16)
,

  -- | Each client metrics resource in the response.

  -- Versions: 0+
  listClientMetricsResourcesResponseClientMetricsResources :: !(KafkaArray (ClientMetricsResource))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListClientMetricsResourcesResponse.
maxListClientMetricsResourcesResponseVersion :: Int16
maxListClientMetricsResourcesResponseVersion = 0

-- | KafkaMessage instance for ListClientMetricsResourcesResponse.
instance KafkaMessage ListClientMetricsResourcesResponse where
  messageApiKey = 74
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a ClientMetricsResource.
wireMaxSizeClientMetricsResource :: Int -> ClientMetricsResource -> Int
wireMaxSizeClientMetricsResource _version msg =
  0
  + WP.dualStringMaxSize (clientMetricsResourceName msg)
  + 1

-- | Direct-poke encoder for ClientMetricsResource.
wirePokeClientMetricsResource :: Int -> Ptr Word8 -> ClientMetricsResource -> IO (Ptr Word8)
wirePokeClientMetricsResource version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 0 then WP.pokeCompactString p0 (P.toCompactString (clientMetricsResourceName msg)) else WP.pokeKafkaString p0 (clientMetricsResourceName msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p1 else pure p1

-- | Direct-poke decoder for ClientMetricsResource.
wirePeekClientMetricsResource :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ClientMetricsResource, Ptr Word8)
wirePeekClientMetricsResource version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p1 endPtr else pure p1
  pure (ClientMetricsResource { clientMetricsResourceName = f0_name }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultClientMetricsResource :: ClientMetricsResource
defaultClientMetricsResource = ClientMetricsResource { clientMetricsResourceName = P.KafkaString Null }

-- | Worst-case wire size of a ListClientMetricsResourcesResponse.
wireMaxSizeListClientMetricsResourcesResponse :: Int -> ListClientMetricsResourcesResponse -> Int
wireMaxSizeListClientMetricsResourcesResponse _version msg =
  0
  + 4
  + 2
  + (5 + (case P.unKafkaArray (listClientMetricsResourcesResponseClientMetricsResources msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeClientMetricsResource _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ListClientMetricsResourcesResponse.
wirePokeListClientMetricsResourcesResponse :: Int -> Ptr Word8 -> ListClientMetricsResourcesResponse -> IO (Ptr Word8)
wirePokeListClientMetricsResourcesResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (listClientMetricsResourcesResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (listClientMetricsResourcesResponseErrorCode msg)
    p3 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeClientMetricsResource version p x) p2 (listClientMetricsResourcesResponseClientMetricsResources msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke ListClientMetricsResourcesResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ListClientMetricsResourcesResponse.
wirePeekListClientMetricsResourcesResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListClientMetricsResourcesResponse, Ptr Word8)
wirePeekListClientMetricsResourcesResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_clientmetricsresources, p3) <- WP.peekVersionedArray version 0 (\p e -> wirePeekClientMetricsResource version _fp _basePtr p e) p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (ListClientMetricsResourcesResponse { listClientMetricsResourcesResponseThrottleTimeMs = f0_throttletimems, listClientMetricsResourcesResponseErrorCode = f1_errorcode, listClientMetricsResourcesResponseClientMetricsResources = f2_clientmetricsresources }, pTagsEnd)
  | otherwise = error $ "wirePeek ListClientMetricsResourcesResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec ListClientMetricsResourcesResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeListClientMetricsResourcesResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeListClientMetricsResourcesResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekListClientMetricsResourcesResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}