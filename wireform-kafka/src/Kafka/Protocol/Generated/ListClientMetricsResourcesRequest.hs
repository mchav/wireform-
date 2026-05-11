{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListClientMetricsResourcesRequest
Description : Kafka ListClientMetricsResourcesRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 74.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListClientMetricsResourcesRequest
  (
    ListClientMetricsResourcesRequest(..),
    maxListClientMetricsResourcesRequestVersion
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




data ListClientMetricsResourcesRequest = ListClientMetricsResourcesRequest
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListClientMetricsResourcesRequest.
maxListClientMetricsResourcesRequestVersion :: Int16
maxListClientMetricsResourcesRequestVersion = 0

-- | KafkaMessage instance for ListClientMetricsResourcesRequest.
instance KafkaMessage ListClientMetricsResourcesRequest where
  messageApiKey = 74
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a ListClientMetricsResourcesRequest.
wireMaxSizeListClientMetricsResourcesRequest :: Int -> ListClientMetricsResourcesRequest -> Int
wireMaxSizeListClientMetricsResourcesRequest _version msg =
  0

  + 1

-- | Direct-poke encoder for ListClientMetricsResourcesRequest.
wirePokeListClientMetricsResourcesRequest :: Int -> Ptr Word8 -> ListClientMetricsResourcesRequest -> IO (Ptr Word8)
wirePokeListClientMetricsResourcesRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    WP.pokeEmptyTaggedFields p0
  | otherwise = error $ "wirePoke ListClientMetricsResourcesRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ListClientMetricsResourcesRequest.
wirePeekListClientMetricsResourcesRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListClientMetricsResourcesRequest, Ptr Word8)
wirePeekListClientMetricsResourcesRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    pTagsEnd <- WP.peekAndSkipTaggedFields p0 endPtr
    pure (ListClientMetricsResourcesRequest {  }, pTagsEnd)
  | otherwise = error $ "wirePeek ListClientMetricsResourcesRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec ListClientMetricsResourcesRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeListClientMetricsResourcesRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeListClientMetricsResourcesRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekListClientMetricsResourcesRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}