{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.RequestHeader
Description : Kafka RequestHeader message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka header (no API key).



Valid versions: 1-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.RequestHeader
  (
    RequestHeader(..),
    maxRequestHeaderVersion
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




data RequestHeader = RequestHeader
  {

  -- | The API key of this request.

  -- Versions: 0+
  requestHeaderRequestApiKey :: !(Int16)
,

  -- | The API version of this request.

  -- Versions: 0+
  requestHeaderRequestApiVersion :: !(Int16)
,

  -- | The correlation ID of this request.

  -- Versions: 0+
  requestHeaderCorrelationId :: !(Int32)
,

  -- | The client ID string.

  -- Versions: 1+
  requestHeaderClientId :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for RequestHeader.
maxRequestHeaderVersion :: Int16
maxRequestHeaderVersion = 2




-- | Worst-case wire size of a RequestHeader.
wireMaxSizeRequestHeader :: Int -> RequestHeader -> Int
wireMaxSizeRequestHeader _version msg =
  0
  + 2
  + 2
  + 4
  + WP.kafkaStringMaxSize (requestHeaderClientId msg)
  + 1

-- | Direct-poke encoder for RequestHeader.
wirePokeRequestHeader :: Int -> Ptr Word8 -> RequestHeader -> IO (Ptr Word8)
wirePokeRequestHeader version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (requestHeaderRequestApiKey msg)
    p2 <- W.pokeInt16BE p1 (requestHeaderRequestApiVersion msg)
    p3 <- W.pokeInt32BE p2 (requestHeaderCorrelationId msg)
    p4 <- (if version >= 1 then WP.pokeKafkaString p3 (requestHeaderClientId msg) else pure p3)
    pure p4
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (requestHeaderRequestApiKey msg)
    p2 <- W.pokeInt16BE p1 (requestHeaderRequestApiVersion msg)
    p3 <- W.pokeInt32BE p2 (requestHeaderCorrelationId msg)
    p4 <- (if version >= 1 then WP.pokeKafkaString p3 (requestHeaderClientId msg) else pure p3)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke RequestHeader : unsupported version: " ++ show version

-- | Direct-poke decoder for RequestHeader.
wirePeekRequestHeader :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (RequestHeader, Ptr Word8)
wirePeekRequestHeader version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_requestapikey, p1) <- W.peekInt16BE p0 endPtr
    (f1_requestapiversion, p2) <- W.peekInt16BE p1 endPtr
    (f2_correlationid, p3) <- W.peekInt32BE p2 endPtr
    (f3_clientid, p4) <- (if version >= 1 then WP.peekKafkaString p3 endPtr else pure (P.KafkaString Null, p3))
    pure (RequestHeader { requestHeaderRequestApiKey = f0_requestapikey, requestHeaderRequestApiVersion = f1_requestapiversion, requestHeaderCorrelationId = f2_correlationid, requestHeaderClientId = f3_clientid }, p4)
  | version == 2 = do
    (f0_requestapikey, p1) <- W.peekInt16BE p0 endPtr
    (f1_requestapiversion, p2) <- W.peekInt16BE p1 endPtr
    (f2_correlationid, p3) <- W.peekInt32BE p2 endPtr
    (f3_clientid, p4) <- (if version >= 1 then WP.peekKafkaString p3 endPtr else pure (P.KafkaString Null, p3))
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (RequestHeader { requestHeaderRequestApiKey = f0_requestapikey, requestHeaderRequestApiVersion = f1_requestapiversion, requestHeaderCorrelationId = f2_correlationid, requestHeaderClientId = f3_clientid }, pTagsEnd)
  | otherwise = error $ "wirePeek RequestHeader : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec RequestHeader where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeRequestHeader (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeRequestHeader (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekRequestHeader (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}