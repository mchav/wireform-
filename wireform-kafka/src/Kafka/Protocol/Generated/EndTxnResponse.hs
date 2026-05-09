{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.EndTxnResponse
Description : Kafka EndTxnResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 26.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.EndTxnResponse
  (
    EndTxnResponse(..),
    maxEndTxnResponseVersion
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




data EndTxnResponse = EndTxnResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  endTxnResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  endTxnResponseErrorCode :: !(Int16)
,

  -- | The producer ID.

  -- Versions: 5+
  endTxnResponseProducerId :: !(Int64)
,

  -- | The current epoch associated with the producer.

  -- Versions: 5+
  endTxnResponseProducerEpoch :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for EndTxnResponse.
maxEndTxnResponseVersion :: Int16
maxEndTxnResponseVersion = 5

-- | KafkaMessage instance for EndTxnResponse.
instance KafkaMessage EndTxnResponse where
  messageApiKey = 26
  messageMinVersion = 0
  messageMaxVersion = 5
  messageFlexibleVersion = Just 3


-- | Worst-case wire size of a EndTxnResponse.
wireMaxSizeEndTxnResponse :: Int -> EndTxnResponse -> Int
wireMaxSizeEndTxnResponse _version msg =
  0
  + 4
  + 2
  + 8
  + 2
  + 1

-- | Direct-poke encoder for EndTxnResponse.
wirePokeEndTxnResponse :: Int -> Ptr Word8 -> EndTxnResponse -> IO (Ptr Word8)
wirePokeEndTxnResponse version basePtr msg
  | version == 5 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (endTxnResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (endTxnResponseErrorCode msg)
    p3 <- W.pokeInt64BE p2 (endTxnResponseProducerId msg)
    p4 <- W.pokeInt16BE p3 (endTxnResponseProducerEpoch msg)
    WP.pokeEmptyTaggedFields p4
  | version >= 3 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (endTxnResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (endTxnResponseErrorCode msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (endTxnResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (endTxnResponseErrorCode msg)
    pure p2
  | otherwise = error $ "wirePoke EndTxnResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for EndTxnResponse.
wirePeekEndTxnResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EndTxnResponse, Ptr Word8)
wirePeekEndTxnResponse version _fp _basePtr p0 endPtr
  | version == 5 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_producerid, p3) <- W.peekInt64BE p2 endPtr
    (f3_producerepoch, p4) <- W.peekInt16BE p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (EndTxnResponse { endTxnResponseThrottleTimeMs = f0_throttletimems, endTxnResponseErrorCode = f1_errorcode, endTxnResponseProducerId = f2_producerid, endTxnResponseProducerEpoch = f3_producerepoch }, pTagsEnd)
  | version >= 3 && version <= 4 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (EndTxnResponse { endTxnResponseThrottleTimeMs = f0_throttletimems, endTxnResponseErrorCode = f1_errorcode, endTxnResponseProducerId = 0, endTxnResponseProducerEpoch = 0 }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    pure (EndTxnResponse { endTxnResponseThrottleTimeMs = f0_throttletimems, endTxnResponseErrorCode = f1_errorcode, endTxnResponseProducerId = 0, endTxnResponseProducerEpoch = 0 }, p2)
  | otherwise = error $ "wirePeek EndTxnResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec EndTxnResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeEndTxnResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeEndTxnResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekEndTxnResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}