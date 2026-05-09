{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.HeartbeatResponse
Description : Kafka HeartbeatResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 12.



Valid versions: 0-4
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.HeartbeatResponse
  (
    HeartbeatResponse(..),
    maxHeartbeatResponseVersion
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




data HeartbeatResponse = HeartbeatResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  heartbeatResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  heartbeatResponseErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for HeartbeatResponse.
maxHeartbeatResponseVersion :: Int16
maxHeartbeatResponseVersion = 4

-- | KafkaMessage instance for HeartbeatResponse.
instance KafkaMessage HeartbeatResponse where
  messageApiKey = 12
  messageMinVersion = 0
  messageMaxVersion = 4
  messageFlexibleVersion = Just 4


-- | Worst-case wire size of a HeartbeatResponse.
wireMaxSizeHeartbeatResponse :: Int -> HeartbeatResponse -> Int
wireMaxSizeHeartbeatResponse _version msg =
  0
  + 4
  + 2
  + 1

-- | Direct-poke encoder for HeartbeatResponse.
wirePokeHeartbeatResponse :: Int -> Ptr Word8 -> HeartbeatResponse -> IO (Ptr Word8)
wirePokeHeartbeatResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (heartbeatResponseErrorCode msg)
    pure p1
  | version == 4 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (heartbeatResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (heartbeatResponseErrorCode msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 1 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (heartbeatResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (heartbeatResponseErrorCode msg)
    pure p2
  | otherwise = error $ "wirePoke HeartbeatResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for HeartbeatResponse.
wirePeekHeartbeatResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (HeartbeatResponse, Ptr Word8)
wirePeekHeartbeatResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    pure (HeartbeatResponse { heartbeatResponseThrottleTimeMs = 0, heartbeatResponseErrorCode = f0_errorcode }, p1)
  | version == 4 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (HeartbeatResponse { heartbeatResponseThrottleTimeMs = f0_throttletimems, heartbeatResponseErrorCode = f1_errorcode }, pTagsEnd)
  | version >= 1 && version <= 3 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    pure (HeartbeatResponse { heartbeatResponseThrottleTimeMs = f0_throttletimems, heartbeatResponseErrorCode = f1_errorcode }, p2)
  | otherwise = error $ "wirePeek HeartbeatResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec HeartbeatResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeHeartbeatResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeHeartbeatResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekHeartbeatResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}