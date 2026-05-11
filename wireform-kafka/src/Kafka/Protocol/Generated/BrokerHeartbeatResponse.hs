{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.BrokerHeartbeatResponse
Description : Kafka BrokerHeartbeatResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 63.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.BrokerHeartbeatResponse
  (
    BrokerHeartbeatResponse(..),
    maxBrokerHeartbeatResponseVersion
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




data BrokerHeartbeatResponse = BrokerHeartbeatResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  brokerHeartbeatResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  brokerHeartbeatResponseErrorCode :: !(Int16)
,

  -- | True if the broker has approximately caught up with the latest metadata.

  -- Versions: 0+
  brokerHeartbeatResponseIsCaughtUp :: !(Bool)
,

  -- | True if the broker is fenced.

  -- Versions: 0+
  brokerHeartbeatResponseIsFenced :: !(Bool)
,

  -- | True if the broker should proceed with its shutdown.

  -- Versions: 0+
  brokerHeartbeatResponseShouldShutDown :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for BrokerHeartbeatResponse.
maxBrokerHeartbeatResponseVersion :: Int16
maxBrokerHeartbeatResponseVersion = 1

-- | KafkaMessage instance for BrokerHeartbeatResponse.
instance KafkaMessage BrokerHeartbeatResponse where
  messageApiKey = 63
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a BrokerHeartbeatResponse.
wireMaxSizeBrokerHeartbeatResponse :: Int -> BrokerHeartbeatResponse -> Int
wireMaxSizeBrokerHeartbeatResponse _version msg =
  0
  + 4
  + 2
  + 1
  + 1
  + 1
  + 1

-- | Direct-poke encoder for BrokerHeartbeatResponse.
wirePokeBrokerHeartbeatResponse :: Int -> Ptr Word8 -> BrokerHeartbeatResponse -> IO (Ptr Word8)
wirePokeBrokerHeartbeatResponse version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (brokerHeartbeatResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (brokerHeartbeatResponseErrorCode msg)
    p3 <- W.pokeWord8 p2 (if (brokerHeartbeatResponseIsCaughtUp msg) then 1 else 0)
    p4 <- W.pokeWord8 p3 (if (brokerHeartbeatResponseIsFenced msg) then 1 else 0)
    p5 <- W.pokeWord8 p4 (if (brokerHeartbeatResponseShouldShutDown msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p5
  | otherwise = error $ "wirePoke BrokerHeartbeatResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for BrokerHeartbeatResponse.
wirePeekBrokerHeartbeatResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (BrokerHeartbeatResponse, Ptr Word8)
wirePeekBrokerHeartbeatResponse version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_iscaughtup, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
    (f3_isfenced, p4) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr
    (f4_shouldshutdown, p5) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p4 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (BrokerHeartbeatResponse { brokerHeartbeatResponseThrottleTimeMs = f0_throttletimems, brokerHeartbeatResponseErrorCode = f1_errorcode, brokerHeartbeatResponseIsCaughtUp = f2_iscaughtup, brokerHeartbeatResponseIsFenced = f3_isfenced, brokerHeartbeatResponseShouldShutDown = f4_shouldshutdown }, pTagsEnd)
  | otherwise = error $ "wirePeek BrokerHeartbeatResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec BrokerHeartbeatResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeBrokerHeartbeatResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeBrokerHeartbeatResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekBrokerHeartbeatResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}