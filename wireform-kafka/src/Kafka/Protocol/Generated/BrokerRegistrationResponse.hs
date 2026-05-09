{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.BrokerRegistrationResponse
Description : Kafka BrokerRegistrationResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 62.



Valid versions: 0-4
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.BrokerRegistrationResponse
  (
    BrokerRegistrationResponse(..),
    maxBrokerRegistrationResponseVersion
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




data BrokerRegistrationResponse = BrokerRegistrationResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  brokerRegistrationResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  brokerRegistrationResponseErrorCode :: !(Int16)
,

  -- | The broker's assigned epoch, or -1 if none was assigned.

  -- Versions: 0+
  brokerRegistrationResponseBrokerEpoch :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for BrokerRegistrationResponse.
maxBrokerRegistrationResponseVersion :: Int16
maxBrokerRegistrationResponseVersion = 4

-- | KafkaMessage instance for BrokerRegistrationResponse.
instance KafkaMessage BrokerRegistrationResponse where
  messageApiKey = 62
  messageMinVersion = 0
  messageMaxVersion = 4
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a BrokerRegistrationResponse.
wireMaxSizeBrokerRegistrationResponse :: Int -> BrokerRegistrationResponse -> Int
wireMaxSizeBrokerRegistrationResponse _version msg =
  0
  + 4
  + 2
  + 8
  + 1

-- | Direct-poke encoder for BrokerRegistrationResponse.
wirePokeBrokerRegistrationResponse :: Int -> Ptr Word8 -> BrokerRegistrationResponse -> IO (Ptr Word8)
wirePokeBrokerRegistrationResponse version basePtr msg
  | version >= 0 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (brokerRegistrationResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (brokerRegistrationResponseErrorCode msg)
    p3 <- W.pokeInt64BE p2 (brokerRegistrationResponseBrokerEpoch msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke BrokerRegistrationResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for BrokerRegistrationResponse.
wirePeekBrokerRegistrationResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (BrokerRegistrationResponse, Ptr Word8)
wirePeekBrokerRegistrationResponse version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 4 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_brokerepoch, p3) <- W.peekInt64BE p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (BrokerRegistrationResponse { brokerRegistrationResponseThrottleTimeMs = f0_throttletimems, brokerRegistrationResponseErrorCode = f1_errorcode, brokerRegistrationResponseBrokerEpoch = f2_brokerepoch }, pTagsEnd)
  | otherwise = error $ "wirePeek BrokerRegistrationResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec BrokerRegistrationResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeBrokerRegistrationResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeBrokerRegistrationResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekBrokerRegistrationResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}