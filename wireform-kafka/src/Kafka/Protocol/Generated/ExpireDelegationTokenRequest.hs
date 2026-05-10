{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ExpireDelegationTokenRequest
Description : Kafka ExpireDelegationTokenRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 40.



Valid versions: 1-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ExpireDelegationTokenRequest
  (
    ExpireDelegationTokenRequest(..),
    maxExpireDelegationTokenRequestVersion
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




data ExpireDelegationTokenRequest = ExpireDelegationTokenRequest
  {

  -- | The HMAC of the delegation token to be expired.

  -- Versions: 0+
  expireDelegationTokenRequestHmac :: !(KafkaBytes)
,

  -- | The expiry time period in milliseconds.

  -- Versions: 0+
  expireDelegationTokenRequestExpiryTimePeriodMs :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ExpireDelegationTokenRequest.
maxExpireDelegationTokenRequestVersion :: Int16
maxExpireDelegationTokenRequestVersion = 2

-- | KafkaMessage instance for ExpireDelegationTokenRequest.
instance KafkaMessage ExpireDelegationTokenRequest where
  messageApiKey = 40
  messageMinVersion = 1
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2


-- | Worst-case wire size of a ExpireDelegationTokenRequest.
wireMaxSizeExpireDelegationTokenRequest :: Int -> ExpireDelegationTokenRequest -> Int
wireMaxSizeExpireDelegationTokenRequest _version msg =
  0
  + WP.dualBytesMaxSize (expireDelegationTokenRequestHmac msg)
  + 8
  + 1

-- | Direct-poke encoder for ExpireDelegationTokenRequest.
wirePokeExpireDelegationTokenRequest :: Int -> Ptr Word8 -> ExpireDelegationTokenRequest -> IO (Ptr Word8)
wirePokeExpireDelegationTokenRequest version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then WP.pokeCompactBytes p0 (P.toCompactBytes (expireDelegationTokenRequestHmac msg)) else WP.pokeKafkaBytes p0 (expireDelegationTokenRequestHmac msg))
    p2 <- W.pokeInt64BE p1 (expireDelegationTokenRequestExpiryTimePeriodMs msg)
    pure p2
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then WP.pokeCompactBytes p0 (P.toCompactBytes (expireDelegationTokenRequestHmac msg)) else WP.pokeKafkaBytes p0 (expireDelegationTokenRequestHmac msg))
    p2 <- W.pokeInt64BE p1 (expireDelegationTokenRequestExpiryTimePeriodMs msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke ExpireDelegationTokenRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ExpireDelegationTokenRequest.
wirePeekExpireDelegationTokenRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ExpireDelegationTokenRequest, Ptr Word8)
wirePeekExpireDelegationTokenRequest version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_hmac, p1) <- (if version >= 2 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p0 endPtr else WP.peekKafkaBytes p0 endPtr)
    (f1_expirytimeperiodms, p2) <- W.peekInt64BE p1 endPtr
    pure (ExpireDelegationTokenRequest { expireDelegationTokenRequestHmac = f0_hmac, expireDelegationTokenRequestExpiryTimePeriodMs = f1_expirytimeperiodms }, p2)
  | version == 2 = do
    (f0_hmac, p1) <- (if version >= 2 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p0 endPtr else WP.peekKafkaBytes p0 endPtr)
    (f1_expirytimeperiodms, p2) <- W.peekInt64BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (ExpireDelegationTokenRequest { expireDelegationTokenRequestHmac = f0_hmac, expireDelegationTokenRequestExpiryTimePeriodMs = f1_expirytimeperiodms }, pTagsEnd)
  | otherwise = error $ "wirePeek ExpireDelegationTokenRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ExpireDelegationTokenRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeExpireDelegationTokenRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeExpireDelegationTokenRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekExpireDelegationTokenRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}