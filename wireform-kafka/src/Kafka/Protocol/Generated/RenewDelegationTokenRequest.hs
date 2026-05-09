{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.RenewDelegationTokenRequest
Description : Kafka RenewDelegationTokenRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 39.



Valid versions: 1-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.RenewDelegationTokenRequest
  (
    RenewDelegationTokenRequest(..),
    maxRenewDelegationTokenRequestVersion
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




data RenewDelegationTokenRequest = RenewDelegationTokenRequest
  {

  -- | The HMAC of the delegation token to be renewed.

  -- Versions: 0+
  renewDelegationTokenRequestHmac :: !(KafkaBytes)
,

  -- | The renewal time period in milliseconds.

  -- Versions: 0+
  renewDelegationTokenRequestRenewPeriodMs :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for RenewDelegationTokenRequest.
maxRenewDelegationTokenRequestVersion :: Int16
maxRenewDelegationTokenRequestVersion = 2

-- | KafkaMessage instance for RenewDelegationTokenRequest.
instance KafkaMessage RenewDelegationTokenRequest where
  messageApiKey = 39
  messageMinVersion = 1
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2


-- | Worst-case wire size of a RenewDelegationTokenRequest.
wireMaxSizeRenewDelegationTokenRequest :: Int -> RenewDelegationTokenRequest -> Int
wireMaxSizeRenewDelegationTokenRequest _version msg =
  0
  + WP.compactBytesMaxSize (P.toCompactBytes (renewDelegationTokenRequestHmac msg))
  + 8
  + 1

-- | Direct-poke encoder for RenewDelegationTokenRequest.
wirePokeRenewDelegationTokenRequest :: Int -> Ptr Word8 -> RenewDelegationTokenRequest -> IO (Ptr Word8)
wirePokeRenewDelegationTokenRequest version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactBytes p0 (P.toCompactBytes (renewDelegationTokenRequestHmac msg))
    p2 <- W.pokeInt64BE p1 (renewDelegationTokenRequestRenewPeriodMs msg)
    pure p2
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactBytes p0 (P.toCompactBytes (renewDelegationTokenRequestHmac msg))
    p2 <- W.pokeInt64BE p1 (renewDelegationTokenRequestRenewPeriodMs msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke RenewDelegationTokenRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for RenewDelegationTokenRequest.
wirePeekRenewDelegationTokenRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (RenewDelegationTokenRequest, Ptr Word8)
wirePeekRenewDelegationTokenRequest version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_hmac, p1) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p0 endPtr
    (f1_renewperiodms, p2) <- W.peekInt64BE p1 endPtr
    pure (RenewDelegationTokenRequest { renewDelegationTokenRequestHmac = f0_hmac, renewDelegationTokenRequestRenewPeriodMs = f1_renewperiodms }, p2)
  | version == 2 = do
    (f0_hmac, p1) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p0 endPtr
    (f1_renewperiodms, p2) <- W.peekInt64BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (RenewDelegationTokenRequest { renewDelegationTokenRequestHmac = f0_hmac, renewDelegationTokenRequestRenewPeriodMs = f1_renewperiodms }, pTagsEnd)
  | otherwise = error $ "wirePeek RenewDelegationTokenRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec RenewDelegationTokenRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeRenewDelegationTokenRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeRenewDelegationTokenRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekRenewDelegationTokenRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}