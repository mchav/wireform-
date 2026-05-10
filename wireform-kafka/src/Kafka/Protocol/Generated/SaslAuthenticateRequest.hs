{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.SaslAuthenticateRequest
Description : Kafka SaslAuthenticateRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 36.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.SaslAuthenticateRequest
  (
    SaslAuthenticateRequest(..),
    maxSaslAuthenticateRequestVersion
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




data SaslAuthenticateRequest = SaslAuthenticateRequest
  {

  -- | The SASL authentication bytes from the client, as defined by the SASL mechanism.

  -- Versions: 0+
  saslAuthenticateRequestAuthBytes :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for SaslAuthenticateRequest.
maxSaslAuthenticateRequestVersion :: Int16
maxSaslAuthenticateRequestVersion = 2

-- | KafkaMessage instance for SaslAuthenticateRequest.
instance KafkaMessage SaslAuthenticateRequest where
  messageApiKey = 36
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2


-- | Worst-case wire size of a SaslAuthenticateRequest.
wireMaxSizeSaslAuthenticateRequest :: Int -> SaslAuthenticateRequest -> Int
wireMaxSizeSaslAuthenticateRequest _version msg =
  0
  + WP.compactBytesMaxSize (P.toCompactBytes (saslAuthenticateRequestAuthBytes msg))
  + 1

-- | Direct-poke encoder for SaslAuthenticateRequest.
wirePokeSaslAuthenticateRequest :: Int -> Ptr Word8 -> SaslAuthenticateRequest -> IO (Ptr Word8)
wirePokeSaslAuthenticateRequest version basePtr msg
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then WP.pokeCompactBytes p0 (P.toCompactBytes (saslAuthenticateRequestAuthBytes msg)) else WP.pokeKafkaBytes p0 (saslAuthenticateRequestAuthBytes msg))
    WP.pokeEmptyTaggedFields p1
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then WP.pokeCompactBytes p0 (P.toCompactBytes (saslAuthenticateRequestAuthBytes msg)) else WP.pokeKafkaBytes p0 (saslAuthenticateRequestAuthBytes msg))
    pure p1
  | otherwise = error $ "wirePoke SaslAuthenticateRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for SaslAuthenticateRequest.
wirePeekSaslAuthenticateRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SaslAuthenticateRequest, Ptr Word8)
wirePeekSaslAuthenticateRequest version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_authbytes, p1) <- (if version >= 2 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p0 endPtr else WP.peekKafkaBytes p0 endPtr)
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (SaslAuthenticateRequest { saslAuthenticateRequestAuthBytes = f0_authbytes }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_authbytes, p1) <- (if version >= 2 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p0 endPtr else WP.peekKafkaBytes p0 endPtr)
    pure (SaslAuthenticateRequest { saslAuthenticateRequestAuthBytes = f0_authbytes }, p1)
  | otherwise = error $ "wirePeek SaslAuthenticateRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec SaslAuthenticateRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeSaslAuthenticateRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeSaslAuthenticateRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekSaslAuthenticateRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}