{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.SaslAuthenticateResponse
Description : Kafka SaslAuthenticateResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 36.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.SaslAuthenticateResponse
  (
    SaslAuthenticateResponse(..),
    maxSaslAuthenticateResponseVersion
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




data SaslAuthenticateResponse = SaslAuthenticateResponse
  {

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  saslAuthenticateResponseErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 0+
  saslAuthenticateResponseErrorMessage :: !(KafkaString)
,

  -- | The SASL authentication bytes from the server, as defined by the SASL mechanism.

  -- Versions: 0+
  saslAuthenticateResponseAuthBytes :: !(KafkaBytes)
,

  -- | Number of milliseconds after which only re-authentication over the existing connection to create a n

  -- Versions: 1+
  saslAuthenticateResponseSessionLifetimeMs :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for SaslAuthenticateResponse.
maxSaslAuthenticateResponseVersion :: Int16
maxSaslAuthenticateResponseVersion = 2

-- | KafkaMessage instance for SaslAuthenticateResponse.
instance KafkaMessage SaslAuthenticateResponse where
  messageApiKey = 36
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2


-- | Worst-case wire size of a SaslAuthenticateResponse.
wireMaxSizeSaslAuthenticateResponse :: Int -> SaslAuthenticateResponse -> Int
wireMaxSizeSaslAuthenticateResponse _version msg =
  0
  + 2
  + WP.dualStringMaxSize (saslAuthenticateResponseErrorMessage msg)
  + WP.dualBytesMaxSize (saslAuthenticateResponseAuthBytes msg)
  + 8
  + 1

-- | Direct-poke encoder for SaslAuthenticateResponse.
wirePokeSaslAuthenticateResponse :: Int -> Ptr Word8 -> SaslAuthenticateResponse -> IO (Ptr Word8)
wirePokeSaslAuthenticateResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (saslAuthenticateResponseErrorCode msg)
    p2 <- (if version >= 2 then WP.pokeCompactString p1 (P.toCompactString (saslAuthenticateResponseErrorMessage msg)) else WP.pokeKafkaString p1 (saslAuthenticateResponseErrorMessage msg))
    p3 <- (if version >= 2 then WP.pokeCompactBytes p2 (P.toCompactBytes (saslAuthenticateResponseAuthBytes msg)) else WP.pokeKafkaBytes p2 (saslAuthenticateResponseAuthBytes msg))
    pure p3
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (saslAuthenticateResponseErrorCode msg)
    p2 <- (if version >= 2 then WP.pokeCompactString p1 (P.toCompactString (saslAuthenticateResponseErrorMessage msg)) else WP.pokeKafkaString p1 (saslAuthenticateResponseErrorMessage msg))
    p3 <- (if version >= 2 then WP.pokeCompactBytes p2 (P.toCompactBytes (saslAuthenticateResponseAuthBytes msg)) else WP.pokeKafkaBytes p2 (saslAuthenticateResponseAuthBytes msg))
    p4 <- (if version >= 1 then W.pokeInt64BE p3 (saslAuthenticateResponseSessionLifetimeMs msg) else pure p3)
    pure p4
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (saslAuthenticateResponseErrorCode msg)
    p2 <- (if version >= 2 then WP.pokeCompactString p1 (P.toCompactString (saslAuthenticateResponseErrorMessage msg)) else WP.pokeKafkaString p1 (saslAuthenticateResponseErrorMessage msg))
    p3 <- (if version >= 2 then WP.pokeCompactBytes p2 (P.toCompactBytes (saslAuthenticateResponseAuthBytes msg)) else WP.pokeKafkaBytes p2 (saslAuthenticateResponseAuthBytes msg))
    p4 <- (if version >= 1 then W.pokeInt64BE p3 (saslAuthenticateResponseSessionLifetimeMs msg) else pure p3)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke SaslAuthenticateResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for SaslAuthenticateResponse.
wirePeekSaslAuthenticateResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SaslAuthenticateResponse, Ptr Word8)
wirePeekSaslAuthenticateResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_errormessage, p2) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
    (f2_authbytes, p3) <- (if version >= 2 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p2 endPtr else WP.peekKafkaBytes p2 endPtr)
    pure (SaslAuthenticateResponse { saslAuthenticateResponseErrorCode = f0_errorcode, saslAuthenticateResponseErrorMessage = f1_errormessage, saslAuthenticateResponseAuthBytes = f2_authbytes, saslAuthenticateResponseSessionLifetimeMs = 0 }, p3)
  | version == 1 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_errormessage, p2) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
    (f2_authbytes, p3) <- (if version >= 2 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p2 endPtr else WP.peekKafkaBytes p2 endPtr)
    (f3_sessionlifetimems, p4) <- (if version >= 1 then W.peekInt64BE p3 endPtr else pure (0, p3))
    pure (SaslAuthenticateResponse { saslAuthenticateResponseErrorCode = f0_errorcode, saslAuthenticateResponseErrorMessage = f1_errormessage, saslAuthenticateResponseAuthBytes = f2_authbytes, saslAuthenticateResponseSessionLifetimeMs = f3_sessionlifetimems }, p4)
  | version == 2 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_errormessage, p2) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
    (f2_authbytes, p3) <- (if version >= 2 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p2 endPtr else WP.peekKafkaBytes p2 endPtr)
    (f3_sessionlifetimems, p4) <- (if version >= 1 then W.peekInt64BE p3 endPtr else pure (0, p3))
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (SaslAuthenticateResponse { saslAuthenticateResponseErrorCode = f0_errorcode, saslAuthenticateResponseErrorMessage = f1_errormessage, saslAuthenticateResponseAuthBytes = f2_authbytes, saslAuthenticateResponseSessionLifetimeMs = f3_sessionlifetimems }, pTagsEnd)
  | otherwise = error $ "wirePeek SaslAuthenticateResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec SaslAuthenticateResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeSaslAuthenticateResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeSaslAuthenticateResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekSaslAuthenticateResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}