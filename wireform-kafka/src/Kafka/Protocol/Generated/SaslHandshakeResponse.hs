{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.SaslHandshakeResponse
Description : Kafka SaslHandshakeResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 17.



Valid versions: 0-1
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.SaslHandshakeResponse
  (
    SaslHandshakeResponse(..),
    maxSaslHandshakeResponseVersion
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




data SaslHandshakeResponse = SaslHandshakeResponse
  {

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  saslHandshakeResponseErrorCode :: !(Int16)
,

  -- | The mechanisms enabled in the server.

  -- Versions: 0+
  saslHandshakeResponseMechanisms :: !(KafkaArray (KafkaString))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for SaslHandshakeResponse.
maxSaslHandshakeResponseVersion :: Int16
maxSaslHandshakeResponseVersion = 1

-- | KafkaMessage instance for SaslHandshakeResponse.
instance KafkaMessage SaslHandshakeResponse where
  messageApiKey = 17
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Nothing


-- | Worst-case wire size of a SaslHandshakeResponse.
wireMaxSizeSaslHandshakeResponse :: Int -> SaslHandshakeResponse -> Int
wireMaxSizeSaslHandshakeResponse _version msg =
  0
  + 2
  + (5 + (case P.unKafkaArray (saslHandshakeResponseMechanisms msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))


-- | Direct-poke encoder for SaslHandshakeResponse.
wirePokeSaslHandshakeResponse :: Int -> Ptr Word8 -> SaslHandshakeResponse -> IO (Ptr Word8)
wirePokeSaslHandshakeResponse version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (saslHandshakeResponseErrorCode msg)
    p2 <- WP.pokeKafkaArray WP.pokeKafkaString p1 (saslHandshakeResponseMechanisms msg)
    pure p2
  | otherwise = error $ "wirePoke SaslHandshakeResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for SaslHandshakeResponse.
wirePeekSaslHandshakeResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SaslHandshakeResponse, Ptr Word8)
wirePeekSaslHandshakeResponse version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_mechanisms, p2) <- WP.peekKafkaArray WP.peekKafkaString p1 endPtr
    pure (SaslHandshakeResponse { saslHandshakeResponseErrorCode = f0_errorcode, saslHandshakeResponseMechanisms = f1_mechanisms }, p2)
  | otherwise = error $ "wirePeek SaslHandshakeResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec SaslHandshakeResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeSaslHandshakeResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeSaslHandshakeResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekSaslHandshakeResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}