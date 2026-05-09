{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.SaslHandshakeRequest
Description : Kafka SaslHandshakeRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 17.



Valid versions: 0-1
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.SaslHandshakeRequest
  (
    SaslHandshakeRequest(..),
    maxSaslHandshakeRequestVersion
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




data SaslHandshakeRequest = SaslHandshakeRequest
  {

  -- | The SASL mechanism chosen by the client.

  -- Versions: 0+
  saslHandshakeRequestMechanism :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for SaslHandshakeRequest.
maxSaslHandshakeRequestVersion :: Int16
maxSaslHandshakeRequestVersion = 1

-- | KafkaMessage instance for SaslHandshakeRequest.
instance KafkaMessage SaslHandshakeRequest where
  messageApiKey = 17
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Nothing


-- | Worst-case wire size of a SaslHandshakeRequest.
wireMaxSizeSaslHandshakeRequest :: Int -> SaslHandshakeRequest -> Int
wireMaxSizeSaslHandshakeRequest _version msg =
  0
  + WP.kafkaStringMaxSize (saslHandshakeRequestMechanism msg)


-- | Direct-poke encoder for SaslHandshakeRequest.
wirePokeSaslHandshakeRequest :: Int -> Ptr Word8 -> SaslHandshakeRequest -> IO (Ptr Word8)
wirePokeSaslHandshakeRequest version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeKafkaString p0 (saslHandshakeRequestMechanism msg)
    pure p1
  | otherwise = error $ "wirePoke SaslHandshakeRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for SaslHandshakeRequest.
wirePeekSaslHandshakeRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SaslHandshakeRequest, Ptr Word8)
wirePeekSaslHandshakeRequest version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_mechanism, p1) <- WP.peekKafkaString p0 endPtr
    pure (SaslHandshakeRequest { saslHandshakeRequestMechanism = f0_mechanism }, p1)
  | otherwise = error $ "wirePeek SaslHandshakeRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec SaslHandshakeRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeSaslHandshakeRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeSaslHandshakeRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekSaslHandshakeRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}