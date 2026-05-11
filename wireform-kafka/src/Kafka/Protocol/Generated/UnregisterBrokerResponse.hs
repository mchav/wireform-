{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.UnregisterBrokerResponse
Description : Kafka UnregisterBrokerResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 64.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.UnregisterBrokerResponse
  (
    UnregisterBrokerResponse(..),
    maxUnregisterBrokerResponseVersion
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




data UnregisterBrokerResponse = UnregisterBrokerResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  unregisterBrokerResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  unregisterBrokerResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or `null` if there was no top-level error.

  -- Versions: 0+
  unregisterBrokerResponseErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for UnregisterBrokerResponse.
maxUnregisterBrokerResponseVersion :: Int16
maxUnregisterBrokerResponseVersion = 0

-- | KafkaMessage instance for UnregisterBrokerResponse.
instance KafkaMessage UnregisterBrokerResponse where
  messageApiKey = 64
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a UnregisterBrokerResponse.
wireMaxSizeUnregisterBrokerResponse :: Int -> UnregisterBrokerResponse -> Int
wireMaxSizeUnregisterBrokerResponse _version msg =
  0
  + 4
  + 2
  + WP.dualStringMaxSize (unregisterBrokerResponseErrorMessage msg)
  + 1

-- | Direct-poke encoder for UnregisterBrokerResponse.
wirePokeUnregisterBrokerResponse :: Int -> Ptr Word8 -> UnregisterBrokerResponse -> IO (Ptr Word8)
wirePokeUnregisterBrokerResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (unregisterBrokerResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (unregisterBrokerResponseErrorCode msg)
    p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (unregisterBrokerResponseErrorMessage msg)) else WP.pokeKafkaString p2 (unregisterBrokerResponseErrorMessage msg))
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke UnregisterBrokerResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for UnregisterBrokerResponse.
wirePeekUnregisterBrokerResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (UnregisterBrokerResponse, Ptr Word8)
wirePeekUnregisterBrokerResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (UnregisterBrokerResponse { unregisterBrokerResponseThrottleTimeMs = f0_throttletimems, unregisterBrokerResponseErrorCode = f1_errorcode, unregisterBrokerResponseErrorMessage = f2_errormessage }, pTagsEnd)
  | otherwise = error $ "wirePeek UnregisterBrokerResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec UnregisterBrokerResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeUnregisterBrokerResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeUnregisterBrokerResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekUnregisterBrokerResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}