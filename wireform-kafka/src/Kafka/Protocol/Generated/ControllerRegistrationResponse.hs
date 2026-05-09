{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ControllerRegistrationResponse
Description : Kafka ControllerRegistrationResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 70.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ControllerRegistrationResponse
  (
    ControllerRegistrationResponse(..),
    maxControllerRegistrationResponseVersion
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




data ControllerRegistrationResponse = ControllerRegistrationResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  controllerRegistrationResponseThrottleTimeMs :: !(Int32)
,

  -- | The response error code.

  -- Versions: 0+
  controllerRegistrationResponseErrorCode :: !(Int16)
,

  -- | The response error message, or null if there was no error.

  -- Versions: 0+
  controllerRegistrationResponseErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ControllerRegistrationResponse.
maxControllerRegistrationResponseVersion :: Int16
maxControllerRegistrationResponseVersion = 0

-- | KafkaMessage instance for ControllerRegistrationResponse.
instance KafkaMessage ControllerRegistrationResponse where
  messageApiKey = 70
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a ControllerRegistrationResponse.
wireMaxSizeControllerRegistrationResponse :: Int -> ControllerRegistrationResponse -> Int
wireMaxSizeControllerRegistrationResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (controllerRegistrationResponseErrorMessage msg))
  + 1

-- | Direct-poke encoder for ControllerRegistrationResponse.
wirePokeControllerRegistrationResponse :: Int -> Ptr Word8 -> ControllerRegistrationResponse -> IO (Ptr Word8)
wirePokeControllerRegistrationResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (controllerRegistrationResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (controllerRegistrationResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (controllerRegistrationResponseErrorMessage msg))
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke ControllerRegistrationResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ControllerRegistrationResponse.
wirePeekControllerRegistrationResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ControllerRegistrationResponse, Ptr Word8)
wirePeekControllerRegistrationResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (ControllerRegistrationResponse { controllerRegistrationResponseThrottleTimeMs = f0_throttletimems, controllerRegistrationResponseErrorCode = f1_errorcode, controllerRegistrationResponseErrorMessage = f2_errormessage }, pTagsEnd)
  | otherwise = error $ "wirePeek ControllerRegistrationResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ControllerRegistrationResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeControllerRegistrationResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeControllerRegistrationResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekControllerRegistrationResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}