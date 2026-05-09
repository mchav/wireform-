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
    encodeUnregisterBrokerResponse,
    decodeUnregisterBrokerResponse,
    maxUnregisterBrokerResponseVersion
  ) where

import Control.Monad (when)
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
import Data.Bytes.Put (MonadPut)
import Data.Bytes.Serial (Serial(..), serialize, deserialize)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( VarInt(..), VarLong(..), UVarInt(..)
  , KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , CompactString, CompactBytes, CompactArray
  , TaggedFields, emptyTaggedFields, Nullable(..)
  , toCompactString, toCompactBytes, toCompactArray
  )
import qualified Kafka.Protocol.Encoding as E
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
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

-- | Encode UnregisterBrokerResponse with the given API version.
encodeUnregisterBrokerResponse :: MonadPut m => E.ApiVersion -> UnregisterBrokerResponse -> m ()
encodeUnregisterBrokerResponse version msg
  | version == 0 =
    do
      serialize (unregisterBrokerResponseThrottleTimeMs msg)
      serialize (unregisterBrokerResponseErrorCode msg)
      serialize (toCompactString (unregisterBrokerResponseErrorMessage msg))
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode UnregisterBrokerResponse with the given API version.
decodeUnregisterBrokerResponse :: MonadGet m => E.ApiVersion -> m UnregisterBrokerResponse
decodeUnregisterBrokerResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure UnregisterBrokerResponse
        {
        unregisterBrokerResponseThrottleTimeMs = fieldthrottletimems
        ,
        unregisterBrokerResponseErrorCode = fielderrorcode
        ,
        unregisterBrokerResponseErrorMessage = fielderrormessage
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


-- | Worst-case wire size of a UnregisterBrokerResponse.
wireMaxSizeUnregisterBrokerResponse :: Int -> UnregisterBrokerResponse -> Int
wireMaxSizeUnregisterBrokerResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (unregisterBrokerResponseErrorMessage msg))
  + 1

-- | Direct-poke encoder for UnregisterBrokerResponse.
wirePokeUnregisterBrokerResponse :: Int -> Ptr Word8 -> UnregisterBrokerResponse -> IO (Ptr Word8)
wirePokeUnregisterBrokerResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (unregisterBrokerResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (unregisterBrokerResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (unregisterBrokerResponseErrorMessage msg))
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke UnregisterBrokerResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for UnregisterBrokerResponse.
wirePeekUnregisterBrokerResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (UnregisterBrokerResponse, Ptr Word8)
wirePeekUnregisterBrokerResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (UnregisterBrokerResponse { unregisterBrokerResponseThrottleTimeMs = f0_throttletimems, unregisterBrokerResponseErrorCode = f1_errorcode, unregisterBrokerResponseErrorMessage = f2_errormessage }, pTagsEnd)
  | otherwise = error $ "wirePeek UnregisterBrokerResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec UnregisterBrokerResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeUnregisterBrokerResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeUnregisterBrokerResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekUnregisterBrokerResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}