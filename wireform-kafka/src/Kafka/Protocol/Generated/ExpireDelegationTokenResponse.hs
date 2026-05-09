{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ExpireDelegationTokenResponse
Description : Kafka ExpireDelegationTokenResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 40.



Valid versions: 1-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ExpireDelegationTokenResponse
  (
    ExpireDelegationTokenResponse(..),
    encodeExpireDelegationTokenResponse,
    decodeExpireDelegationTokenResponse,
    maxExpireDelegationTokenResponseVersion
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




data ExpireDelegationTokenResponse = ExpireDelegationTokenResponse
  {

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  expireDelegationTokenResponseErrorCode :: !(Int16)
,

  -- | The timestamp in milliseconds at which this token expires.

  -- Versions: 0+
  expireDelegationTokenResponseExpiryTimestampMs :: !(Int64)
,

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  expireDelegationTokenResponseThrottleTimeMs :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ExpireDelegationTokenResponse.
maxExpireDelegationTokenResponseVersion :: Int16
maxExpireDelegationTokenResponseVersion = 2

-- | KafkaMessage instance for ExpireDelegationTokenResponse.
instance KafkaMessage ExpireDelegationTokenResponse where
  messageApiKey = 40
  messageMinVersion = 1
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2

-- | Encode ExpireDelegationTokenResponse with the given API version.
encodeExpireDelegationTokenResponse :: MonadPut m => E.ApiVersion -> ExpireDelegationTokenResponse -> m ()
encodeExpireDelegationTokenResponse version msg
  | version == 1 =
    do
      serialize (expireDelegationTokenResponseErrorCode msg)
      serialize (expireDelegationTokenResponseExpiryTimestampMs msg)
      serialize (expireDelegationTokenResponseThrottleTimeMs msg)


  | version == 2 =
    do
      serialize (expireDelegationTokenResponseErrorCode msg)
      serialize (expireDelegationTokenResponseExpiryTimestampMs msg)
      serialize (expireDelegationTokenResponseThrottleTimeMs msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ExpireDelegationTokenResponse with the given API version.
decodeExpireDelegationTokenResponse :: MonadGet m => E.ApiVersion -> m ExpireDelegationTokenResponse
decodeExpireDelegationTokenResponse version
  | version == 1 =
    do
      fielderrorcode <- deserialize
      fieldexpirytimestampms <- deserialize
      fieldthrottletimems <- deserialize
      pure ExpireDelegationTokenResponse
        {
        expireDelegationTokenResponseErrorCode = fielderrorcode
        ,
        expireDelegationTokenResponseExpiryTimestampMs = fieldexpirytimestampms
        ,
        expireDelegationTokenResponseThrottleTimeMs = fieldthrottletimems
        }

  | version == 2 =
    do
      fielderrorcode <- deserialize
      fieldexpirytimestampms <- deserialize
      fieldthrottletimems <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ExpireDelegationTokenResponse
        {
        expireDelegationTokenResponseErrorCode = fielderrorcode
        ,
        expireDelegationTokenResponseExpiryTimestampMs = fieldexpirytimestampms
        ,
        expireDelegationTokenResponseThrottleTimeMs = fieldthrottletimems
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


-- | Worst-case wire size of a ExpireDelegationTokenResponse.
wireMaxSizeExpireDelegationTokenResponse :: Int -> ExpireDelegationTokenResponse -> Int
wireMaxSizeExpireDelegationTokenResponse _version msg =
  0
  + 2
  + 8
  + 4
  + 1

-- | Direct-poke encoder for ExpireDelegationTokenResponse.
wirePokeExpireDelegationTokenResponse :: Int -> Ptr Word8 -> ExpireDelegationTokenResponse -> IO (Ptr Word8)
wirePokeExpireDelegationTokenResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (expireDelegationTokenResponseErrorCode msg)
    p2 <- W.pokeInt64BE p1 (expireDelegationTokenResponseExpiryTimestampMs msg)
    p3 <- W.pokeInt32BE p2 (expireDelegationTokenResponseThrottleTimeMs msg)
    pure p3
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (expireDelegationTokenResponseErrorCode msg)
    p2 <- W.pokeInt64BE p1 (expireDelegationTokenResponseExpiryTimestampMs msg)
    p3 <- W.pokeInt32BE p2 (expireDelegationTokenResponseThrottleTimeMs msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke ExpireDelegationTokenResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ExpireDelegationTokenResponse.
wirePeekExpireDelegationTokenResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ExpireDelegationTokenResponse, Ptr Word8)
wirePeekExpireDelegationTokenResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_expirytimestampms, p2) <- W.peekInt64BE p1 endPtr
    (f2_throttletimems, p3) <- W.peekInt32BE p2 endPtr
    pure (ExpireDelegationTokenResponse { expireDelegationTokenResponseErrorCode = f0_errorcode, expireDelegationTokenResponseExpiryTimestampMs = f1_expirytimestampms, expireDelegationTokenResponseThrottleTimeMs = f2_throttletimems }, p3)
  | version == 2 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_expirytimestampms, p2) <- W.peekInt64BE p1 endPtr
    (f2_throttletimems, p3) <- W.peekInt32BE p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (ExpireDelegationTokenResponse { expireDelegationTokenResponseErrorCode = f0_errorcode, expireDelegationTokenResponseExpiryTimestampMs = f1_expirytimestampms, expireDelegationTokenResponseThrottleTimeMs = f2_throttletimems }, pTagsEnd)
  | otherwise = error $ "wirePeek ExpireDelegationTokenResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec ExpireDelegationTokenResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeExpireDelegationTokenResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeExpireDelegationTokenResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekExpireDelegationTokenResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}