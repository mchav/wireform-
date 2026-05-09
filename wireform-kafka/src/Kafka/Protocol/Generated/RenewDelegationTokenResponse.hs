{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.RenewDelegationTokenResponse
Description : Kafka RenewDelegationTokenResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 39.



Valid versions: 1-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.RenewDelegationTokenResponse
  (
    RenewDelegationTokenResponse(..),
    encodeRenewDelegationTokenResponse,
    decodeRenewDelegationTokenResponse,
    maxRenewDelegationTokenResponseVersion
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




data RenewDelegationTokenResponse = RenewDelegationTokenResponse
  {

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  renewDelegationTokenResponseErrorCode :: !(Int16)
,

  -- | The timestamp in milliseconds at which this token expires.

  -- Versions: 0+
  renewDelegationTokenResponseExpiryTimestampMs :: !(Int64)
,

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  renewDelegationTokenResponseThrottleTimeMs :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for RenewDelegationTokenResponse.
maxRenewDelegationTokenResponseVersion :: Int16
maxRenewDelegationTokenResponseVersion = 2

-- | KafkaMessage instance for RenewDelegationTokenResponse.
instance KafkaMessage RenewDelegationTokenResponse where
  messageApiKey = 39
  messageMinVersion = 1
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2

-- | Encode RenewDelegationTokenResponse with the given API version.
encodeRenewDelegationTokenResponse :: MonadPut m => E.ApiVersion -> RenewDelegationTokenResponse -> m ()
encodeRenewDelegationTokenResponse version msg
  | version == 1 =
    do
      serialize (renewDelegationTokenResponseErrorCode msg)
      serialize (renewDelegationTokenResponseExpiryTimestampMs msg)
      serialize (renewDelegationTokenResponseThrottleTimeMs msg)


  | version == 2 =
    do
      serialize (renewDelegationTokenResponseErrorCode msg)
      serialize (renewDelegationTokenResponseExpiryTimestampMs msg)
      serialize (renewDelegationTokenResponseThrottleTimeMs msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode RenewDelegationTokenResponse with the given API version.
decodeRenewDelegationTokenResponse :: MonadGet m => E.ApiVersion -> m RenewDelegationTokenResponse
decodeRenewDelegationTokenResponse version
  | version == 1 =
    do
      fielderrorcode <- deserialize
      fieldexpirytimestampms <- deserialize
      fieldthrottletimems <- deserialize
      pure RenewDelegationTokenResponse
        {
        renewDelegationTokenResponseErrorCode = fielderrorcode
        ,
        renewDelegationTokenResponseExpiryTimestampMs = fieldexpirytimestampms
        ,
        renewDelegationTokenResponseThrottleTimeMs = fieldthrottletimems
        }

  | version == 2 =
    do
      fielderrorcode <- deserialize
      fieldexpirytimestampms <- deserialize
      fieldthrottletimems <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure RenewDelegationTokenResponse
        {
        renewDelegationTokenResponseErrorCode = fielderrorcode
        ,
        renewDelegationTokenResponseExpiryTimestampMs = fieldexpirytimestampms
        ,
        renewDelegationTokenResponseThrottleTimeMs = fieldthrottletimems
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


-- | Worst-case wire size of a RenewDelegationTokenResponse.
wireMaxSizeRenewDelegationTokenResponse :: Int -> RenewDelegationTokenResponse -> Int
wireMaxSizeRenewDelegationTokenResponse _version msg =
  0
  + 2
  + 8
  + 4
  + 1

-- | Direct-poke encoder for RenewDelegationTokenResponse.
wirePokeRenewDelegationTokenResponse :: Int -> Ptr Word8 -> RenewDelegationTokenResponse -> IO (Ptr Word8)
wirePokeRenewDelegationTokenResponse version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (renewDelegationTokenResponseErrorCode msg)
    p2 <- W.pokeInt64BE p1 (renewDelegationTokenResponseExpiryTimestampMs msg)
    p3 <- W.pokeInt32BE p2 (renewDelegationTokenResponseThrottleTimeMs msg)
    pure p3
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (renewDelegationTokenResponseErrorCode msg)
    p2 <- W.pokeInt64BE p1 (renewDelegationTokenResponseExpiryTimestampMs msg)
    p3 <- W.pokeInt32BE p2 (renewDelegationTokenResponseThrottleTimeMs msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke RenewDelegationTokenResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for RenewDelegationTokenResponse.
wirePeekRenewDelegationTokenResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (RenewDelegationTokenResponse, Ptr Word8)
wirePeekRenewDelegationTokenResponse version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_expirytimestampms, p2) <- W.peekInt64BE p1 endPtr
    (f2_throttletimems, p3) <- W.peekInt32BE p2 endPtr
    pure (RenewDelegationTokenResponse { renewDelegationTokenResponseErrorCode = f0_errorcode, renewDelegationTokenResponseExpiryTimestampMs = f1_expirytimestampms, renewDelegationTokenResponseThrottleTimeMs = f2_throttletimems }, p3)
  | version == 2 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_expirytimestampms, p2) <- W.peekInt64BE p1 endPtr
    (f2_throttletimems, p3) <- W.peekInt32BE p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (RenewDelegationTokenResponse { renewDelegationTokenResponseErrorCode = f0_errorcode, renewDelegationTokenResponseExpiryTimestampMs = f1_expirytimestampms, renewDelegationTokenResponseThrottleTimeMs = f2_throttletimems }, pTagsEnd)
  | otherwise = error $ "wirePeek RenewDelegationTokenResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec RenewDelegationTokenResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeRenewDelegationTokenResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeRenewDelegationTokenResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekRenewDelegationTokenResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}