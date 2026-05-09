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
    encodeSaslAuthenticateRequest,
    decodeSaslAuthenticateRequest,
    maxSaslAuthenticateRequestVersion
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

-- | Encode SaslAuthenticateRequest with the given API version.
encodeSaslAuthenticateRequest :: MonadPut m => E.ApiVersion -> SaslAuthenticateRequest -> m ()
encodeSaslAuthenticateRequest version msg
  | version == 2 =
    do
      serialize (toCompactBytes (saslAuthenticateRequestAuthBytes msg))
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 1 =
    do
      serialize (saslAuthenticateRequestAuthBytes msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode SaslAuthenticateRequest with the given API version.
decodeSaslAuthenticateRequest :: MonadGet m => E.ApiVersion -> m SaslAuthenticateRequest
decodeSaslAuthenticateRequest version
  | version == 2 =
    do
      fieldauthbytes <- if version >= 2 then P.fromCompactBytes <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure SaslAuthenticateRequest
        {
        saslAuthenticateRequestAuthBytes = fieldauthbytes
        }

  | version >= 0 && version <= 1 =
    do
      fieldauthbytes <- deserialize
      pure SaslAuthenticateRequest
        {
        saslAuthenticateRequestAuthBytes = fieldauthbytes
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


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
    p1 <- WP.pokeCompactBytes p0 (P.toCompactBytes (saslAuthenticateRequestAuthBytes msg))
    WP.pokeEmptyTaggedFields p1
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactBytes p0 (P.toCompactBytes (saslAuthenticateRequestAuthBytes msg))
    pure p1
  | otherwise = error $ "wirePoke SaslAuthenticateRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for SaslAuthenticateRequest.
wirePeekSaslAuthenticateRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SaslAuthenticateRequest, Ptr Word8)
wirePeekSaslAuthenticateRequest version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_authbytes, p1) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (SaslAuthenticateRequest { saslAuthenticateRequestAuthBytes = f0_authbytes }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_authbytes, p1) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p0 endPtr
    pure (SaslAuthenticateRequest { saslAuthenticateRequestAuthBytes = f0_authbytes }, p1)
  | otherwise = error $ "wirePeek SaslAuthenticateRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec SaslAuthenticateRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeSaslAuthenticateRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeSaslAuthenticateRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekSaslAuthenticateRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}