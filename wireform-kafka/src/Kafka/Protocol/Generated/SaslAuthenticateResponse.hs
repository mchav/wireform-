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
    encodeSaslAuthenticateResponse,
    decodeSaslAuthenticateResponse,
    maxSaslAuthenticateResponseVersion
  ) where

import Control.Monad (when)
import Data.Bytes.Get (MonadGet)
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
import qualified Kafka.Protocol.Wire.Codec as WC




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

-- | Encode SaslAuthenticateResponse with the given API version.
encodeSaslAuthenticateResponse :: MonadPut m => E.ApiVersion -> SaslAuthenticateResponse -> m ()
encodeSaslAuthenticateResponse version msg
  | version == 0 =
    do
      serialize (saslAuthenticateResponseErrorCode msg)
      serialize (saslAuthenticateResponseErrorMessage msg)
      serialize (saslAuthenticateResponseAuthBytes msg)


  | version == 1 =
    do
      serialize (saslAuthenticateResponseErrorCode msg)
      serialize (saslAuthenticateResponseErrorMessage msg)
      serialize (saslAuthenticateResponseAuthBytes msg)
      serialize (saslAuthenticateResponseSessionLifetimeMs msg)


  | version == 2 =
    do
      serialize (saslAuthenticateResponseErrorCode msg)
      serialize (toCompactString (saslAuthenticateResponseErrorMessage msg))
      serialize (toCompactBytes (saslAuthenticateResponseAuthBytes msg))
      serialize (saslAuthenticateResponseSessionLifetimeMs msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode SaslAuthenticateResponse with the given API version.
decodeSaslAuthenticateResponse :: MonadGet m => E.ApiVersion -> m SaslAuthenticateResponse
decodeSaslAuthenticateResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      fielderrormessage <- deserialize
      fieldauthbytes <- deserialize
      pure SaslAuthenticateResponse
        {
        saslAuthenticateResponseErrorCode = fielderrorcode
        ,
        saslAuthenticateResponseErrorMessage = fielderrormessage
        ,
        saslAuthenticateResponseAuthBytes = fieldauthbytes
        ,
        saslAuthenticateResponseSessionLifetimeMs = 0
        }

  | version == 1 =
    do
      fielderrorcode <- deserialize
      fielderrormessage <- deserialize
      fieldauthbytes <- deserialize
      fieldsessionlifetimems <- deserialize
      pure SaslAuthenticateResponse
        {
        saslAuthenticateResponseErrorCode = fielderrorcode
        ,
        saslAuthenticateResponseErrorMessage = fielderrormessage
        ,
        saslAuthenticateResponseAuthBytes = fieldauthbytes
        ,
        saslAuthenticateResponseSessionLifetimeMs = fieldsessionlifetimems
        }

  | version == 2 =
    do
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldauthbytes <- if version >= 2 then P.fromCompactBytes <$> deserialize else deserialize
      fieldsessionlifetimems <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure SaslAuthenticateResponse
        {
        saslAuthenticateResponseErrorCode = fielderrorcode
        ,
        saslAuthenticateResponseErrorMessage = fielderrormessage
        ,
        saslAuthenticateResponseAuthBytes = fieldauthbytes
        ,
        saslAuthenticateResponseSessionLifetimeMs = fieldsessionlifetimems
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec SaslAuthenticateResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
