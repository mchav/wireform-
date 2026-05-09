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

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec SaslAuthenticateRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
