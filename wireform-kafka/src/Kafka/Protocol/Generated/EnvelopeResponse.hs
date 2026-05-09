{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.EnvelopeResponse
Description : Kafka EnvelopeResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 58.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.EnvelopeResponse
  (
    EnvelopeResponse(..),
    encodeEnvelopeResponse,
    decodeEnvelopeResponse,
    maxEnvelopeResponseVersion
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




data EnvelopeResponse = EnvelopeResponse
  {

  -- | The embedded response header and data.

  -- Versions: 0+
  envelopeResponseResponseData :: !(KafkaBytes)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  envelopeResponseErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for EnvelopeResponse.
maxEnvelopeResponseVersion :: Int16
maxEnvelopeResponseVersion = 0

-- | Encode EnvelopeResponse with the given API version.
encodeEnvelopeResponse :: MonadPut m => E.ApiVersion -> EnvelopeResponse -> m ()
encodeEnvelopeResponse version msg
  | version == 0 =
    do
      serialize (toCompactBytes (envelopeResponseResponseData msg))
      serialize (envelopeResponseErrorCode msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode EnvelopeResponse with the given API version.
decodeEnvelopeResponse :: MonadGet m => E.ApiVersion -> m EnvelopeResponse
decodeEnvelopeResponse version
  | version == 0 =
    do
      fieldresponsedata <- if version >= 0 then P.fromCompactBytes <$> deserialize else deserialize
      fielderrorcode <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure EnvelopeResponse
        {
        envelopeResponseResponseData = fieldresponsedata
        ,
        envelopeResponseErrorCode = fielderrorcode
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec EnvelopeResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
