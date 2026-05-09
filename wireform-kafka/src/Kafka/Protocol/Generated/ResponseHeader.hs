{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ResponseHeader
Description : Kafka ResponseHeader message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka header (no API key).



Valid versions: 0-1
Flexible versions: 1+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ResponseHeader
  (
    ResponseHeader(..),
    encodeResponseHeader,
    decodeResponseHeader,
    maxResponseHeaderVersion
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




data ResponseHeader = ResponseHeader
  {

  -- | The correlation ID of this response.

  -- Versions: 0+
  responseHeaderCorrelationId :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ResponseHeader.
maxResponseHeaderVersion :: Int16
maxResponseHeaderVersion = 1

-- | Encode ResponseHeader with the given API version.
encodeResponseHeader :: MonadPut m => E.ApiVersion -> ResponseHeader -> m ()
encodeResponseHeader version msg
  | version == 0 =
    do
      serialize (responseHeaderCorrelationId msg)


  | version == 1 =
    do
      serialize (responseHeaderCorrelationId msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ResponseHeader with the given API version.
decodeResponseHeader :: MonadGet m => E.ApiVersion -> m ResponseHeader
decodeResponseHeader version
  | version == 0 =
    do
      fieldcorrelationid <- deserialize
      pure ResponseHeader
        {
        responseHeaderCorrelationId = fieldcorrelationid
        }

  | version == 1 =
    do
      fieldcorrelationid <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ResponseHeader
        {
        responseHeaderCorrelationId = fieldcorrelationid
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ResponseHeader where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
