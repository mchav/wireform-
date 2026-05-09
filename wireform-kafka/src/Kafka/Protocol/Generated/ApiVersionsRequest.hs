{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ApiVersionsRequest
Description : Kafka ApiVersionsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 18.



Valid versions: 0-4
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ApiVersionsRequest
  (
    ApiVersionsRequest(..),
    encodeApiVersionsRequest,
    decodeApiVersionsRequest,
    maxApiVersionsRequestVersion
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




data ApiVersionsRequest = ApiVersionsRequest
  {

  -- | The name of the client.

  -- Versions: 3+
  apiVersionsRequestClientSoftwareName :: !(KafkaString)
,

  -- | The version of the client.

  -- Versions: 3+
  apiVersionsRequestClientSoftwareVersion :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ApiVersionsRequest.
maxApiVersionsRequestVersion :: Int16
maxApiVersionsRequestVersion = 4

-- | Encode ApiVersionsRequest with the given API version.
encodeApiVersionsRequest :: MonadPut m => E.ApiVersion -> ApiVersionsRequest -> m ()
encodeApiVersionsRequest version msg
  | version >= 3 && version <= 4 =
    do
      serialize (toCompactString (apiVersionsRequestClientSoftwareName msg))
      serialize (toCompactString (apiVersionsRequestClientSoftwareVersion msg))
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 2 =
    pure ()
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ApiVersionsRequest with the given API version.
decodeApiVersionsRequest :: MonadGet m => E.ApiVersion -> m ApiVersionsRequest
decodeApiVersionsRequest version
  | version >= 3 && version <= 4 =
    do
      fieldclientsoftwarename <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldclientsoftwareversion <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ApiVersionsRequest
        {
        apiVersionsRequestClientSoftwareName = fieldclientsoftwarename
        ,
        apiVersionsRequestClientSoftwareVersion = fieldclientsoftwareversion
        }

  | version >= 0 && version <= 2 =
    do

      pure ApiVersionsRequest
        {
        apiVersionsRequestClientSoftwareName = P.KafkaString Null
        ,
        apiVersionsRequestClientSoftwareVersion = P.KafkaString Null
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ApiVersionsRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
