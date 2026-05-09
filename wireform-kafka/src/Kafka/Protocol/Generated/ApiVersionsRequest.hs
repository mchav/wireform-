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
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP




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

----------------------------------------------------------------------
-- Native 'Wire' codec — emitted by
-- "Kafka.Protocol.Codegen.WireGenerator". See the matching block in
-- 'Kafka.Protocol.Generated.RequestHeader' for the full rationale.
----------------------------------------------------------------------

-- | Worst-case wire size of a ApiVersionsRequest.
-- Sums the per-field upper bounds; the actual poke may advance
-- the cursor by less.
wireMaxSizeApiVersionsRequest :: Int -> ApiVersionsRequest -> Int
wireMaxSizeApiVersionsRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (apiVersionsRequestClientSoftwareName msg))
  + WP.compactStringMaxSize (P.toCompactString (apiVersionsRequestClientSoftwareVersion msg))
  + 1
-- | Direct-poke encoder for ApiVersionsRequest.
wirePokeApiVersionsRequest :: Int -> Ptr Word8 -> ApiVersionsRequest -> IO (Ptr Word8)
wirePokeApiVersionsRequest version basePtr msg
  | version >= 3 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (apiVersionsRequestClientSoftwareName msg))
    p2 <- WP.pokeCompactString p1 (P.toCompactString (apiVersionsRequestClientSoftwareVersion msg))
    WP.pokeEmptyTaggedFields p2
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    pure p0
  | otherwise = error $ "wirePoke ApiVersionsRequest : unsupported version: " ++ show version
-- | Direct-poke decoder for ApiVersionsRequest.
wirePeekApiVersionsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ApiVersionsRequest, Ptr Word8)
wirePeekApiVersionsRequest version _fp _basePtr p0 endPtr
  | version >= 3 && version <= 4 = do
    (f0_clientsoftwarename, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_clientsoftwareversion, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (ApiVersionsRequest { apiVersionsRequestClientSoftwareName = f0_clientsoftwarename, apiVersionsRequestClientSoftwareVersion = f1_clientsoftwareversion }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    pure (ApiVersionsRequest { apiVersionsRequestClientSoftwareName = P.KafkaString Null, apiVersionsRequestClientSoftwareVersion = P.KafkaString Null }, p0)
  | otherwise = error $ "wirePeek ApiVersionsRequest : unsupported version: " ++ show version
-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec ApiVersionsRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeApiVersionsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeApiVersionsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekApiVersionsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}
