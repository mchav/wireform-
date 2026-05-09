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


-- | Worst-case wire size of a ResponseHeader.
wireMaxSizeResponseHeader :: Int -> ResponseHeader -> Int
wireMaxSizeResponseHeader _version msg =
  0
  + 4
  + 1

-- | Direct-poke encoder for ResponseHeader.
wirePokeResponseHeader :: Int -> Ptr Word8 -> ResponseHeader -> IO (Ptr Word8)
wirePokeResponseHeader version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (responseHeaderCorrelationId msg)
    pure p1
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (responseHeaderCorrelationId msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke ResponseHeader : unsupported version: " ++ show version

-- | Direct-poke decoder for ResponseHeader.
wirePeekResponseHeader :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ResponseHeader, Ptr Word8)
wirePeekResponseHeader version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_correlationid, p1) <- W.peekInt32BE p0 endPtr
    pure (ResponseHeader { responseHeaderCorrelationId = f0_correlationid }, p1)
  | version == 1 = do
    (f0_correlationid, p1) <- W.peekInt32BE p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (ResponseHeader { responseHeaderCorrelationId = f0_correlationid }, pTagsEnd)
  | otherwise = error $ "wirePeek ResponseHeader : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec ResponseHeader where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeResponseHeader (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeResponseHeader (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekResponseHeader (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}