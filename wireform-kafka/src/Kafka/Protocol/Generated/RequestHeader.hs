{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.RequestHeader
Description : Kafka RequestHeader message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka header (no API key).



Valid versions: 1-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.RequestHeader
  (
    RequestHeader(..),
    encodeRequestHeader,
    decodeRequestHeader,
    maxRequestHeaderVersion
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
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP




data RequestHeader = RequestHeader
  {

  -- | The API key of this request.

  -- Versions: 0+
  requestHeaderRequestApiKey :: !(Int16)
,

  -- | The API version of this request.

  -- Versions: 0+
  requestHeaderRequestApiVersion :: !(Int16)
,

  -- | The correlation ID of this request.

  -- Versions: 0+
  requestHeaderCorrelationId :: !(Int32)
,

  -- | The client ID string.

  -- Versions: 1+
  requestHeaderClientId :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for RequestHeader.
maxRequestHeaderVersion :: Int16
maxRequestHeaderVersion = 2



-- | Encode RequestHeader with the given API version.
encodeRequestHeader :: MonadPut m => E.ApiVersion -> RequestHeader -> m ()
encodeRequestHeader version msg
  | version == 1 =
    do
      serialize (requestHeaderRequestApiKey msg)
      serialize (requestHeaderRequestApiVersion msg)
      serialize (requestHeaderCorrelationId msg)
      serialize (requestHeaderClientId msg)


  | version == 2 =
    do
      serialize (requestHeaderRequestApiKey msg)
      serialize (requestHeaderRequestApiVersion msg)
      serialize (requestHeaderCorrelationId msg)
      serialize (requestHeaderClientId msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode RequestHeader with the given API version.
decodeRequestHeader :: MonadGet m => E.ApiVersion -> m RequestHeader
decodeRequestHeader version
  | version == 1 =
    do
      fieldrequestapikey <- deserialize
      fieldrequestapiversion <- deserialize
      fieldcorrelationid <- deserialize
      fieldclientid <- deserialize
      pure RequestHeader
        {
        requestHeaderRequestApiKey = fieldrequestapikey
        ,
        requestHeaderRequestApiVersion = fieldrequestapiversion
        ,
        requestHeaderCorrelationId = fieldcorrelationid
        ,
        requestHeaderClientId = fieldclientid
        }

  | version == 2 =
    do
      fieldrequestapikey <- deserialize
      fieldrequestapiversion <- deserialize
      fieldcorrelationid <- deserialize
      fieldclientid <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure RequestHeader
        {
        requestHeaderRequestApiKey = fieldrequestapikey
        ,
        requestHeaderRequestApiVersion = fieldrequestapiversion
        ,
        requestHeaderCorrelationId = fieldcorrelationid
        ,
        requestHeaderClientId = fieldclientid
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

----------------------------------------------------------------------
-- Native 'Wire' codec
--
-- The functions below are emitted by
-- "Kafka.Protocol.Codegen.WireGenerator" alongside the legacy
-- 'Serial'-shape @encode@ / @decode@ pair above. Both coexist so
-- callers can flip between them per-call-site, but the @WireCodec@
-- instance at the bottom of this module dispatches every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' through the native pokes.
--
-- The shape is locked down by 'Codegen.WireGeneratorSpec' (snapshot)
-- and 'Protocol.WireCodecParitySpec' (cross-codec byte equivalence).
----------------------------------------------------------------------

-- | Worst-case wire size of a RequestHeader.
-- Sums the per-field upper bounds; the actual poke may advance
-- the cursor by less.
wireMaxSizeRequestHeader :: Int -> RequestHeader -> Int
wireMaxSizeRequestHeader _version msg =
  0
  + 2
  + 2
  + 4
  + WP.kafkaStringMaxSize (requestHeaderClientId msg)
  + 1
-- | Direct-poke encoder for RequestHeader.
wirePokeRequestHeader :: Int -> Ptr Word8 -> RequestHeader -> IO (Ptr Word8)
wirePokeRequestHeader version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (requestHeaderRequestApiKey msg)
    p2 <- W.pokeInt16BE p1 (requestHeaderRequestApiVersion msg)
    p3 <- W.pokeInt32BE p2 (requestHeaderCorrelationId msg)
    p4 <- WP.pokeKafkaString p3 (requestHeaderClientId msg)
    pure p4
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (requestHeaderRequestApiKey msg)
    p2 <- W.pokeInt16BE p1 (requestHeaderRequestApiVersion msg)
    p3 <- W.pokeInt32BE p2 (requestHeaderCorrelationId msg)
    p4 <- WP.pokeKafkaString p3 (requestHeaderClientId msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke RequestHeader : unsupported version: " ++ show version
-- | Direct-poke decoder for RequestHeader.
wirePeekRequestHeader :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (RequestHeader, Ptr Word8)
wirePeekRequestHeader version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_requestapikey, p1) <- W.peekInt16BE p0 endPtr
    (f1_requestapiversion, p2) <- W.peekInt16BE p1 endPtr
    (f2_correlationid, p3) <- W.peekInt32BE p2 endPtr
    (f3_clientid, p4) <- WP.peekKafkaString p3 endPtr
    pure (RequestHeader { requestHeaderRequestApiKey = f0_requestapikey, requestHeaderRequestApiVersion = f1_requestapiversion, requestHeaderCorrelationId = f2_correlationid, requestHeaderClientId = f3_clientid }, p4)
  | version == 2 = do
    (f0_requestapikey, p1) <- W.peekInt16BE p0 endPtr
    (f1_requestapiversion, p2) <- W.peekInt16BE p1 endPtr
    (f2_correlationid, p3) <- W.peekInt32BE p2 endPtr
    (f3_clientid, p4) <- WP.peekKafkaString p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (RequestHeader { requestHeaderRequestApiKey = f0_requestapikey, requestHeaderRequestApiVersion = f1_requestapiversion, requestHeaderCorrelationId = f2_correlationid, requestHeaderClientId = f3_clientid }, pTagsEnd)
  | otherwise = error $ "wirePeek RequestHeader : unsupported version: " ++ show version
-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec RequestHeader where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeRequestHeader (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeRequestHeader (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekRequestHeader (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}
