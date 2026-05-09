{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.EndTxnRequest
Description : Kafka EndTxnRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 26.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.EndTxnRequest
  (
    EndTxnRequest(..),
    encodeEndTxnRequest,
    decodeEndTxnRequest,
    maxEndTxnRequestVersion
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
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP




data EndTxnRequest = EndTxnRequest
  {

  -- | The ID of the transaction to end.

  -- Versions: 0+
  endTxnRequestTransactionalId :: !(KafkaString)
,

  -- | The producer ID.

  -- Versions: 0+
  endTxnRequestProducerId :: !(Int64)
,

  -- | The current epoch associated with the producer.

  -- Versions: 0+
  endTxnRequestProducerEpoch :: !(Int16)
,

  -- | True if the transaction was committed, false if it was aborted.

  -- Versions: 0+
  endTxnRequestCommitted :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for EndTxnRequest.
maxEndTxnRequestVersion :: Int16
maxEndTxnRequestVersion = 5

-- | KafkaMessage instance for EndTxnRequest.
instance KafkaMessage EndTxnRequest where
  messageApiKey = 26
  messageMinVersion = 0
  messageMaxVersion = 5
  messageFlexibleVersion = Just 3

-- | Encode EndTxnRequest with the given API version.
encodeEndTxnRequest :: MonadPut m => E.ApiVersion -> EndTxnRequest -> m ()
encodeEndTxnRequest version msg
  | version >= 0 && version <= 2 =
    do
      serialize (endTxnRequestTransactionalId msg)
      serialize (endTxnRequestProducerId msg)
      serialize (endTxnRequestProducerEpoch msg)
      serialize (endTxnRequestCommitted msg)


  | version >= 3 && version <= 5 =
    do
      serialize (toCompactString (endTxnRequestTransactionalId msg))
      serialize (endTxnRequestProducerId msg)
      serialize (endTxnRequestProducerEpoch msg)
      serialize (endTxnRequestCommitted msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode EndTxnRequest with the given API version.
decodeEndTxnRequest :: MonadGet m => E.ApiVersion -> m EndTxnRequest
decodeEndTxnRequest version
  | version >= 0 && version <= 2 =
    do
      fieldtransactionalid <- deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      fieldcommitted <- deserialize
      pure EndTxnRequest
        {
        endTxnRequestTransactionalId = fieldtransactionalid
        ,
        endTxnRequestProducerId = fieldproducerid
        ,
        endTxnRequestProducerEpoch = fieldproducerepoch
        ,
        endTxnRequestCommitted = fieldcommitted
        }

  | version >= 3 && version <= 5 =
    do
      fieldtransactionalid <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      fieldcommitted <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure EndTxnRequest
        {
        endTxnRequestTransactionalId = fieldtransactionalid
        ,
        endTxnRequestProducerId = fieldproducerid
        ,
        endTxnRequestProducerEpoch = fieldproducerepoch
        ,
        endTxnRequestCommitted = fieldcommitted
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


-- | Worst-case wire size of a EndTxnRequest.
wireMaxSizeEndTxnRequest :: Int -> EndTxnRequest -> Int
wireMaxSizeEndTxnRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (endTxnRequestTransactionalId msg))
  + 8
  + 2
  + 1
  + 1

-- | Direct-poke encoder for EndTxnRequest.
wirePokeEndTxnRequest :: Int -> Ptr Word8 -> EndTxnRequest -> IO (Ptr Word8)
wirePokeEndTxnRequest version basePtr msg
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (endTxnRequestTransactionalId msg))
    p2 <- W.pokeInt64BE p1 (endTxnRequestProducerId msg)
    p3 <- W.pokeInt16BE p2 (endTxnRequestProducerEpoch msg)
    p4 <- W.pokeWord8 p3 (if (endTxnRequestCommitted msg) then 1 else 0)
    pure p4
  | version >= 3 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (endTxnRequestTransactionalId msg))
    p2 <- W.pokeInt64BE p1 (endTxnRequestProducerId msg)
    p3 <- W.pokeInt16BE p2 (endTxnRequestProducerEpoch msg)
    p4 <- W.pokeWord8 p3 (if (endTxnRequestCommitted msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke EndTxnRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for EndTxnRequest.
wirePeekEndTxnRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EndTxnRequest, Ptr Word8)
wirePeekEndTxnRequest version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 2 = do
    (f0_transactionalid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_producerid, p2) <- W.peekInt64BE p1 endPtr
    (f2_producerepoch, p3) <- W.peekInt16BE p2 endPtr
    (f3_committed, p4) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr
    pure (EndTxnRequest { endTxnRequestTransactionalId = f0_transactionalid, endTxnRequestProducerId = f1_producerid, endTxnRequestProducerEpoch = f2_producerepoch, endTxnRequestCommitted = f3_committed }, p4)
  | version >= 3 && version <= 5 = do
    (f0_transactionalid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_producerid, p2) <- W.peekInt64BE p1 endPtr
    (f2_producerepoch, p3) <- W.peekInt16BE p2 endPtr
    (f3_committed, p4) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (EndTxnRequest { endTxnRequestTransactionalId = f0_transactionalid, endTxnRequestProducerId = f1_producerid, endTxnRequestProducerEpoch = f2_producerepoch, endTxnRequestCommitted = f3_committed }, pTagsEnd)
  | otherwise = error $ "wirePeek EndTxnRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec EndTxnRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeEndTxnRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeEndTxnRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekEndTxnRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}