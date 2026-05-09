{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AddOffsetsToTxnRequest
Description : Kafka AddOffsetsToTxnRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 25.



Valid versions: 0-4
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AddOffsetsToTxnRequest
  (
    AddOffsetsToTxnRequest(..),
    encodeAddOffsetsToTxnRequest,
    decodeAddOffsetsToTxnRequest,
    maxAddOffsetsToTxnRequestVersion
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




data AddOffsetsToTxnRequest = AddOffsetsToTxnRequest
  {

  -- | The transactional id corresponding to the transaction.

  -- Versions: 0+
  addOffsetsToTxnRequestTransactionalId :: !(KafkaString)
,

  -- | Current producer id in use by the transactional id.

  -- Versions: 0+
  addOffsetsToTxnRequestProducerId :: !(Int64)
,

  -- | Current epoch associated with the producer id.

  -- Versions: 0+
  addOffsetsToTxnRequestProducerEpoch :: !(Int16)
,

  -- | The unique group identifier.

  -- Versions: 0+
  addOffsetsToTxnRequestGroupId :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AddOffsetsToTxnRequest.
maxAddOffsetsToTxnRequestVersion :: Int16
maxAddOffsetsToTxnRequestVersion = 4

-- | KafkaMessage instance for AddOffsetsToTxnRequest.
instance KafkaMessage AddOffsetsToTxnRequest where
  messageApiKey = 25
  messageMinVersion = 0
  messageMaxVersion = 4
  messageFlexibleVersion = Just 3

-- | Encode AddOffsetsToTxnRequest with the given API version.
encodeAddOffsetsToTxnRequest :: MonadPut m => E.ApiVersion -> AddOffsetsToTxnRequest -> m ()
encodeAddOffsetsToTxnRequest version msg
  | version >= 3 && version <= 4 =
    do
      serialize (toCompactString (addOffsetsToTxnRequestTransactionalId msg))
      serialize (addOffsetsToTxnRequestProducerId msg)
      serialize (addOffsetsToTxnRequestProducerEpoch msg)
      serialize (toCompactString (addOffsetsToTxnRequestGroupId msg))
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 2 =
    do
      serialize (addOffsetsToTxnRequestTransactionalId msg)
      serialize (addOffsetsToTxnRequestProducerId msg)
      serialize (addOffsetsToTxnRequestProducerEpoch msg)
      serialize (addOffsetsToTxnRequestGroupId msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AddOffsetsToTxnRequest with the given API version.
decodeAddOffsetsToTxnRequest :: MonadGet m => E.ApiVersion -> m AddOffsetsToTxnRequest
decodeAddOffsetsToTxnRequest version
  | version >= 3 && version <= 4 =
    do
      fieldtransactionalid <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      fieldgroupid <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AddOffsetsToTxnRequest
        {
        addOffsetsToTxnRequestTransactionalId = fieldtransactionalid
        ,
        addOffsetsToTxnRequestProducerId = fieldproducerid
        ,
        addOffsetsToTxnRequestProducerEpoch = fieldproducerepoch
        ,
        addOffsetsToTxnRequestGroupId = fieldgroupid
        }

  | version >= 0 && version <= 2 =
    do
      fieldtransactionalid <- deserialize
      fieldproducerid <- deserialize
      fieldproducerepoch <- deserialize
      fieldgroupid <- deserialize
      pure AddOffsetsToTxnRequest
        {
        addOffsetsToTxnRequestTransactionalId = fieldtransactionalid
        ,
        addOffsetsToTxnRequestProducerId = fieldproducerid
        ,
        addOffsetsToTxnRequestProducerEpoch = fieldproducerepoch
        ,
        addOffsetsToTxnRequestGroupId = fieldgroupid
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


-- | Worst-case wire size of a AddOffsetsToTxnRequest.
wireMaxSizeAddOffsetsToTxnRequest :: Int -> AddOffsetsToTxnRequest -> Int
wireMaxSizeAddOffsetsToTxnRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (addOffsetsToTxnRequestTransactionalId msg))
  + 8
  + 2
  + WP.compactStringMaxSize (P.toCompactString (addOffsetsToTxnRequestGroupId msg))
  + 1

-- | Direct-poke encoder for AddOffsetsToTxnRequest.
wirePokeAddOffsetsToTxnRequest :: Int -> Ptr Word8 -> AddOffsetsToTxnRequest -> IO (Ptr Word8)
wirePokeAddOffsetsToTxnRequest version basePtr msg
  | version >= 3 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (addOffsetsToTxnRequestTransactionalId msg))
    p2 <- W.pokeInt64BE p1 (addOffsetsToTxnRequestProducerId msg)
    p3 <- W.pokeInt16BE p2 (addOffsetsToTxnRequestProducerEpoch msg)
    p4 <- WP.pokeCompactString p3 (P.toCompactString (addOffsetsToTxnRequestGroupId msg))
    WP.pokeEmptyTaggedFields p4
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (addOffsetsToTxnRequestTransactionalId msg))
    p2 <- W.pokeInt64BE p1 (addOffsetsToTxnRequestProducerId msg)
    p3 <- W.pokeInt16BE p2 (addOffsetsToTxnRequestProducerEpoch msg)
    p4 <- WP.pokeCompactString p3 (P.toCompactString (addOffsetsToTxnRequestGroupId msg))
    pure p4
  | otherwise = error $ "wirePoke AddOffsetsToTxnRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for AddOffsetsToTxnRequest.
wirePeekAddOffsetsToTxnRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AddOffsetsToTxnRequest, Ptr Word8)
wirePeekAddOffsetsToTxnRequest version _fp _basePtr p0 endPtr
  | version >= 3 && version <= 4 = do
    (f0_transactionalid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_producerid, p2) <- W.peekInt64BE p1 endPtr
    (f2_producerepoch, p3) <- W.peekInt16BE p2 endPtr
    (f3_groupid, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (AddOffsetsToTxnRequest { addOffsetsToTxnRequestTransactionalId = f0_transactionalid, addOffsetsToTxnRequestProducerId = f1_producerid, addOffsetsToTxnRequestProducerEpoch = f2_producerepoch, addOffsetsToTxnRequestGroupId = f3_groupid }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    (f0_transactionalid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_producerid, p2) <- W.peekInt64BE p1 endPtr
    (f2_producerepoch, p3) <- W.peekInt16BE p2 endPtr
    (f3_groupid, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
    pure (AddOffsetsToTxnRequest { addOffsetsToTxnRequestTransactionalId = f0_transactionalid, addOffsetsToTxnRequestProducerId = f1_producerid, addOffsetsToTxnRequestProducerEpoch = f2_producerepoch, addOffsetsToTxnRequestGroupId = f3_groupid }, p4)
  | otherwise = error $ "wirePeek AddOffsetsToTxnRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec AddOffsetsToTxnRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAddOffsetsToTxnRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAddOffsetsToTxnRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAddOffsetsToTxnRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}