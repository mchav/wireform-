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
    maxAddOffsetsToTxnRequestVersion
  ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , Nullable(..)
  )
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
    p1 <- (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (addOffsetsToTxnRequestTransactionalId msg)) else WP.pokeKafkaString p0 (addOffsetsToTxnRequestTransactionalId msg))
    p2 <- W.pokeInt64BE p1 (addOffsetsToTxnRequestProducerId msg)
    p3 <- W.pokeInt16BE p2 (addOffsetsToTxnRequestProducerEpoch msg)
    p4 <- (if version >= 3 then WP.pokeCompactString p3 (P.toCompactString (addOffsetsToTxnRequestGroupId msg)) else WP.pokeKafkaString p3 (addOffsetsToTxnRequestGroupId msg))
    WP.pokeEmptyTaggedFields p4
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (addOffsetsToTxnRequestTransactionalId msg)) else WP.pokeKafkaString p0 (addOffsetsToTxnRequestTransactionalId msg))
    p2 <- W.pokeInt64BE p1 (addOffsetsToTxnRequestProducerId msg)
    p3 <- W.pokeInt16BE p2 (addOffsetsToTxnRequestProducerEpoch msg)
    p4 <- (if version >= 3 then WP.pokeCompactString p3 (P.toCompactString (addOffsetsToTxnRequestGroupId msg)) else WP.pokeKafkaString p3 (addOffsetsToTxnRequestGroupId msg))
    pure p4
  | otherwise = error $ "wirePoke AddOffsetsToTxnRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for AddOffsetsToTxnRequest.
wirePeekAddOffsetsToTxnRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AddOffsetsToTxnRequest, Ptr Word8)
wirePeekAddOffsetsToTxnRequest version _fp _basePtr p0 endPtr
  | version >= 3 && version <= 4 = do
    (f0_transactionalid, p1) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_producerid, p2) <- W.peekInt64BE p1 endPtr
    (f2_producerepoch, p3) <- W.peekInt16BE p2 endPtr
    (f3_groupid, p4) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (AddOffsetsToTxnRequest { addOffsetsToTxnRequestTransactionalId = f0_transactionalid, addOffsetsToTxnRequestProducerId = f1_producerid, addOffsetsToTxnRequestProducerEpoch = f2_producerepoch, addOffsetsToTxnRequestGroupId = f3_groupid }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    (f0_transactionalid, p1) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_producerid, p2) <- W.peekInt64BE p1 endPtr
    (f2_producerepoch, p3) <- W.peekInt16BE p2 endPtr
    (f3_groupid, p4) <- (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
    pure (AddOffsetsToTxnRequest { addOffsetsToTxnRequestTransactionalId = f0_transactionalid, addOffsetsToTxnRequestProducerId = f1_producerid, addOffsetsToTxnRequestProducerEpoch = f2_producerepoch, addOffsetsToTxnRequestGroupId = f3_groupid }, p4)
  | otherwise = error $ "wirePeek AddOffsetsToTxnRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AddOffsetsToTxnRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAddOffsetsToTxnRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAddOffsetsToTxnRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAddOffsetsToTxnRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}