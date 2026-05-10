{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.InitProducerIdRequest
Description : Kafka InitProducerIdRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 22.



Valid versions: 0-6
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.InitProducerIdRequest
  (
    InitProducerIdRequest(..),
    maxInitProducerIdRequestVersion
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




data InitProducerIdRequest = InitProducerIdRequest
  {

  -- | The transactional id, or null if the producer is not transactional.

  -- Versions: 0+
  initProducerIdRequestTransactionalId :: !(KafkaString)
,

  -- | The time in ms to wait before aborting idle transactions sent by this producer. This is only relevan

  -- Versions: 0+
  initProducerIdRequestTransactionTimeoutMs :: !(Int32)
,

  -- | The producer id. This is used to disambiguate requests if a transactional id is reused following its

  -- Versions: 3+
  initProducerIdRequestProducerId :: !(Int64)
,

  -- | The producer's current epoch. This will be checked against the producer epoch on the broker, and the

  -- Versions: 3+
  initProducerIdRequestProducerEpoch :: !(Int16)
,

  -- | True if the client wants to enable two-phase commit (2PC) protocol for transactions.

  -- Versions: 6+
  initProducerIdRequestEnable2Pc :: !(Bool)
,

  -- | True if the client wants to keep the currently ongoing transaction instead of aborting it.

  -- Versions: 6+
  initProducerIdRequestKeepPreparedTxn :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for InitProducerIdRequest.
maxInitProducerIdRequestVersion :: Int16
maxInitProducerIdRequestVersion = 6

-- | KafkaMessage instance for InitProducerIdRequest.
instance KafkaMessage InitProducerIdRequest where
  messageApiKey = 22
  messageMinVersion = 0
  messageMaxVersion = 6
  messageFlexibleVersion = Just 2


-- | Worst-case wire size of a InitProducerIdRequest.
wireMaxSizeInitProducerIdRequest :: Int -> InitProducerIdRequest -> Int
wireMaxSizeInitProducerIdRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (initProducerIdRequestTransactionalId msg))
  + 4
  + 8
  + 2
  + 1
  + 1
  + 1

-- | Direct-poke encoder for InitProducerIdRequest.
wirePokeInitProducerIdRequest :: Int -> Ptr Word8 -> InitProducerIdRequest -> IO (Ptr Word8)
wirePokeInitProducerIdRequest version basePtr msg
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then WP.pokeCompactString p0 (P.toCompactString (initProducerIdRequestTransactionalId msg)) else WP.pokeKafkaString p0 (initProducerIdRequestTransactionalId msg))
    p2 <- W.pokeInt32BE p1 (initProducerIdRequestTransactionTimeoutMs msg)
    WP.pokeEmptyTaggedFields p2
  | version == 6 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then WP.pokeCompactString p0 (P.toCompactString (initProducerIdRequestTransactionalId msg)) else WP.pokeKafkaString p0 (initProducerIdRequestTransactionalId msg))
    p2 <- W.pokeInt32BE p1 (initProducerIdRequestTransactionTimeoutMs msg)
    p3 <- (if version >= 3 then W.pokeInt64BE p2 (initProducerIdRequestProducerId msg) else pure p2)
    p4 <- (if version >= 3 then W.pokeInt16BE p3 (initProducerIdRequestProducerEpoch msg) else pure p3)
    p5 <- (if version >= 6 then W.pokeWord8 p4 (if (initProducerIdRequestEnable2Pc msg) then 1 else 0) else pure p4)
    p6 <- (if version >= 6 then W.pokeWord8 p5 (if (initProducerIdRequestKeepPreparedTxn msg) then 1 else 0) else pure p5)
    WP.pokeEmptyTaggedFields p6
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then WP.pokeCompactString p0 (P.toCompactString (initProducerIdRequestTransactionalId msg)) else WP.pokeKafkaString p0 (initProducerIdRequestTransactionalId msg))
    p2 <- W.pokeInt32BE p1 (initProducerIdRequestTransactionTimeoutMs msg)
    pure p2
  | version >= 3 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then WP.pokeCompactString p0 (P.toCompactString (initProducerIdRequestTransactionalId msg)) else WP.pokeKafkaString p0 (initProducerIdRequestTransactionalId msg))
    p2 <- W.pokeInt32BE p1 (initProducerIdRequestTransactionTimeoutMs msg)
    p3 <- (if version >= 3 then W.pokeInt64BE p2 (initProducerIdRequestProducerId msg) else pure p2)
    p4 <- (if version >= 3 then W.pokeInt16BE p3 (initProducerIdRequestProducerEpoch msg) else pure p3)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke InitProducerIdRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for InitProducerIdRequest.
wirePeekInitProducerIdRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (InitProducerIdRequest, Ptr Word8)
wirePeekInitProducerIdRequest version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_transactionalid, p1) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_transactiontimeoutms, p2) <- W.peekInt32BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (InitProducerIdRequest { initProducerIdRequestTransactionalId = f0_transactionalid, initProducerIdRequestTransactionTimeoutMs = f1_transactiontimeoutms, initProducerIdRequestProducerId = 0, initProducerIdRequestProducerEpoch = 0, initProducerIdRequestEnable2Pc = False, initProducerIdRequestKeepPreparedTxn = False }, pTagsEnd)
  | version == 6 = do
    (f0_transactionalid, p1) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_transactiontimeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_producerid, p3) <- (if version >= 3 then W.peekInt64BE p2 endPtr else pure (0, p2))
    (f3_producerepoch, p4) <- (if version >= 3 then W.peekInt16BE p3 endPtr else pure (0, p3))
    (f4_enable2pc, p5) <- (if version >= 6 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p4 endPtr else pure (False, p4))
    (f5_keeppreparedtxn, p6) <- (if version >= 6 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p5 endPtr else pure (False, p5))
    pTagsEnd <- WP.peekAndSkipTaggedFields p6 endPtr
    pure (InitProducerIdRequest { initProducerIdRequestTransactionalId = f0_transactionalid, initProducerIdRequestTransactionTimeoutMs = f1_transactiontimeoutms, initProducerIdRequestProducerId = f2_producerid, initProducerIdRequestProducerEpoch = f3_producerepoch, initProducerIdRequestEnable2Pc = f4_enable2pc, initProducerIdRequestKeepPreparedTxn = f5_keeppreparedtxn }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_transactionalid, p1) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_transactiontimeoutms, p2) <- W.peekInt32BE p1 endPtr
    pure (InitProducerIdRequest { initProducerIdRequestTransactionalId = f0_transactionalid, initProducerIdRequestTransactionTimeoutMs = f1_transactiontimeoutms, initProducerIdRequestProducerId = 0, initProducerIdRequestProducerEpoch = 0, initProducerIdRequestEnable2Pc = False, initProducerIdRequestKeepPreparedTxn = False }, p2)
  | version >= 3 && version <= 5 = do
    (f0_transactionalid, p1) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_transactiontimeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_producerid, p3) <- (if version >= 3 then W.peekInt64BE p2 endPtr else pure (0, p2))
    (f3_producerepoch, p4) <- (if version >= 3 then W.peekInt16BE p3 endPtr else pure (0, p3))
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (InitProducerIdRequest { initProducerIdRequestTransactionalId = f0_transactionalid, initProducerIdRequestTransactionTimeoutMs = f1_transactiontimeoutms, initProducerIdRequestProducerId = f2_producerid, initProducerIdRequestProducerEpoch = f3_producerepoch, initProducerIdRequestEnable2Pc = False, initProducerIdRequestKeepPreparedTxn = False }, pTagsEnd)
  | otherwise = error $ "wirePeek InitProducerIdRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec InitProducerIdRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeInitProducerIdRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeInitProducerIdRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekInitProducerIdRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}