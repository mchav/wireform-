{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.InitProducerIdRequest
Description : Kafka InitProducerIdRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 22.



Valid versions: 0-5
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

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for InitProducerIdRequest.
maxInitProducerIdRequestVersion :: Int16
maxInitProducerIdRequestVersion = 5

-- | KafkaMessage instance for InitProducerIdRequest.
instance KafkaMessage InitProducerIdRequest where
  messageApiKey = 22
  messageMinVersion = 0
  messageMaxVersion = 5
  messageFlexibleVersion = Just 2


-- | Worst-case wire size of a InitProducerIdRequest.
wireMaxSizeInitProducerIdRequest :: Int -> InitProducerIdRequest -> Int
wireMaxSizeInitProducerIdRequest _version msg =
  0
  + WP.dualStringMaxSize (initProducerIdRequestTransactionalId msg)
  + 4
  + 8
  + 2
  + 1

-- | Direct-poke encoder for InitProducerIdRequest.
wirePokeInitProducerIdRequest :: Int -> Ptr Word8 -> InitProducerIdRequest -> IO (Ptr Word8)
wirePokeInitProducerIdRequest version basePtr msg
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 2 then WP.pokeCompactString p0 (P.toCompactString (initProducerIdRequestTransactionalId msg)) else WP.pokeKafkaString p0 (initProducerIdRequestTransactionalId msg))
    p2 <- W.pokeInt32BE p1 (initProducerIdRequestTransactionTimeoutMs msg)
    WP.pokeEmptyTaggedFields p2
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
    pure (InitProducerIdRequest { initProducerIdRequestTransactionalId = f0_transactionalid, initProducerIdRequestTransactionTimeoutMs = f1_transactiontimeoutms, initProducerIdRequestProducerId = -1, initProducerIdRequestProducerEpoch = -1 }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_transactionalid, p1) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_transactiontimeoutms, p2) <- W.peekInt32BE p1 endPtr
    pure (InitProducerIdRequest { initProducerIdRequestTransactionalId = f0_transactionalid, initProducerIdRequestTransactionTimeoutMs = f1_transactiontimeoutms, initProducerIdRequestProducerId = -1, initProducerIdRequestProducerEpoch = -1 }, p2)
  | version >= 3 && version <= 5 = do
    (f0_transactionalid, p1) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_transactiontimeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_producerid, p3) <- (if version >= 3 then W.peekInt64BE p2 endPtr else pure (-1, p2))
    (f3_producerepoch, p4) <- (if version >= 3 then W.peekInt16BE p3 endPtr else pure (-1, p3))
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (InitProducerIdRequest { initProducerIdRequestTransactionalId = f0_transactionalid, initProducerIdRequestTransactionTimeoutMs = f1_transactiontimeoutms, initProducerIdRequestProducerId = f2_producerid, initProducerIdRequestProducerEpoch = f3_producerepoch }, pTagsEnd)
  | otherwise = error $ "wirePeek InitProducerIdRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec InitProducerIdRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeInitProducerIdRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeInitProducerIdRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekInitProducerIdRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}