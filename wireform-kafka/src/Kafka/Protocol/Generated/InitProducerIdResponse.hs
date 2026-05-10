{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.InitProducerIdResponse
Description : Kafka InitProducerIdResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 22.



Valid versions: 0-6
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.InitProducerIdResponse
  (
    InitProducerIdResponse(..),
    maxInitProducerIdResponseVersion
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




data InitProducerIdResponse = InitProducerIdResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  initProducerIdResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  initProducerIdResponseErrorCode :: !(Int16)
,

  -- | The current producer id.

  -- Versions: 0+
  initProducerIdResponseProducerId :: !(Int64)
,

  -- | The current epoch associated with the producer id.

  -- Versions: 0+
  initProducerIdResponseProducerEpoch :: !(Int16)
,

  -- | The producer id for ongoing transaction when KeepPreparedTxn is used, -1 if there is no transaction 

  -- Versions: 6+
  initProducerIdResponseOngoingTxnProducerId :: !(Int64)
,

  -- | The epoch associated with the  producer id for ongoing transaction when KeepPreparedTxn is used, -1 

  -- Versions: 6+
  initProducerIdResponseOngoingTxnProducerEpoch :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for InitProducerIdResponse.
maxInitProducerIdResponseVersion :: Int16
maxInitProducerIdResponseVersion = 6

-- | KafkaMessage instance for InitProducerIdResponse.
instance KafkaMessage InitProducerIdResponse where
  messageApiKey = 22
  messageMinVersion = 0
  messageMaxVersion = 6
  messageFlexibleVersion = Just 2


-- | Worst-case wire size of a InitProducerIdResponse.
wireMaxSizeInitProducerIdResponse :: Int -> InitProducerIdResponse -> Int
wireMaxSizeInitProducerIdResponse _version msg =
  0
  + 4
  + 2
  + 8
  + 2
  + 8
  + 2
  + 1

-- | Direct-poke encoder for InitProducerIdResponse.
wirePokeInitProducerIdResponse :: Int -> Ptr Word8 -> InitProducerIdResponse -> IO (Ptr Word8)
wirePokeInitProducerIdResponse version basePtr msg
  | version == 6 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (initProducerIdResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (initProducerIdResponseErrorCode msg)
    p3 <- W.pokeInt64BE p2 (initProducerIdResponseProducerId msg)
    p4 <- W.pokeInt16BE p3 (initProducerIdResponseProducerEpoch msg)
    p5 <- (if version >= 6 then W.pokeInt64BE p4 (initProducerIdResponseOngoingTxnProducerId msg) else pure p4)
    p6 <- (if version >= 6 then W.pokeInt16BE p5 (initProducerIdResponseOngoingTxnProducerEpoch msg) else pure p5)
    WP.pokeEmptyTaggedFields p6
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (initProducerIdResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (initProducerIdResponseErrorCode msg)
    p3 <- W.pokeInt64BE p2 (initProducerIdResponseProducerId msg)
    p4 <- W.pokeInt16BE p3 (initProducerIdResponseProducerEpoch msg)
    pure p4
  | version >= 2 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (initProducerIdResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (initProducerIdResponseErrorCode msg)
    p3 <- W.pokeInt64BE p2 (initProducerIdResponseProducerId msg)
    p4 <- W.pokeInt16BE p3 (initProducerIdResponseProducerEpoch msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke InitProducerIdResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for InitProducerIdResponse.
wirePeekInitProducerIdResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (InitProducerIdResponse, Ptr Word8)
wirePeekInitProducerIdResponse version _fp _basePtr p0 endPtr
  | version == 6 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_producerid, p3) <- W.peekInt64BE p2 endPtr
    (f3_producerepoch, p4) <- W.peekInt16BE p3 endPtr
    (f4_ongoingtxnproducerid, p5) <- (if version >= 6 then W.peekInt64BE p4 endPtr else pure (0, p4))
    (f5_ongoingtxnproducerepoch, p6) <- (if version >= 6 then W.peekInt16BE p5 endPtr else pure (0, p5))
    pTagsEnd <- WP.peekAndSkipTaggedFields p6 endPtr
    pure (InitProducerIdResponse { initProducerIdResponseThrottleTimeMs = f0_throttletimems, initProducerIdResponseErrorCode = f1_errorcode, initProducerIdResponseProducerId = f2_producerid, initProducerIdResponseProducerEpoch = f3_producerepoch, initProducerIdResponseOngoingTxnProducerId = f4_ongoingtxnproducerid, initProducerIdResponseOngoingTxnProducerEpoch = f5_ongoingtxnproducerepoch }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_producerid, p3) <- W.peekInt64BE p2 endPtr
    (f3_producerepoch, p4) <- W.peekInt16BE p3 endPtr
    pure (InitProducerIdResponse { initProducerIdResponseThrottleTimeMs = f0_throttletimems, initProducerIdResponseErrorCode = f1_errorcode, initProducerIdResponseProducerId = f2_producerid, initProducerIdResponseProducerEpoch = f3_producerepoch, initProducerIdResponseOngoingTxnProducerId = 0, initProducerIdResponseOngoingTxnProducerEpoch = 0 }, p4)
  | version >= 2 && version <= 5 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_producerid, p3) <- W.peekInt64BE p2 endPtr
    (f3_producerepoch, p4) <- W.peekInt16BE p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (InitProducerIdResponse { initProducerIdResponseThrottleTimeMs = f0_throttletimems, initProducerIdResponseErrorCode = f1_errorcode, initProducerIdResponseProducerId = f2_producerid, initProducerIdResponseProducerEpoch = f3_producerepoch, initProducerIdResponseOngoingTxnProducerId = 0, initProducerIdResponseOngoingTxnProducerEpoch = 0 }, pTagsEnd)
  | otherwise = error $ "wirePeek InitProducerIdResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec InitProducerIdResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeInitProducerIdResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeInitProducerIdResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekInitProducerIdResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}