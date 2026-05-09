{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AllocateProducerIdsRequest
Description : Kafka AllocateProducerIdsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 67.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AllocateProducerIdsRequest
  (
    AllocateProducerIdsRequest(..),
    encodeAllocateProducerIdsRequest,
    decodeAllocateProducerIdsRequest,
    maxAllocateProducerIdsRequestVersion
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




data AllocateProducerIdsRequest = AllocateProducerIdsRequest
  {

  -- | The ID of the requesting broker.

  -- Versions: 0+
  allocateProducerIdsRequestBrokerId :: !(Int32)
,

  -- | The epoch of the requesting broker.

  -- Versions: 0+
  allocateProducerIdsRequestBrokerEpoch :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AllocateProducerIdsRequest.
maxAllocateProducerIdsRequestVersion :: Int16
maxAllocateProducerIdsRequestVersion = 0

-- | KafkaMessage instance for AllocateProducerIdsRequest.
instance KafkaMessage AllocateProducerIdsRequest where
  messageApiKey = 67
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode AllocateProducerIdsRequest with the given API version.
encodeAllocateProducerIdsRequest :: MonadPut m => E.ApiVersion -> AllocateProducerIdsRequest -> m ()
encodeAllocateProducerIdsRequest version msg
  | version == 0 =
    do
      serialize (allocateProducerIdsRequestBrokerId msg)
      serialize (allocateProducerIdsRequestBrokerEpoch msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AllocateProducerIdsRequest with the given API version.
decodeAllocateProducerIdsRequest :: MonadGet m => E.ApiVersion -> m AllocateProducerIdsRequest
decodeAllocateProducerIdsRequest version
  | version == 0 =
    do
      fieldbrokerid <- deserialize
      fieldbrokerepoch <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AllocateProducerIdsRequest
        {
        allocateProducerIdsRequestBrokerId = fieldbrokerid
        ,
        allocateProducerIdsRequestBrokerEpoch = fieldbrokerepoch
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


-- | Worst-case wire size of a AllocateProducerIdsRequest.
wireMaxSizeAllocateProducerIdsRequest :: Int -> AllocateProducerIdsRequest -> Int
wireMaxSizeAllocateProducerIdsRequest _version msg =
  0
  + 4
  + 8
  + 1

-- | Direct-poke encoder for AllocateProducerIdsRequest.
wirePokeAllocateProducerIdsRequest :: Int -> Ptr Word8 -> AllocateProducerIdsRequest -> IO (Ptr Word8)
wirePokeAllocateProducerIdsRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (allocateProducerIdsRequestBrokerId msg)
    p2 <- W.pokeInt64BE p1 (allocateProducerIdsRequestBrokerEpoch msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke AllocateProducerIdsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for AllocateProducerIdsRequest.
wirePeekAllocateProducerIdsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AllocateProducerIdsRequest, Ptr Word8)
wirePeekAllocateProducerIdsRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_brokerid, p1) <- W.peekInt32BE p0 endPtr
    (f1_brokerepoch, p2) <- W.peekInt64BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (AllocateProducerIdsRequest { allocateProducerIdsRequestBrokerId = f0_brokerid, allocateProducerIdsRequestBrokerEpoch = f1_brokerepoch }, pTagsEnd)
  | otherwise = error $ "wirePeek AllocateProducerIdsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec AllocateProducerIdsRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAllocateProducerIdsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAllocateProducerIdsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAllocateProducerIdsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}