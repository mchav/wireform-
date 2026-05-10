{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AllocateProducerIdsResponse
Description : Kafka AllocateProducerIdsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 67.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AllocateProducerIdsResponse
  (
    AllocateProducerIdsResponse(..),
    maxAllocateProducerIdsResponseVersion
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




data AllocateProducerIdsResponse = AllocateProducerIdsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  allocateProducerIdsResponseThrottleTimeMs :: !(Int32)
,

  -- | The top level response error code.

  -- Versions: 0+
  allocateProducerIdsResponseErrorCode :: !(Int16)
,

  -- | The first producer ID in this range, inclusive.

  -- Versions: 0+
  allocateProducerIdsResponseProducerIdStart :: !(Int64)
,

  -- | The number of producer IDs in this range.

  -- Versions: 0+
  allocateProducerIdsResponseProducerIdLen :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AllocateProducerIdsResponse.
maxAllocateProducerIdsResponseVersion :: Int16
maxAllocateProducerIdsResponseVersion = 0

-- | KafkaMessage instance for AllocateProducerIdsResponse.
instance KafkaMessage AllocateProducerIdsResponse where
  messageApiKey = 67
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a AllocateProducerIdsResponse.
wireMaxSizeAllocateProducerIdsResponse :: Int -> AllocateProducerIdsResponse -> Int
wireMaxSizeAllocateProducerIdsResponse _version msg =
  0
  + 4
  + 2
  + 8
  + 4
  + 1

-- | Direct-poke encoder for AllocateProducerIdsResponse.
wirePokeAllocateProducerIdsResponse :: Int -> Ptr Word8 -> AllocateProducerIdsResponse -> IO (Ptr Word8)
wirePokeAllocateProducerIdsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (allocateProducerIdsResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (allocateProducerIdsResponseErrorCode msg)
    p3 <- W.pokeInt64BE p2 (allocateProducerIdsResponseProducerIdStart msg)
    p4 <- W.pokeInt32BE p3 (allocateProducerIdsResponseProducerIdLen msg)
    WP.pokeEmptyTaggedFields p4
  | otherwise = error $ "wirePoke AllocateProducerIdsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for AllocateProducerIdsResponse.
wirePeekAllocateProducerIdsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AllocateProducerIdsResponse, Ptr Word8)
wirePeekAllocateProducerIdsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_produceridstart, p3) <- W.peekInt64BE p2 endPtr
    (f3_produceridlen, p4) <- W.peekInt32BE p3 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (AllocateProducerIdsResponse { allocateProducerIdsResponseThrottleTimeMs = f0_throttletimems, allocateProducerIdsResponseErrorCode = f1_errorcode, allocateProducerIdsResponseProducerIdStart = f2_produceridstart, allocateProducerIdsResponseProducerIdLen = f3_produceridlen }, pTagsEnd)
  | otherwise = error $ "wirePeek AllocateProducerIdsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec AllocateProducerIdsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAllocateProducerIdsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAllocateProducerIdsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAllocateProducerIdsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}