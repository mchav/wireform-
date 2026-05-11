{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.HeartbeatRequest
Description : Kafka HeartbeatRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 12.



Valid versions: 0-4
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.HeartbeatRequest
  (
    HeartbeatRequest(..),
    maxHeartbeatRequestVersion
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




data HeartbeatRequest = HeartbeatRequest
  {

  -- | The group id.

  -- Versions: 0+
  heartbeatRequestGroupId :: !(KafkaString)
,

  -- | The generation of the group.

  -- Versions: 0+
  heartbeatRequestGenerationId :: !(Int32)
,

  -- | The member ID.

  -- Versions: 0+
  heartbeatRequestMemberId :: !(KafkaString)
,

  -- | The unique identifier of the consumer instance provided by end user.

  -- Versions: 3+
  heartbeatRequestGroupInstanceId :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for HeartbeatRequest.
maxHeartbeatRequestVersion :: Int16
maxHeartbeatRequestVersion = 4

-- | KafkaMessage instance for HeartbeatRequest.
instance KafkaMessage HeartbeatRequest where
  messageApiKey = 12
  messageMinVersion = 0
  messageMaxVersion = 4
  messageFlexibleVersion = Just 4


-- | Worst-case wire size of a HeartbeatRequest.
wireMaxSizeHeartbeatRequest :: Int -> HeartbeatRequest -> Int
wireMaxSizeHeartbeatRequest _version msg =
  0
  + WP.dualStringMaxSize (heartbeatRequestGroupId msg)
  + 4
  + WP.dualStringMaxSize (heartbeatRequestMemberId msg)
  + WP.dualStringMaxSize (heartbeatRequestGroupInstanceId msg)
  + 1

-- | Direct-poke encoder for HeartbeatRequest.
wirePokeHeartbeatRequest :: Int -> Ptr Word8 -> HeartbeatRequest -> IO (Ptr Word8)
wirePokeHeartbeatRequest version basePtr msg
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- (if version >= 4 then WP.pokeCompactString p0 (P.toCompactString (heartbeatRequestGroupId msg)) else WP.pokeKafkaString p0 (heartbeatRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (heartbeatRequestGenerationId msg)
    p3 <- (if version >= 4 then WP.pokeCompactString p2 (P.toCompactString (heartbeatRequestMemberId msg)) else WP.pokeKafkaString p2 (heartbeatRequestMemberId msg))
    p4 <- (if version >= 3 then (if version >= 4 then WP.pokeCompactString p3 (P.toCompactString (heartbeatRequestGroupInstanceId msg)) else WP.pokeKafkaString p3 (heartbeatRequestGroupInstanceId msg)) else pure p3)
    pure p4
  | version == 4 = do
    p0 <- pure basePtr
    p1 <- (if version >= 4 then WP.pokeCompactString p0 (P.toCompactString (heartbeatRequestGroupId msg)) else WP.pokeKafkaString p0 (heartbeatRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (heartbeatRequestGenerationId msg)
    p3 <- (if version >= 4 then WP.pokeCompactString p2 (P.toCompactString (heartbeatRequestMemberId msg)) else WP.pokeKafkaString p2 (heartbeatRequestMemberId msg))
    p4 <- (if version >= 3 then (if version >= 4 then WP.pokeCompactString p3 (P.toCompactString (heartbeatRequestGroupInstanceId msg)) else WP.pokeKafkaString p3 (heartbeatRequestGroupInstanceId msg)) else pure p3)
    WP.pokeEmptyTaggedFields p4
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 4 then WP.pokeCompactString p0 (P.toCompactString (heartbeatRequestGroupId msg)) else WP.pokeKafkaString p0 (heartbeatRequestGroupId msg))
    p2 <- W.pokeInt32BE p1 (heartbeatRequestGenerationId msg)
    p3 <- (if version >= 4 then WP.pokeCompactString p2 (P.toCompactString (heartbeatRequestMemberId msg)) else WP.pokeKafkaString p2 (heartbeatRequestMemberId msg))
    pure p3
  | otherwise = error $ "wirePoke HeartbeatRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for HeartbeatRequest.
wirePeekHeartbeatRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (HeartbeatRequest, Ptr Word8)
wirePeekHeartbeatRequest version _fp _basePtr p0 endPtr
  | version == 3 = do
    (f0_groupid, p1) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_generationid, p2) <- W.peekInt32BE p1 endPtr
    (f2_memberid, p3) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_groupinstanceid, p4) <- (if version >= 3 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
    pure (HeartbeatRequest { heartbeatRequestGroupId = f0_groupid, heartbeatRequestGenerationId = f1_generationid, heartbeatRequestMemberId = f2_memberid, heartbeatRequestGroupInstanceId = f3_groupinstanceid }, p4)
  | version == 4 = do
    (f0_groupid, p1) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_generationid, p2) <- W.peekInt32BE p1 endPtr
    (f2_memberid, p3) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_groupinstanceid, p4) <- (if version >= 3 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
    pTagsEnd <- WP.peekAndSkipTaggedFields p4 endPtr
    pure (HeartbeatRequest { heartbeatRequestGroupId = f0_groupid, heartbeatRequestGenerationId = f1_generationid, heartbeatRequestMemberId = f2_memberid, heartbeatRequestGroupInstanceId = f3_groupinstanceid }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    (f0_groupid, p1) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
    (f1_generationid, p2) <- W.peekInt32BE p1 endPtr
    (f2_memberid, p3) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    pure (HeartbeatRequest { heartbeatRequestGroupId = f0_groupid, heartbeatRequestGenerationId = f1_generationid, heartbeatRequestMemberId = f2_memberid, heartbeatRequestGroupInstanceId = P.KafkaString Null }, p3)
  | otherwise = error $ "wirePeek HeartbeatRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec HeartbeatRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeHeartbeatRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeHeartbeatRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekHeartbeatRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}