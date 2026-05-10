{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.SyncGroupResponse
Description : Kafka SyncGroupResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 14.



Valid versions: 0-5
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.SyncGroupResponse
  (
    SyncGroupResponse(..),
    maxSyncGroupResponseVersion
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




data SyncGroupResponse = SyncGroupResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  syncGroupResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  syncGroupResponseErrorCode :: !(Int16)
,

  -- | The group protocol type.

  -- Versions: 5+
  syncGroupResponseProtocolType :: !(KafkaString)
,

  -- | The group protocol name.

  -- Versions: 5+
  syncGroupResponseProtocolName :: !(KafkaString)
,

  -- | The member assignment.

  -- Versions: 0+
  syncGroupResponseAssignment :: !(KafkaBytes)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for SyncGroupResponse.
maxSyncGroupResponseVersion :: Int16
maxSyncGroupResponseVersion = 5

-- | KafkaMessage instance for SyncGroupResponse.
instance KafkaMessage SyncGroupResponse where
  messageApiKey = 14
  messageMinVersion = 0
  messageMaxVersion = 5
  messageFlexibleVersion = Just 4


-- | Worst-case wire size of a SyncGroupResponse.
wireMaxSizeSyncGroupResponse :: Int -> SyncGroupResponse -> Int
wireMaxSizeSyncGroupResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (syncGroupResponseProtocolType msg))
  + WP.compactStringMaxSize (P.toCompactString (syncGroupResponseProtocolName msg))
  + WP.compactBytesMaxSize (P.toCompactBytes (syncGroupResponseAssignment msg))
  + 1

-- | Direct-poke encoder for SyncGroupResponse.
wirePokeSyncGroupResponse :: Int -> Ptr Word8 -> SyncGroupResponse -> IO (Ptr Word8)
wirePokeSyncGroupResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (syncGroupResponseErrorCode msg)
    p2 <- (if version >= 4 then WP.pokeCompactBytes p1 (P.toCompactBytes (syncGroupResponseAssignment msg)) else WP.pokeKafkaBytes p1 (syncGroupResponseAssignment msg))
    pure p2
  | version == 4 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (syncGroupResponseThrottleTimeMs msg) else pure p0)
    p2 <- W.pokeInt16BE p1 (syncGroupResponseErrorCode msg)
    p3 <- (if version >= 4 then WP.pokeCompactBytes p2 (P.toCompactBytes (syncGroupResponseAssignment msg)) else WP.pokeKafkaBytes p2 (syncGroupResponseAssignment msg))
    WP.pokeEmptyTaggedFields p3
  | version == 5 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (syncGroupResponseThrottleTimeMs msg) else pure p0)
    p2 <- W.pokeInt16BE p1 (syncGroupResponseErrorCode msg)
    p3 <- (if version >= 5 then (if version >= 4 then WP.pokeCompactString p2 (P.toCompactString (syncGroupResponseProtocolType msg)) else WP.pokeKafkaString p2 (syncGroupResponseProtocolType msg)) else pure p2)
    p4 <- (if version >= 5 then (if version >= 4 then WP.pokeCompactString p3 (P.toCompactString (syncGroupResponseProtocolName msg)) else WP.pokeKafkaString p3 (syncGroupResponseProtocolName msg)) else pure p3)
    p5 <- (if version >= 4 then WP.pokeCompactBytes p4 (P.toCompactBytes (syncGroupResponseAssignment msg)) else WP.pokeKafkaBytes p4 (syncGroupResponseAssignment msg))
    WP.pokeEmptyTaggedFields p5
  | version >= 1 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (syncGroupResponseThrottleTimeMs msg) else pure p0)
    p2 <- W.pokeInt16BE p1 (syncGroupResponseErrorCode msg)
    p3 <- (if version >= 4 then WP.pokeCompactBytes p2 (P.toCompactBytes (syncGroupResponseAssignment msg)) else WP.pokeKafkaBytes p2 (syncGroupResponseAssignment msg))
    pure p3
  | otherwise = error $ "wirePoke SyncGroupResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for SyncGroupResponse.
wirePeekSyncGroupResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SyncGroupResponse, Ptr Word8)
wirePeekSyncGroupResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_assignment, p2) <- (if version >= 4 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p1 endPtr else WP.peekKafkaBytes p1 endPtr)
    pure (SyncGroupResponse { syncGroupResponseThrottleTimeMs = 0, syncGroupResponseErrorCode = f0_errorcode, syncGroupResponseProtocolType = P.KafkaString Null, syncGroupResponseProtocolName = P.KafkaString Null, syncGroupResponseAssignment = f1_assignment }, p2)
  | version == 4 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_assignment, p3) <- (if version >= 4 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p2 endPtr else WP.peekKafkaBytes p2 endPtr)
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (SyncGroupResponse { syncGroupResponseThrottleTimeMs = f0_throttletimems, syncGroupResponseErrorCode = f1_errorcode, syncGroupResponseProtocolType = P.KafkaString Null, syncGroupResponseProtocolName = P.KafkaString Null, syncGroupResponseAssignment = f2_assignment }, pTagsEnd)
  | version == 5 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_protocoltype, p3) <- (if version >= 5 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr) else pure (P.KafkaString Null, p2))
    (f3_protocolname, p4) <- (if version >= 5 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
    (f4_assignment, p5) <- (if version >= 4 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p4 endPtr else WP.peekKafkaBytes p4 endPtr)
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (SyncGroupResponse { syncGroupResponseThrottleTimeMs = f0_throttletimems, syncGroupResponseErrorCode = f1_errorcode, syncGroupResponseProtocolType = f2_protocoltype, syncGroupResponseProtocolName = f3_protocolname, syncGroupResponseAssignment = f4_assignment }, pTagsEnd)
  | version >= 1 && version <= 3 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_assignment, p3) <- (if version >= 4 then (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p2 endPtr else WP.peekKafkaBytes p2 endPtr)
    pure (SyncGroupResponse { syncGroupResponseThrottleTimeMs = f0_throttletimems, syncGroupResponseErrorCode = f1_errorcode, syncGroupResponseProtocolType = P.KafkaString Null, syncGroupResponseProtocolName = P.KafkaString Null, syncGroupResponseAssignment = f2_assignment }, p3)
  | otherwise = error $ "wirePeek SyncGroupResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec SyncGroupResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeSyncGroupResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeSyncGroupResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekSyncGroupResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}