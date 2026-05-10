{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.FindCoordinatorResponse
Description : Kafka FindCoordinatorResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 10.



Valid versions: 0-6
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.FindCoordinatorResponse
  (
    FindCoordinatorResponse(..),
    Coordinator(..),
    maxFindCoordinatorResponseVersion
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


-- | Each coordinator result in the response.
data Coordinator = Coordinator
  {

  -- | The coordinator key.

  -- Versions: 4+
  coordinatorKey :: !(KafkaString)
,

  -- | The node id.

  -- Versions: 4+
  coordinatorNodeId :: !(Int32)
,

  -- | The host name.

  -- Versions: 4+
  coordinatorHost :: !(KafkaString)
,

  -- | The port.

  -- Versions: 4+
  coordinatorPort :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 4+
  coordinatorErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 4+
  coordinatorErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


data FindCoordinatorResponse = FindCoordinatorResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  findCoordinatorResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0-3
  findCoordinatorResponseErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 1-3
  findCoordinatorResponseErrorMessage :: !(KafkaString)
,

  -- | The node id.

  -- Versions: 0-3
  findCoordinatorResponseNodeId :: !(Int32)
,

  -- | The host name.

  -- Versions: 0-3
  findCoordinatorResponseHost :: !(KafkaString)
,

  -- | The port.

  -- Versions: 0-3
  findCoordinatorResponsePort :: !(Int32)
,

  -- | Each coordinator result in the response.

  -- Versions: 4+
  findCoordinatorResponseCoordinators :: !(KafkaArray (Coordinator))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for FindCoordinatorResponse.
maxFindCoordinatorResponseVersion :: Int16
maxFindCoordinatorResponseVersion = 6

-- | KafkaMessage instance for FindCoordinatorResponse.
instance KafkaMessage FindCoordinatorResponse where
  messageApiKey = 10
  messageMinVersion = 0
  messageMaxVersion = 6
  messageFlexibleVersion = Just 3

-- | Worst-case wire size of a Coordinator.
wireMaxSizeCoordinator :: Int -> Coordinator -> Int
wireMaxSizeCoordinator _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (coordinatorKey msg))
  + 4
  + WP.compactStringMaxSize (P.toCompactString (coordinatorHost msg))
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (coordinatorErrorMessage msg))
  + 1

-- | Direct-poke encoder for Coordinator.
wirePokeCoordinator :: Int -> Ptr Word8 -> Coordinator -> IO (Ptr Word8)
wirePokeCoordinator version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 4 then (if version >= 3 then WP.pokeCompactString p0 (P.toCompactString (coordinatorKey msg)) else WP.pokeKafkaString p0 (coordinatorKey msg)) else pure p0)
  p2 <- (if version >= 4 then W.pokeInt32BE p1 (coordinatorNodeId msg) else pure p1)
  p3 <- (if version >= 4 then (if version >= 3 then WP.pokeCompactString p2 (P.toCompactString (coordinatorHost msg)) else WP.pokeKafkaString p2 (coordinatorHost msg)) else pure p2)
  p4 <- (if version >= 4 then W.pokeInt32BE p3 (coordinatorPort msg) else pure p3)
  p5 <- (if version >= 4 then W.pokeInt16BE p4 (coordinatorErrorCode msg) else pure p4)
  p6 <- (if version >= 4 then (if version >= 3 then WP.pokeCompactString p5 (P.toCompactString (coordinatorErrorMessage msg)) else WP.pokeKafkaString p5 (coordinatorErrorMessage msg)) else pure p5)
  if version >= 3 then WP.pokeEmptyTaggedFields p6 else pure p6

-- | Direct-poke decoder for Coordinator.
wirePeekCoordinator :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Coordinator, Ptr Word8)
wirePeekCoordinator version _fp _basePtr p0 endPtr = do
  (f0_key, p1) <- (if version >= 4 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr) else pure (P.KafkaString Null, p0))
  (f1_nodeid, p2) <- (if version >= 4 then W.peekInt32BE p1 endPtr else pure (0, p1))
  (f2_host, p3) <- (if version >= 4 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr) else pure (P.KafkaString Null, p2))
  (f3_port, p4) <- (if version >= 4 then W.peekInt32BE p3 endPtr else pure (0, p3))
  (f4_errorcode, p5) <- (if version >= 4 then W.peekInt16BE p4 endPtr else pure (0, p4))
  (f5_errormessage, p6) <- (if version >= 4 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr else WP.peekKafkaString p5 endPtr) else pure (P.KafkaString Null, p5))
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p6 endPtr else pure p6
  pure (Coordinator { coordinatorKey = f0_key, coordinatorNodeId = f1_nodeid, coordinatorHost = f2_host, coordinatorPort = f3_port, coordinatorErrorCode = f4_errorcode, coordinatorErrorMessage = f5_errormessage }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultCoordinator :: Coordinator
defaultCoordinator = Coordinator { coordinatorKey = P.KafkaString Null, coordinatorNodeId = 0, coordinatorHost = P.KafkaString Null, coordinatorPort = 0, coordinatorErrorCode = 0, coordinatorErrorMessage = P.KafkaString Null }

-- | Worst-case wire size of a FindCoordinatorResponse.
wireMaxSizeFindCoordinatorResponse :: Int -> FindCoordinatorResponse -> Int
wireMaxSizeFindCoordinatorResponse _version msg =
  0
  + 4
  + 2
  + WP.compactStringMaxSize (P.toCompactString (findCoordinatorResponseErrorMessage msg))
  + 4
  + WP.compactStringMaxSize (P.toCompactString (findCoordinatorResponseHost msg))
  + 4
  + (5 + (case P.unKafkaArray (findCoordinatorResponseCoordinators msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeCoordinator _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for FindCoordinatorResponse.
wirePokeFindCoordinatorResponse :: Int -> Ptr Word8 -> FindCoordinatorResponse -> IO (Ptr Word8)
wirePokeFindCoordinatorResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- (if version <= 3 then W.pokeInt16BE p0 (findCoordinatorResponseErrorCode msg) else pure p0)
    p2 <- (if version <= 3 then W.pokeInt32BE p1 (findCoordinatorResponseNodeId msg) else pure p1)
    p3 <- (if version <= 3 then (if version >= 3 then WP.pokeCompactString p2 (P.toCompactString (findCoordinatorResponseHost msg)) else WP.pokeKafkaString p2 (findCoordinatorResponseHost msg)) else pure p2)
    p4 <- (if version <= 3 then W.pokeInt32BE p3 (findCoordinatorResponsePort msg) else pure p3)
    pure p4
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (findCoordinatorResponseThrottleTimeMs msg) else pure p0)
    p2 <- (if version <= 3 then W.pokeInt16BE p1 (findCoordinatorResponseErrorCode msg) else pure p1)
    p3 <- (if version >= 1 && version <= 3 then (if version >= 3 then WP.pokeCompactString p2 (P.toCompactString (findCoordinatorResponseErrorMessage msg)) else WP.pokeKafkaString p2 (findCoordinatorResponseErrorMessage msg)) else pure p2)
    p4 <- (if version <= 3 then W.pokeInt32BE p3 (findCoordinatorResponseNodeId msg) else pure p3)
    p5 <- (if version <= 3 then (if version >= 3 then WP.pokeCompactString p4 (P.toCompactString (findCoordinatorResponseHost msg)) else WP.pokeKafkaString p4 (findCoordinatorResponseHost msg)) else pure p4)
    p6 <- (if version <= 3 then W.pokeInt32BE p5 (findCoordinatorResponsePort msg) else pure p5)
    WP.pokeEmptyTaggedFields p6
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (findCoordinatorResponseThrottleTimeMs msg) else pure p0)
    p2 <- (if version <= 3 then W.pokeInt16BE p1 (findCoordinatorResponseErrorCode msg) else pure p1)
    p3 <- (if version >= 1 && version <= 3 then (if version >= 3 then WP.pokeCompactString p2 (P.toCompactString (findCoordinatorResponseErrorMessage msg)) else WP.pokeKafkaString p2 (findCoordinatorResponseErrorMessage msg)) else pure p2)
    p4 <- (if version <= 3 then W.pokeInt32BE p3 (findCoordinatorResponseNodeId msg) else pure p3)
    p5 <- (if version <= 3 then (if version >= 3 then WP.pokeCompactString p4 (P.toCompactString (findCoordinatorResponseHost msg)) else WP.pokeKafkaString p4 (findCoordinatorResponseHost msg)) else pure p4)
    p6 <- (if version <= 3 then W.pokeInt32BE p5 (findCoordinatorResponsePort msg) else pure p5)
    pure p6
  | version >= 4 && version <= 6 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (findCoordinatorResponseThrottleTimeMs msg) else pure p0)
    p2 <- (if version >= 4 then WP.pokeVersionedArray version 3 (\p x -> wirePokeCoordinator version p x) p1 (findCoordinatorResponseCoordinators msg) else pure p1)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke FindCoordinatorResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for FindCoordinatorResponse.
wirePeekFindCoordinatorResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FindCoordinatorResponse, Ptr Word8)
wirePeekFindCoordinatorResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_errorcode, p1) <- (if version <= 3 then W.peekInt16BE p0 endPtr else pure (0, p0))
    (f1_nodeid, p2) <- (if version <= 3 then W.peekInt32BE p1 endPtr else pure (0, p1))
    (f2_host, p3) <- (if version <= 3 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr) else pure (P.KafkaString Null, p2))
    (f3_port, p4) <- (if version <= 3 then W.peekInt32BE p3 endPtr else pure (0, p3))
    pure (FindCoordinatorResponse { findCoordinatorResponseThrottleTimeMs = 0, findCoordinatorResponseErrorCode = f0_errorcode, findCoordinatorResponseErrorMessage = P.KafkaString Null, findCoordinatorResponseNodeId = f1_nodeid, findCoordinatorResponseHost = f2_host, findCoordinatorResponsePort = f3_port, findCoordinatorResponseCoordinators = P.mkKafkaArray V.empty }, p4)
  | version == 3 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- (if version <= 3 then W.peekInt16BE p1 endPtr else pure (0, p1))
    (f2_errormessage, p3) <- (if version >= 1 && version <= 3 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr) else pure (P.KafkaString Null, p2))
    (f3_nodeid, p4) <- (if version <= 3 then W.peekInt32BE p3 endPtr else pure (0, p3))
    (f4_host, p5) <- (if version <= 3 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr) else pure (P.KafkaString Null, p4))
    (f5_port, p6) <- (if version <= 3 then W.peekInt32BE p5 endPtr else pure (0, p5))
    pTagsEnd <- WP.peekAndSkipTaggedFields p6 endPtr
    pure (FindCoordinatorResponse { findCoordinatorResponseThrottleTimeMs = f0_throttletimems, findCoordinatorResponseErrorCode = f1_errorcode, findCoordinatorResponseErrorMessage = f2_errormessage, findCoordinatorResponseNodeId = f3_nodeid, findCoordinatorResponseHost = f4_host, findCoordinatorResponsePort = f5_port, findCoordinatorResponseCoordinators = P.mkKafkaArray V.empty }, pTagsEnd)
  | version >= 1 && version <= 2 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_errorcode, p2) <- (if version <= 3 then W.peekInt16BE p1 endPtr else pure (0, p1))
    (f2_errormessage, p3) <- (if version >= 1 && version <= 3 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr) else pure (P.KafkaString Null, p2))
    (f3_nodeid, p4) <- (if version <= 3 then W.peekInt32BE p3 endPtr else pure (0, p3))
    (f4_host, p5) <- (if version <= 3 then (if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr) else pure (P.KafkaString Null, p4))
    (f5_port, p6) <- (if version <= 3 then W.peekInt32BE p5 endPtr else pure (0, p5))
    pure (FindCoordinatorResponse { findCoordinatorResponseThrottleTimeMs = f0_throttletimems, findCoordinatorResponseErrorCode = f1_errorcode, findCoordinatorResponseErrorMessage = f2_errormessage, findCoordinatorResponseNodeId = f3_nodeid, findCoordinatorResponseHost = f4_host, findCoordinatorResponsePort = f5_port, findCoordinatorResponseCoordinators = P.mkKafkaArray V.empty }, p6)
  | version >= 4 && version <= 6 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_coordinators, p2) <- (if version >= 4 then WP.peekVersionedArray version 3 (\p e -> wirePeekCoordinator version _fp _basePtr p e) p1 endPtr else pure (P.mkKafkaArray V.empty, p1))
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (FindCoordinatorResponse { findCoordinatorResponseThrottleTimeMs = f0_throttletimems, findCoordinatorResponseErrorCode = 0, findCoordinatorResponseErrorMessage = P.KafkaString Null, findCoordinatorResponseNodeId = 0, findCoordinatorResponseHost = P.KafkaString Null, findCoordinatorResponsePort = 0, findCoordinatorResponseCoordinators = f1_coordinators }, pTagsEnd)
  | otherwise = error $ "wirePeek FindCoordinatorResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec FindCoordinatorResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeFindCoordinatorResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeFindCoordinatorResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekFindCoordinatorResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}