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
    encodeFindCoordinatorResponse,
    decodeFindCoordinatorResponse,
    maxFindCoordinatorResponseVersion
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


-- | Encode Coordinator with version-aware field handling.
encodeCoordinator :: MonadPut m => E.ApiVersion -> Coordinator -> m ()
encodeCoordinator version cmsg =
  do
    when (version >= 4) $
      if version >= 3 then serialize (toCompactString (coordinatorKey cmsg)) else serialize (coordinatorKey cmsg)
    when (version >= 4) $
      serialize (coordinatorNodeId cmsg)
    when (version >= 4) $
      if version >= 3 then serialize (toCompactString (coordinatorHost cmsg)) else serialize (coordinatorHost cmsg)
    when (version >= 4) $
      serialize (coordinatorPort cmsg)
    when (version >= 4) $
      serialize (coordinatorErrorCode cmsg)
    when (version >= 4) $
      if version >= 3 then serialize (toCompactString (coordinatorErrorMessage cmsg)) else serialize (coordinatorErrorMessage cmsg)
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Coordinator with version-aware field handling.
decodeCoordinator :: MonadGet m => E.ApiVersion -> m Coordinator
decodeCoordinator version =
  do
    fieldkey <- if version >= 4
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldnodeid <- if version >= 4
      then deserialize
      else pure (0)
    fieldhost <- if version >= 4
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldport <- if version >= 4
      then deserialize
      else pure (0)
    fielderrorcode <- if version >= 4
      then deserialize
      else pure (0)
    fielderrormessage <- if version >= 4
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Coordinator
      {
      coordinatorKey = fieldkey
      ,
      coordinatorNodeId = fieldnodeid
      ,
      coordinatorHost = fieldhost
      ,
      coordinatorPort = fieldport
      ,
      coordinatorErrorCode = fielderrorcode
      ,
      coordinatorErrorMessage = fielderrormessage
      }



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

-- | Encode FindCoordinatorResponse with the given API version.
encodeFindCoordinatorResponse :: MonadPut m => E.ApiVersion -> FindCoordinatorResponse -> m ()
encodeFindCoordinatorResponse version msg
  | version == 0 =
    do
      serialize (findCoordinatorResponseErrorCode msg)
      serialize (findCoordinatorResponseNodeId msg)
      serialize (findCoordinatorResponseHost msg)
      serialize (findCoordinatorResponsePort msg)


  | version == 3 =
    do
      serialize (findCoordinatorResponseThrottleTimeMs msg)
      serialize (findCoordinatorResponseErrorCode msg)
      serialize (toCompactString (findCoordinatorResponseErrorMessage msg))
      serialize (findCoordinatorResponseNodeId msg)
      serialize (toCompactString (findCoordinatorResponseHost msg))
      serialize (findCoordinatorResponsePort msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 2 =
    do
      serialize (findCoordinatorResponseThrottleTimeMs msg)
      serialize (findCoordinatorResponseErrorCode msg)
      serialize (findCoordinatorResponseErrorMessage msg)
      serialize (findCoordinatorResponseNodeId msg)
      serialize (findCoordinatorResponseHost msg)
      serialize (findCoordinatorResponsePort msg)


  | version >= 4 && version <= 6 =
    do
      serialize (findCoordinatorResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 3 encodeCoordinator (case P.unKafkaArray (findCoordinatorResponseCoordinators msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode FindCoordinatorResponse with the given API version.
decodeFindCoordinatorResponse :: MonadGet m => E.ApiVersion -> m FindCoordinatorResponse
decodeFindCoordinatorResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      fieldnodeid <- deserialize
      fieldhost <- deserialize
      fieldport <- deserialize
      pure FindCoordinatorResponse
        {
        findCoordinatorResponseThrottleTimeMs = 0
        ,
        findCoordinatorResponseErrorCode = fielderrorcode
        ,
        findCoordinatorResponseErrorMessage = P.KafkaString Null
        ,
        findCoordinatorResponseNodeId = fieldnodeid
        ,
        findCoordinatorResponseHost = fieldhost
        ,
        findCoordinatorResponsePort = fieldport
        ,
        findCoordinatorResponseCoordinators = P.mkKafkaArray V.empty
        }

  | version == 3 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldnodeid <- deserialize
      fieldhost <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      fieldport <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure FindCoordinatorResponse
        {
        findCoordinatorResponseThrottleTimeMs = fieldthrottletimems
        ,
        findCoordinatorResponseErrorCode = fielderrorcode
        ,
        findCoordinatorResponseErrorMessage = fielderrormessage
        ,
        findCoordinatorResponseNodeId = fieldnodeid
        ,
        findCoordinatorResponseHost = fieldhost
        ,
        findCoordinatorResponsePort = fieldport
        ,
        findCoordinatorResponseCoordinators = P.mkKafkaArray V.empty
        }

  | version >= 1 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fielderrormessage <- deserialize
      fieldnodeid <- deserialize
      fieldhost <- deserialize
      fieldport <- deserialize
      pure FindCoordinatorResponse
        {
        findCoordinatorResponseThrottleTimeMs = fieldthrottletimems
        ,
        findCoordinatorResponseErrorCode = fielderrorcode
        ,
        findCoordinatorResponseErrorMessage = fielderrormessage
        ,
        findCoordinatorResponseNodeId = fieldnodeid
        ,
        findCoordinatorResponseHost = fieldhost
        ,
        findCoordinatorResponsePort = fieldport
        ,
        findCoordinatorResponseCoordinators = P.mkKafkaArray V.empty
        }

  | version >= 4 && version <= 6 =
    do
      fieldthrottletimems <- deserialize
      fieldcoordinators <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeCoordinator
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure FindCoordinatorResponse
        {
        findCoordinatorResponseThrottleTimeMs = fieldthrottletimems
        ,
        findCoordinatorResponseErrorCode = 0
        ,
        findCoordinatorResponseErrorMessage = P.KafkaString Null
        ,
        findCoordinatorResponseNodeId = 0
        ,
        findCoordinatorResponseHost = P.KafkaString Null
        ,
        findCoordinatorResponsePort = 0
        ,
        findCoordinatorResponseCoordinators = fieldcoordinators
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

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
  p1 <- WP.pokeCompactString p0 (P.toCompactString (coordinatorKey msg))
  p2 <- W.pokeInt32BE p1 (coordinatorNodeId msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (coordinatorHost msg))
  p4 <- W.pokeInt32BE p3 (coordinatorPort msg)
  p5 <- W.pokeInt16BE p4 (coordinatorErrorCode msg)
  p6 <- WP.pokeCompactString p5 (P.toCompactString (coordinatorErrorMessage msg))
  if version >= 3 then WP.pokeEmptyTaggedFields p6 else pure p6

-- | Direct-poke decoder for Coordinator.
wirePeekCoordinator :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Coordinator, Ptr Word8)
wirePeekCoordinator version _fp _basePtr p0 endPtr = do
  (f0_key, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_nodeid, p2) <- W.peekInt32BE p1 endPtr
  (f2_host, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_port, p4) <- W.peekInt32BE p3 endPtr
  (f4_errorcode, p5) <- W.peekInt16BE p4 endPtr
  (f5_errormessage, p6) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p5 endPtr
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p6 endPtr else pure p6
  pure (Coordinator { coordinatorKey = f0_key, coordinatorNodeId = f1_nodeid, coordinatorHost = f2_host, coordinatorPort = f3_port, coordinatorErrorCode = f4_errorcode, coordinatorErrorMessage = f5_errormessage }, pTagsEnd)

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
    p1 <- W.pokeInt16BE p0 (findCoordinatorResponseErrorCode msg)
    p2 <- W.pokeInt32BE p1 (findCoordinatorResponseNodeId msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (findCoordinatorResponseHost msg))
    p4 <- W.pokeInt32BE p3 (findCoordinatorResponsePort msg)
    pure p4
  | version == 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (findCoordinatorResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (findCoordinatorResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (findCoordinatorResponseErrorMessage msg))
    p4 <- W.pokeInt32BE p3 (findCoordinatorResponseNodeId msg)
    p5 <- WP.pokeCompactString p4 (P.toCompactString (findCoordinatorResponseHost msg))
    p6 <- W.pokeInt32BE p5 (findCoordinatorResponsePort msg)
    WP.pokeEmptyTaggedFields p6
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (findCoordinatorResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (findCoordinatorResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (findCoordinatorResponseErrorMessage msg))
    p4 <- W.pokeInt32BE p3 (findCoordinatorResponseNodeId msg)
    p5 <- WP.pokeCompactString p4 (P.toCompactString (findCoordinatorResponseHost msg))
    p6 <- W.pokeInt32BE p5 (findCoordinatorResponsePort msg)
    pure p6
  | version >= 4 && version <= 6 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (findCoordinatorResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeCoordinator version p x) p1 (findCoordinatorResponseCoordinators msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke FindCoordinatorResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for FindCoordinatorResponse.
wirePeekFindCoordinatorResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FindCoordinatorResponse, Ptr Word8)
wirePeekFindCoordinatorResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_nodeid, p2) <- W.peekInt32BE p1 endPtr
    (f2_host, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_port, p4) <- W.peekInt32BE p3 endPtr
    pure (FindCoordinatorResponse { findCoordinatorResponseThrottleTimeMs = 0, findCoordinatorResponseErrorCode = f0_errorcode, findCoordinatorResponseErrorMessage = P.KafkaString Null, findCoordinatorResponseNodeId = f1_nodeid, findCoordinatorResponseHost = f2_host, findCoordinatorResponsePort = f3_port, findCoordinatorResponseCoordinators = P.mkKafkaArray V.empty }, p4)
  | version == 3 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_nodeid, p4) <- W.peekInt32BE p3 endPtr
    (f4_host, p5) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr
    (f5_port, p6) <- W.peekInt32BE p5 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p6 endPtr
    pure (FindCoordinatorResponse { findCoordinatorResponseThrottleTimeMs = f0_throttletimems, findCoordinatorResponseErrorCode = f1_errorcode, findCoordinatorResponseErrorMessage = f2_errormessage, findCoordinatorResponseNodeId = f3_nodeid, findCoordinatorResponseHost = f4_host, findCoordinatorResponsePort = f5_port, findCoordinatorResponseCoordinators = P.mkKafkaArray V.empty }, pTagsEnd)
  | version >= 1 && version <= 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_nodeid, p4) <- W.peekInt32BE p3 endPtr
    (f4_host, p5) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr
    (f5_port, p6) <- W.peekInt32BE p5 endPtr
    pure (FindCoordinatorResponse { findCoordinatorResponseThrottleTimeMs = f0_throttletimems, findCoordinatorResponseErrorCode = f1_errorcode, findCoordinatorResponseErrorMessage = f2_errormessage, findCoordinatorResponseNodeId = f3_nodeid, findCoordinatorResponseHost = f4_host, findCoordinatorResponsePort = f5_port, findCoordinatorResponseCoordinators = P.mkKafkaArray V.empty }, p6)
  | version >= 4 && version <= 6 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_coordinators, p2) <- WP.peekVersionedArray version 3 (\p e -> wirePeekCoordinator version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (FindCoordinatorResponse { findCoordinatorResponseThrottleTimeMs = f0_throttletimems, findCoordinatorResponseErrorCode = 0, findCoordinatorResponseErrorMessage = P.KafkaString Null, findCoordinatorResponseNodeId = 0, findCoordinatorResponseHost = P.KafkaString Null, findCoordinatorResponsePort = 0, findCoordinatorResponseCoordinators = f1_coordinators }, pTagsEnd)
  | otherwise = error $ "wirePeek FindCoordinatorResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec FindCoordinatorResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeFindCoordinatorResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeFindCoordinatorResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekFindCoordinatorResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}