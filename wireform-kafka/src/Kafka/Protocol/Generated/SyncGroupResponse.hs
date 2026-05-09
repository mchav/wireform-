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
    encodeSyncGroupResponse,
    decodeSyncGroupResponse,
    maxSyncGroupResponseVersion
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

-- | Encode SyncGroupResponse with the given API version.
encodeSyncGroupResponse :: MonadPut m => E.ApiVersion -> SyncGroupResponse -> m ()
encodeSyncGroupResponse version msg
  | version == 0 =
    do
      serialize (syncGroupResponseErrorCode msg)
      serialize (syncGroupResponseAssignment msg)


  | version == 4 =
    do
      serialize (syncGroupResponseThrottleTimeMs msg)
      serialize (syncGroupResponseErrorCode msg)
      serialize (toCompactBytes (syncGroupResponseAssignment msg))
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 5 =
    do
      serialize (syncGroupResponseThrottleTimeMs msg)
      serialize (syncGroupResponseErrorCode msg)
      serialize (toCompactString (syncGroupResponseProtocolType msg))
      serialize (toCompactString (syncGroupResponseProtocolName msg))
      serialize (toCompactBytes (syncGroupResponseAssignment msg))
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 3 =
    do
      serialize (syncGroupResponseThrottleTimeMs msg)
      serialize (syncGroupResponseErrorCode msg)
      serialize (syncGroupResponseAssignment msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode SyncGroupResponse with the given API version.
decodeSyncGroupResponse :: MonadGet m => E.ApiVersion -> m SyncGroupResponse
decodeSyncGroupResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      fieldassignment <- deserialize
      pure SyncGroupResponse
        {
        syncGroupResponseThrottleTimeMs = 0
        ,
        syncGroupResponseErrorCode = fielderrorcode
        ,
        syncGroupResponseProtocolType = P.KafkaString Null
        ,
        syncGroupResponseProtocolName = P.KafkaString Null
        ,
        syncGroupResponseAssignment = fieldassignment
        }

  | version == 4 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldassignment <- if version >= 4 then P.fromCompactBytes <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure SyncGroupResponse
        {
        syncGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        syncGroupResponseErrorCode = fielderrorcode
        ,
        syncGroupResponseProtocolType = P.KafkaString Null
        ,
        syncGroupResponseProtocolName = P.KafkaString Null
        ,
        syncGroupResponseAssignment = fieldassignment
        }

  | version == 5 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldprotocoltype <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldprotocolname <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      fieldassignment <- if version >= 4 then P.fromCompactBytes <$> deserialize else deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure SyncGroupResponse
        {
        syncGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        syncGroupResponseErrorCode = fielderrorcode
        ,
        syncGroupResponseProtocolType = fieldprotocoltype
        ,
        syncGroupResponseProtocolName = fieldprotocolname
        ,
        syncGroupResponseAssignment = fieldassignment
        }

  | version >= 1 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldassignment <- deserialize
      pure SyncGroupResponse
        {
        syncGroupResponseThrottleTimeMs = fieldthrottletimems
        ,
        syncGroupResponseErrorCode = fielderrorcode
        ,
        syncGroupResponseProtocolType = P.KafkaString Null
        ,
        syncGroupResponseProtocolName = P.KafkaString Null
        ,
        syncGroupResponseAssignment = fieldassignment
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


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
    p2 <- WP.pokeCompactBytes p1 (P.toCompactBytes (syncGroupResponseAssignment msg))
    pure p2
  | version == 4 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (syncGroupResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (syncGroupResponseErrorCode msg)
    p3 <- WP.pokeCompactBytes p2 (P.toCompactBytes (syncGroupResponseAssignment msg))
    WP.pokeEmptyTaggedFields p3
  | version == 5 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (syncGroupResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (syncGroupResponseErrorCode msg)
    p3 <- WP.pokeCompactString p2 (P.toCompactString (syncGroupResponseProtocolType msg))
    p4 <- WP.pokeCompactString p3 (P.toCompactString (syncGroupResponseProtocolName msg))
    p5 <- WP.pokeCompactBytes p4 (P.toCompactBytes (syncGroupResponseAssignment msg))
    WP.pokeEmptyTaggedFields p5
  | version >= 1 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (syncGroupResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (syncGroupResponseErrorCode msg)
    p3 <- WP.pokeCompactBytes p2 (P.toCompactBytes (syncGroupResponseAssignment msg))
    pure p3
  | otherwise = error $ "wirePoke SyncGroupResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for SyncGroupResponse.
wirePeekSyncGroupResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SyncGroupResponse, Ptr Word8)
wirePeekSyncGroupResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_assignment, p2) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p1 endPtr
    pure (SyncGroupResponse { syncGroupResponseThrottleTimeMs = 0, syncGroupResponseErrorCode = f0_errorcode, syncGroupResponseProtocolType = P.KafkaString Null, syncGroupResponseProtocolName = P.KafkaString Null, syncGroupResponseAssignment = f1_assignment }, p2)
  | version == 4 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_assignment, p3) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (SyncGroupResponse { syncGroupResponseThrottleTimeMs = f0_throttletimems, syncGroupResponseErrorCode = f1_errorcode, syncGroupResponseProtocolType = P.KafkaString Null, syncGroupResponseProtocolName = P.KafkaString Null, syncGroupResponseAssignment = f2_assignment }, pTagsEnd)
  | version == 5 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_protocoltype, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
    (f3_protocolname, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
    (f4_assignment, p5) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p4 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (SyncGroupResponse { syncGroupResponseThrottleTimeMs = f0_throttletimems, syncGroupResponseErrorCode = f1_errorcode, syncGroupResponseProtocolType = f2_protocoltype, syncGroupResponseProtocolName = f3_protocolname, syncGroupResponseAssignment = f4_assignment }, pTagsEnd)
  | version >= 1 && version <= 3 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_assignment, p3) <- (\(cb, p') -> (P.fromCompactBytes cb, p')) <$> WP.peekCompactBytes p2 endPtr
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