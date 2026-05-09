{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.UpdateRaftVoterResponse
Description : Kafka UpdateRaftVoterResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 82.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.UpdateRaftVoterResponse
  (
    UpdateRaftVoterResponse(..),
    CurrentLeader(..),
    encodeUpdateRaftVoterResponse,
    decodeUpdateRaftVoterResponse,
    maxUpdateRaftVoterResponseVersion
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


-- | Details of the current Raft cluster leader.
data CurrentLeader = CurrentLeader
  {

  -- | The replica id of the current leader or -1 if the leader is unknown.

  -- Versions: 0+
  currentLeaderLeaderId :: !(Int32)
,

  -- | The latest known leader epoch.

  -- Versions: 0+
  currentLeaderLeaderEpoch :: !(Int32)
,

  -- | The node's hostname.

  -- Versions: 0+
  currentLeaderHost :: !(KafkaString)
,

  -- | The node's port.

  -- Versions: 0+
  currentLeaderPort :: !(Int32)

  }
  deriving (Eq, Show, Generic)


-- | Encode CurrentLeader with version-aware field handling.
encodeCurrentLeader :: MonadPut m => E.ApiVersion -> CurrentLeader -> m ()
encodeCurrentLeader version cmsg =
  do
    serialize (currentLeaderLeaderId cmsg)
    serialize (currentLeaderLeaderEpoch cmsg)
    if version >= 0 then serialize (toCompactString (currentLeaderHost cmsg)) else serialize (currentLeaderHost cmsg)
    serialize (currentLeaderPort cmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CurrentLeader with version-aware field handling.
decodeCurrentLeader :: MonadGet m => E.ApiVersion -> m CurrentLeader
decodeCurrentLeader version =
  do
    fieldleaderid <- deserialize
    fieldleaderepoch <- deserialize
    fieldhost <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldport <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CurrentLeader
      {
      currentLeaderLeaderId = fieldleaderid
      ,
      currentLeaderLeaderEpoch = fieldleaderepoch
      ,
      currentLeaderHost = fieldhost
      ,
      currentLeaderPort = fieldport
      }



data UpdateRaftVoterResponse = UpdateRaftVoterResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  updateRaftVoterResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  updateRaftVoterResponseErrorCode :: !(Int16)
,

  -- | Details of the current Raft cluster leader.

  -- Versions: 0+
  updateRaftVoterResponseCurrentLeader :: !(CurrentLeader)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for UpdateRaftVoterResponse.
maxUpdateRaftVoterResponseVersion :: Int16
maxUpdateRaftVoterResponseVersion = 0

-- | KafkaMessage instance for UpdateRaftVoterResponse.
instance KafkaMessage UpdateRaftVoterResponse where
  messageApiKey = 82
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode UpdateRaftVoterResponse with the given API version.
encodeUpdateRaftVoterResponse :: MonadPut m => E.ApiVersion -> UpdateRaftVoterResponse -> m ()
encodeUpdateRaftVoterResponse version msg
  | version == 0 =
    do
      serialize (updateRaftVoterResponseThrottleTimeMs msg)
      serialize (updateRaftVoterResponseErrorCode msg)
      do
        let _entries = (if version >= 0 then [(0, Data.Bytes.Put.runPutS (encodeCurrentLeader version (updateRaftVoterResponseCurrentLeader msg)))] else [])
        P.serializeTaggedFieldEntries _entries
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode UpdateRaftVoterResponse with the given API version.
decodeUpdateRaftVoterResponse :: MonadGet m => E.ApiVersion -> m UpdateRaftVoterResponse
decodeUpdateRaftVoterResponse version
  | version == 0 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldcurrentleader =
            if version >= 0
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (decodeCurrentLeader version) _bs of
                    Right _v -> _v
                    Left  _  -> (CurrentLeader { currentLeaderLeaderId = (-1), currentLeaderLeaderEpoch = (-1), currentLeaderHost = P.KafkaString Null, currentLeaderPort = 0 })
                Nothing  -> (CurrentLeader { currentLeaderLeaderId = (-1), currentLeaderLeaderEpoch = (-1), currentLeaderHost = P.KafkaString Null, currentLeaderPort = 0 })
              else (CurrentLeader { currentLeaderLeaderId = (-1), currentLeaderLeaderEpoch = (-1), currentLeaderHost = P.KafkaString Null, currentLeaderPort = 0 })
      pure UpdateRaftVoterResponse
        {
        updateRaftVoterResponseThrottleTimeMs = fieldthrottletimems
        ,
        updateRaftVoterResponseErrorCode = fielderrorcode
        ,
        updateRaftVoterResponseCurrentLeader = fieldcurrentleader
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a CurrentLeader.
wireMaxSizeCurrentLeader :: Int -> CurrentLeader -> Int
wireMaxSizeCurrentLeader _version msg =
  0
  + 4
  + 4
  + WP.compactStringMaxSize (P.toCompactString (currentLeaderHost msg))
  + 4
  + 1

-- | Direct-poke encoder for CurrentLeader.
wirePokeCurrentLeader :: Int -> Ptr Word8 -> CurrentLeader -> IO (Ptr Word8)
wirePokeCurrentLeader version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (currentLeaderLeaderId msg)
  p2 <- W.pokeInt32BE p1 (currentLeaderLeaderEpoch msg)
  p3 <- WP.pokeCompactString p2 (P.toCompactString (currentLeaderHost msg))
  p4 <- W.pokeInt32BE p3 (currentLeaderPort msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for CurrentLeader.
wirePeekCurrentLeader :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CurrentLeader, Ptr Word8)
wirePeekCurrentLeader version _fp _basePtr p0 endPtr = do
  (f0_leaderid, p1) <- W.peekInt32BE p0 endPtr
  (f1_leaderepoch, p2) <- W.peekInt32BE p1 endPtr
  (f2_host, p3) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr
  (f3_port, p4) <- W.peekInt32BE p3 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (CurrentLeader { currentLeaderLeaderId = f0_leaderid, currentLeaderLeaderEpoch = f1_leaderepoch, currentLeaderHost = f2_host, currentLeaderPort = f3_port }, pTagsEnd)

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries tagged fields with payloads — KIP-866
-- style — that the generator hasn't been taught yet), so
-- we lift the legacy 'encodeUpdateRaftVoterResponse' / 'decodeUpdateRaftVoterResponse'
-- pair into a 'WireCodecImpl' via 'WC.serialShimCodec'.
-- The dispatch shape is identical to the native case —
-- every 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through
-- a 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec UpdateRaftVoterResponse where
  wireCodec = Just (WC.serialShimCodec encodeUpdateRaftVoterResponse decodeUpdateRaftVoterResponse)
  {-# INLINE wireCodec #-}