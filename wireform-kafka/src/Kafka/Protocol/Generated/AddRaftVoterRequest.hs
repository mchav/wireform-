{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AddRaftVoterRequest
Description : Kafka AddRaftVoterRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 80.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AddRaftVoterRequest
  (
    AddRaftVoterRequest(..),
    Listener(..),
    encodeAddRaftVoterRequest,
    decodeAddRaftVoterRequest,
    maxAddRaftVoterRequestVersion
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


-- | The endpoints that can be used to communicate with the voter.
data Listener = Listener
  {

  -- | The name of the endpoint.

  -- Versions: 0+
  listenerName :: !(KafkaString)
,

  -- | The hostname.

  -- Versions: 0+
  listenerHost :: !(KafkaString)
,

  -- | The port.

  -- Versions: 0+
  listenerPort :: !(Word16)

  }
  deriving (Eq, Show, Generic)


-- | Encode Listener with version-aware field handling.
encodeListener :: MonadPut m => E.ApiVersion -> Listener -> m ()
encodeListener version lmsg =
  do
    if version >= 0 then serialize (toCompactString (listenerName lmsg)) else serialize (listenerName lmsg)
    if version >= 0 then serialize (toCompactString (listenerHost lmsg)) else serialize (listenerHost lmsg)
    serialize (listenerPort lmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode Listener with version-aware field handling.
decodeListener :: MonadGet m => E.ApiVersion -> m Listener
decodeListener version =
  do
    fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldhost <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    fieldport <- deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure Listener
      {
      listenerName = fieldname
      ,
      listenerHost = fieldhost
      ,
      listenerPort = fieldport
      }



data AddRaftVoterRequest = AddRaftVoterRequest
  {

  -- | The cluster id.

  -- Versions: 0+
  addRaftVoterRequestClusterId :: !(KafkaString)
,

  -- | The maximum time to wait for the request to complete before returning.

  -- Versions: 0+
  addRaftVoterRequestTimeoutMs :: !(Int32)
,

  -- | The replica id of the voter getting added to the topic partition.

  -- Versions: 0+
  addRaftVoterRequestVoterId :: !(Int32)
,

  -- | The directory id of the voter getting added to the topic partition.

  -- Versions: 0+
  addRaftVoterRequestVoterDirectoryId :: !(KafkaUuid)
,

  -- | The endpoints that can be used to communicate with the voter.

  -- Versions: 0+
  addRaftVoterRequestListeners :: !(KafkaArray (Listener))
,

  -- | When true, return a response after the new voter set is committed. Otherwise, return after the leade

  -- Versions: 1+
  addRaftVoterRequestAckWhenCommitted :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AddRaftVoterRequest.
maxAddRaftVoterRequestVersion :: Int16
maxAddRaftVoterRequestVersion = 1

-- | KafkaMessage instance for AddRaftVoterRequest.
instance KafkaMessage AddRaftVoterRequest where
  messageApiKey = 80
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0

-- | Encode AddRaftVoterRequest with the given API version.
encodeAddRaftVoterRequest :: MonadPut m => E.ApiVersion -> AddRaftVoterRequest -> m ()
encodeAddRaftVoterRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (addRaftVoterRequestClusterId msg))
      serialize (addRaftVoterRequestTimeoutMs msg)
      serialize (addRaftVoterRequestVoterId msg)
      serialize (addRaftVoterRequestVoterDirectoryId msg)
      E.encodeVersionedArray version 0 encodeListener (case P.unKafkaArray (addRaftVoterRequestListeners msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 1 =
    do
      serialize (toCompactString (addRaftVoterRequestClusterId msg))
      serialize (addRaftVoterRequestTimeoutMs msg)
      serialize (addRaftVoterRequestVoterId msg)
      serialize (addRaftVoterRequestVoterDirectoryId msg)
      E.encodeVersionedArray version 0 encodeListener (case P.unKafkaArray (addRaftVoterRequestListeners msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (addRaftVoterRequestAckWhenCommitted msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AddRaftVoterRequest with the given API version.
decodeAddRaftVoterRequest :: MonadGet m => E.ApiVersion -> m AddRaftVoterRequest
decodeAddRaftVoterRequest version
  | version == 0 =
    do
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtimeoutms <- deserialize
      fieldvoterid <- deserialize
      fieldvoterdirectoryid <- deserialize
      fieldlisteners <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeListener
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AddRaftVoterRequest
        {
        addRaftVoterRequestClusterId = fieldclusterid
        ,
        addRaftVoterRequestTimeoutMs = fieldtimeoutms
        ,
        addRaftVoterRequestVoterId = fieldvoterid
        ,
        addRaftVoterRequestVoterDirectoryId = fieldvoterdirectoryid
        ,
        addRaftVoterRequestListeners = fieldlisteners
        ,
        addRaftVoterRequestAckWhenCommitted = True
        }

  | version == 1 =
    do
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtimeoutms <- deserialize
      fieldvoterid <- deserialize
      fieldvoterdirectoryid <- deserialize
      fieldlisteners <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeListener
      fieldackwhencommitted <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AddRaftVoterRequest
        {
        addRaftVoterRequestClusterId = fieldclusterid
        ,
        addRaftVoterRequestTimeoutMs = fieldtimeoutms
        ,
        addRaftVoterRequestVoterId = fieldvoterid
        ,
        addRaftVoterRequestVoterDirectoryId = fieldvoterdirectoryid
        ,
        addRaftVoterRequestListeners = fieldlisteners
        ,
        addRaftVoterRequestAckWhenCommitted = fieldackwhencommitted
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a Listener.
wireMaxSizeListener :: Int -> Listener -> Int
wireMaxSizeListener _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (listenerName msg))
  + WP.compactStringMaxSize (P.toCompactString (listenerHost msg))
  + 2
  + 1

-- | Direct-poke encoder for Listener.
wirePokeListener :: Int -> Ptr Word8 -> Listener -> IO (Ptr Word8)
wirePokeListener version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (listenerName msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (listenerHost msg))
  p3 <- W.pokeWord16BE p2 (listenerPort msg)
  if version >= 0 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for Listener.
wirePeekListener :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (Listener, Ptr Word8)
wirePeekListener version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_host, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_port, p3) <- W.peekWord16BE p2 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (Listener { listenerName = f0_name, listenerHost = f1_host, listenerPort = f2_port }, pTagsEnd)

-- | Worst-case wire size of a AddRaftVoterRequest.
wireMaxSizeAddRaftVoterRequest :: Int -> AddRaftVoterRequest -> Int
wireMaxSizeAddRaftVoterRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (addRaftVoterRequestClusterId msg))
  + 4
  + 4
  + 16
  + (5 + (case P.unKafkaArray (addRaftVoterRequestListeners msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeListener _version x ) v); P.Null -> 0 }))
  + 1
  + 1

-- | Direct-poke encoder for AddRaftVoterRequest.
wirePokeAddRaftVoterRequest :: Int -> Ptr Word8 -> AddRaftVoterRequest -> IO (Ptr Word8)
wirePokeAddRaftVoterRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (addRaftVoterRequestClusterId msg))
    p2 <- W.pokeInt32BE p1 (addRaftVoterRequestTimeoutMs msg)
    p3 <- W.pokeInt32BE p2 (addRaftVoterRequestVoterId msg)
    p4 <- WP.pokeKafkaUuid p3 (addRaftVoterRequestVoterDirectoryId msg)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeListener version p x) p4 (addRaftVoterRequestListeners msg)
    WP.pokeEmptyTaggedFields p5
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (addRaftVoterRequestClusterId msg))
    p2 <- W.pokeInt32BE p1 (addRaftVoterRequestTimeoutMs msg)
    p3 <- W.pokeInt32BE p2 (addRaftVoterRequestVoterId msg)
    p4 <- WP.pokeKafkaUuid p3 (addRaftVoterRequestVoterDirectoryId msg)
    p5 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeListener version p x) p4 (addRaftVoterRequestListeners msg)
    p6 <- W.pokeWord8 p5 (if (addRaftVoterRequestAckWhenCommitted msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p6
  | otherwise = error $ "wirePoke AddRaftVoterRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for AddRaftVoterRequest.
wirePeekAddRaftVoterRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AddRaftVoterRequest, Ptr Word8)
wirePeekAddRaftVoterRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_clusterid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_timeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_voterid, p3) <- W.peekInt32BE p2 endPtr
    (f3_voterdirectoryid, p4) <- WP.peekKafkaUuid p3 endPtr
    (f4_listeners, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekListener version _fp _basePtr p e) p4 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p5 endPtr
    pure (AddRaftVoterRequest { addRaftVoterRequestClusterId = f0_clusterid, addRaftVoterRequestTimeoutMs = f1_timeoutms, addRaftVoterRequestVoterId = f2_voterid, addRaftVoterRequestVoterDirectoryId = f3_voterdirectoryid, addRaftVoterRequestListeners = f4_listeners, addRaftVoterRequestAckWhenCommitted = False }, pTagsEnd)
  | version == 1 = do
    (f0_clusterid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_timeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_voterid, p3) <- W.peekInt32BE p2 endPtr
    (f3_voterdirectoryid, p4) <- WP.peekKafkaUuid p3 endPtr
    (f4_listeners, p5) <- WP.peekVersionedArray version 0 (\p e -> wirePeekListener version _fp _basePtr p e) p4 endPtr
    (f5_ackwhencommitted, p6) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p5 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p6 endPtr
    pure (AddRaftVoterRequest { addRaftVoterRequestClusterId = f0_clusterid, addRaftVoterRequestTimeoutMs = f1_timeoutms, addRaftVoterRequestVoterId = f2_voterid, addRaftVoterRequestVoterDirectoryId = f3_voterdirectoryid, addRaftVoterRequestListeners = f4_listeners, addRaftVoterRequestAckWhenCommitted = f5_ackwhencommitted }, pTagsEnd)
  | otherwise = error $ "wirePeek AddRaftVoterRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec AddRaftVoterRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAddRaftVoterRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAddRaftVoterRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAddRaftVoterRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}