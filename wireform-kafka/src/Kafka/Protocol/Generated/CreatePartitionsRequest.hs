{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.CreatePartitionsRequest
Description : Kafka CreatePartitionsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 37.



Valid versions: 0-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.CreatePartitionsRequest
  (
    CreatePartitionsRequest(..),
    CreatePartitionsTopic(..),
    CreatePartitionsAssignment(..),
    encodeCreatePartitionsRequest,
    decodeCreatePartitionsRequest,
    maxCreatePartitionsRequestVersion
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


-- | The new partition assignments.
data CreatePartitionsAssignment = CreatePartitionsAssignment
  {

  -- | The assigned broker IDs.

  -- Versions: 0+
  createPartitionsAssignmentBrokerIds :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode CreatePartitionsAssignment with version-aware field handling.
encodeCreatePartitionsAssignment :: MonadPut m => E.ApiVersion -> CreatePartitionsAssignment -> m ()
encodeCreatePartitionsAssignment version cmsg =
  do
    E.encodeVersionedArray version 2 (\_ x -> serialize x) (case P.unKafkaArray (createPartitionsAssignmentBrokerIds cmsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CreatePartitionsAssignment with version-aware field handling.
decodeCreatePartitionsAssignment :: MonadGet m => E.ApiVersion -> m CreatePartitionsAssignment
decodeCreatePartitionsAssignment version =
  do
    fieldbrokerids <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 (\_ -> deserialize)
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CreatePartitionsAssignment
      {
      createPartitionsAssignmentBrokerIds = fieldbrokerids
      }


-- | Each topic that we want to create new partitions inside.
data CreatePartitionsTopic = CreatePartitionsTopic
  {

  -- | The topic name.

  -- Versions: 0+
  createPartitionsTopicName :: !(KafkaString)
,

  -- | The new partition count.

  -- Versions: 0+
  createPartitionsTopicCount :: !(Int32)
,

  -- | The new partition assignments.

  -- Versions: 0+
  createPartitionsTopicAssignments :: !(KafkaArray (CreatePartitionsAssignment))

  }
  deriving (Eq, Show, Generic)


-- | Encode CreatePartitionsTopic with version-aware field handling.
encodeCreatePartitionsTopic :: MonadPut m => E.ApiVersion -> CreatePartitionsTopic -> m ()
encodeCreatePartitionsTopic version cmsg =
  do
    if version >= 2 then serialize (toCompactString (createPartitionsTopicName cmsg)) else serialize (createPartitionsTopicName cmsg)
    serialize (createPartitionsTopicCount cmsg)
    E.encodeVersionedNullableArray version 2 encodeCreatePartitionsAssignment (createPartitionsTopicAssignments cmsg)
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode CreatePartitionsTopic with version-aware field handling.
decodeCreatePartitionsTopic :: MonadGet m => E.ApiVersion -> m CreatePartitionsTopic
decodeCreatePartitionsTopic version =
  do
    fieldname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldcount <- deserialize
    fieldassignments <- E.decodeVersionedNullableArray version 2 decodeCreatePartitionsAssignment
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure CreatePartitionsTopic
      {
      createPartitionsTopicName = fieldname
      ,
      createPartitionsTopicCount = fieldcount
      ,
      createPartitionsTopicAssignments = fieldassignments
      }



data CreatePartitionsRequest = CreatePartitionsRequest
  {

  -- | Each topic that we want to create new partitions inside.

  -- Versions: 0+
  createPartitionsRequestTopics :: !(KafkaArray (CreatePartitionsTopic))
,

  -- | The time in ms to wait for the partitions to be created.

  -- Versions: 0+
  createPartitionsRequestTimeoutMs :: !(Int32)
,

  -- | If true, then validate the request, but don't actually increase the number of partitions.

  -- Versions: 0+
  createPartitionsRequestValidateOnly :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for CreatePartitionsRequest.
maxCreatePartitionsRequestVersion :: Int16
maxCreatePartitionsRequestVersion = 3

-- | KafkaMessage instance for CreatePartitionsRequest.
instance KafkaMessage CreatePartitionsRequest where
  messageApiKey = 37
  messageMinVersion = 0
  messageMaxVersion = 3
  messageFlexibleVersion = Just 2

-- | Encode CreatePartitionsRequest with the given API version.
encodeCreatePartitionsRequest :: MonadPut m => E.ApiVersion -> CreatePartitionsRequest -> m ()
encodeCreatePartitionsRequest version msg
  | version >= 0 && version <= 1 =
    do
      E.encodeVersionedArray version 2 encodeCreatePartitionsTopic (case P.unKafkaArray (createPartitionsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (createPartitionsRequestTimeoutMs msg)
      serialize (createPartitionsRequestValidateOnly msg)


  | version >= 2 && version <= 3 =
    do
      E.encodeVersionedArray version 2 encodeCreatePartitionsTopic (case P.unKafkaArray (createPartitionsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (createPartitionsRequestTimeoutMs msg)
      serialize (createPartitionsRequestValidateOnly msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode CreatePartitionsRequest with the given API version.
decodeCreatePartitionsRequest :: MonadGet m => E.ApiVersion -> m CreatePartitionsRequest
decodeCreatePartitionsRequest version
  | version >= 0 && version <= 1 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeCreatePartitionsTopic
      fieldtimeoutms <- deserialize
      fieldvalidateonly <- deserialize
      pure CreatePartitionsRequest
        {
        createPartitionsRequestTopics = fieldtopics
        ,
        createPartitionsRequestTimeoutMs = fieldtimeoutms
        ,
        createPartitionsRequestValidateOnly = fieldvalidateonly
        }

  | version >= 2 && version <= 3 =
    do
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeCreatePartitionsTopic
      fieldtimeoutms <- deserialize
      fieldvalidateonly <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure CreatePartitionsRequest
        {
        createPartitionsRequestTopics = fieldtopics
        ,
        createPartitionsRequestTimeoutMs = fieldtimeoutms
        ,
        createPartitionsRequestValidateOnly = fieldvalidateonly
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a CreatePartitionsAssignment.
wireMaxSizeCreatePartitionsAssignment :: Int -> CreatePartitionsAssignment -> Int
wireMaxSizeCreatePartitionsAssignment _version msg =
  0
  + (5 + (case P.unKafkaArray (createPartitionsAssignmentBrokerIds msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for CreatePartitionsAssignment.
wirePokeCreatePartitionsAssignment :: Int -> Ptr Word8 -> CreatePartitionsAssignment -> IO (Ptr Word8)
wirePokeCreatePartitionsAssignment version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeVersionedArray version 2 W.pokeInt32BE p0 (createPartitionsAssignmentBrokerIds msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p1 else pure p1

-- | Direct-poke decoder for CreatePartitionsAssignment.
wirePeekCreatePartitionsAssignment :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreatePartitionsAssignment, Ptr Word8)
wirePeekCreatePartitionsAssignment version _fp _basePtr p0 endPtr = do
  (f0_brokerids, p1) <- WP.peekVersionedArray version 2 W.peekInt32BE p0 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p1 endPtr else pure p1
  pure (CreatePartitionsAssignment { createPartitionsAssignmentBrokerIds = f0_brokerids }, pTagsEnd)

-- | Worst-case wire size of a CreatePartitionsTopic.
wireMaxSizeCreatePartitionsTopic :: Int -> CreatePartitionsTopic -> Int
wireMaxSizeCreatePartitionsTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (createPartitionsTopicName msg))
  + 4
  + (5 + (case P.unKafkaArray (createPartitionsTopicAssignments msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeCreatePartitionsAssignment _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for CreatePartitionsTopic.
wirePokeCreatePartitionsTopic :: Int -> Ptr Word8 -> CreatePartitionsTopic -> IO (Ptr Word8)
wirePokeCreatePartitionsTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (createPartitionsTopicName msg))
  p2 <- W.pokeInt32BE p1 (createPartitionsTopicCount msg)
  p3 <- WP.pokeVersionedNullableArray version 2 (\p x -> wirePokeCreatePartitionsAssignment version p x) p2 (createPartitionsTopicAssignments msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for CreatePartitionsTopic.
wirePeekCreatePartitionsTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreatePartitionsTopic, Ptr Word8)
wirePeekCreatePartitionsTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_count, p2) <- W.peekInt32BE p1 endPtr
  (f2_assignments, p3) <- WP.peekVersionedNullableArray version 2 (\p e -> wirePeekCreatePartitionsAssignment version _fp _basePtr p e) p2 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (CreatePartitionsTopic { createPartitionsTopicName = f0_name, createPartitionsTopicCount = f1_count, createPartitionsTopicAssignments = f2_assignments }, pTagsEnd)

-- | Worst-case wire size of a CreatePartitionsRequest.
wireMaxSizeCreatePartitionsRequest :: Int -> CreatePartitionsRequest -> Int
wireMaxSizeCreatePartitionsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (createPartitionsRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeCreatePartitionsTopic _version x ) v); P.Null -> 0 }))
  + 4
  + 1
  + 1

-- | Direct-poke encoder for CreatePartitionsRequest.
wirePokeCreatePartitionsRequest :: Int -> Ptr Word8 -> CreatePartitionsRequest -> IO (Ptr Word8)
wirePokeCreatePartitionsRequest version basePtr msg
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeCreatePartitionsTopic version p x) p0 (createPartitionsRequestTopics msg)
    p2 <- W.pokeInt32BE p1 (createPartitionsRequestTimeoutMs msg)
    p3 <- W.pokeWord8 p2 (if (createPartitionsRequestValidateOnly msg) then 1 else 0)
    pure p3
  | version >= 2 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeCreatePartitionsTopic version p x) p0 (createPartitionsRequestTopics msg)
    p2 <- W.pokeInt32BE p1 (createPartitionsRequestTimeoutMs msg)
    p3 <- W.pokeWord8 p2 (if (createPartitionsRequestValidateOnly msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke CreatePartitionsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for CreatePartitionsRequest.
wirePeekCreatePartitionsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (CreatePartitionsRequest, Ptr Word8)
wirePeekCreatePartitionsRequest version _fp _basePtr p0 endPtr
  | version >= 0 && version <= 1 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekCreatePartitionsTopic version _fp _basePtr p e) p0 endPtr
    (f1_timeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_validateonly, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
    pure (CreatePartitionsRequest { createPartitionsRequestTopics = f0_topics, createPartitionsRequestTimeoutMs = f1_timeoutms, createPartitionsRequestValidateOnly = f2_validateonly }, p3)
  | version >= 2 && version <= 3 = do
    (f0_topics, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekCreatePartitionsTopic version _fp _basePtr p e) p0 endPtr
    (f1_timeoutms, p2) <- W.peekInt32BE p1 endPtr
    (f2_validateonly, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (CreatePartitionsRequest { createPartitionsRequestTopics = f0_topics, createPartitionsRequestTimeoutMs = f1_timeoutms, createPartitionsRequestValidateOnly = f2_validateonly }, pTagsEnd)
  | otherwise = error $ "wirePeek CreatePartitionsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec CreatePartitionsRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeCreatePartitionsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeCreatePartitionsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekCreatePartitionsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}