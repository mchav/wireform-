{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AlterReplicaLogDirsRequest
Description : Kafka AlterReplicaLogDirsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 34.



Valid versions: 1-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AlterReplicaLogDirsRequest
  (
    AlterReplicaLogDirsRequest(..),
    AlterReplicaLogDir(..),
    AlterReplicaLogDirTopic(..),
    encodeAlterReplicaLogDirsRequest,
    decodeAlterReplicaLogDirsRequest,
    maxAlterReplicaLogDirsRequestVersion
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


-- | The topics to add to the directory.
data AlterReplicaLogDirTopic = AlterReplicaLogDirTopic
  {

  -- | The topic name.

  -- Versions: 0+
  alterReplicaLogDirTopicName :: !(KafkaString)
,

  -- | The partition indexes.

  -- Versions: 0+
  alterReplicaLogDirTopicPartitions :: !(KafkaArray (Int32))

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterReplicaLogDirTopic with version-aware field handling.
encodeAlterReplicaLogDirTopic :: MonadPut m => E.ApiVersion -> AlterReplicaLogDirTopic -> m ()
encodeAlterReplicaLogDirTopic version amsg =
  do
    if version >= 2 then serialize (toCompactString (alterReplicaLogDirTopicName amsg)) else serialize (alterReplicaLogDirTopicName amsg)
    E.encodeVersionedArray version 2 (\_ x -> serialize x) (case P.unKafkaArray (alterReplicaLogDirTopicPartitions amsg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int32"
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterReplicaLogDirTopic with version-aware field handling.
decodeAlterReplicaLogDirTopic :: MonadGet m => E.ApiVersion -> m AlterReplicaLogDirTopic
decodeAlterReplicaLogDirTopic version =
  do
    fieldname <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldpartitions <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 (\_ -> deserialize)
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterReplicaLogDirTopic
      {
      alterReplicaLogDirTopicName = fieldname
      ,
      alterReplicaLogDirTopicPartitions = fieldpartitions
      }


-- | The alterations to make for each directory.
data AlterReplicaLogDir = AlterReplicaLogDir
  {

  -- | The absolute directory path.

  -- Versions: 0+
  alterReplicaLogDirPath :: !(KafkaString)
,

  -- | The topics to add to the directory.

  -- Versions: 0+
  alterReplicaLogDirTopics :: !(KafkaArray (AlterReplicaLogDirTopic))

  }
  deriving (Eq, Show, Generic)


-- | Encode AlterReplicaLogDir with version-aware field handling.
encodeAlterReplicaLogDir :: MonadPut m => E.ApiVersion -> AlterReplicaLogDir -> m ()
encodeAlterReplicaLogDir version amsg =
  do
    if version >= 2 then serialize (toCompactString (alterReplicaLogDirPath amsg)) else serialize (alterReplicaLogDirPath amsg)
    E.encodeVersionedArray version 2 encodeAlterReplicaLogDirTopic (case P.unKafkaArray (alterReplicaLogDirTopics amsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 2) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode AlterReplicaLogDir with version-aware field handling.
decodeAlterReplicaLogDir :: MonadGet m => E.ApiVersion -> m AlterReplicaLogDir
decodeAlterReplicaLogDir version =
  do
    fieldpath <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
    fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAlterReplicaLogDirTopic
    _ <- if version >= 2 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure AlterReplicaLogDir
      {
      alterReplicaLogDirPath = fieldpath
      ,
      alterReplicaLogDirTopics = fieldtopics
      }



data AlterReplicaLogDirsRequest = AlterReplicaLogDirsRequest
  {

  -- | The alterations to make for each directory.

  -- Versions: 0+
  alterReplicaLogDirsRequestDirs :: !(KafkaArray (AlterReplicaLogDir))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AlterReplicaLogDirsRequest.
maxAlterReplicaLogDirsRequestVersion :: Int16
maxAlterReplicaLogDirsRequestVersion = 2

-- | KafkaMessage instance for AlterReplicaLogDirsRequest.
instance KafkaMessage AlterReplicaLogDirsRequest where
  messageApiKey = 34
  messageMinVersion = 1
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2

-- | Encode AlterReplicaLogDirsRequest with the given API version.
encodeAlterReplicaLogDirsRequest :: MonadPut m => E.ApiVersion -> AlterReplicaLogDirsRequest -> m ()
encodeAlterReplicaLogDirsRequest version msg
  | version == 1 =
    do
      E.encodeVersionedArray version 2 encodeAlterReplicaLogDir (case P.unKafkaArray (alterReplicaLogDirsRequestDirs msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version == 2 =
    do
      E.encodeVersionedArray version 2 encodeAlterReplicaLogDir (case P.unKafkaArray (alterReplicaLogDirsRequestDirs msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AlterReplicaLogDirsRequest with the given API version.
decodeAlterReplicaLogDirsRequest :: MonadGet m => E.ApiVersion -> m AlterReplicaLogDirsRequest
decodeAlterReplicaLogDirsRequest version
  | version == 1 =
    do
      fielddirs <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAlterReplicaLogDir
      pure AlterReplicaLogDirsRequest
        {
        alterReplicaLogDirsRequestDirs = fielddirs
        }

  | version == 2 =
    do
      fielddirs <- P.mkKafkaArray <$> E.decodeVersionedArray version 2 decodeAlterReplicaLogDir
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AlterReplicaLogDirsRequest
        {
        alterReplicaLogDirsRequestDirs = fielddirs
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a AlterReplicaLogDirTopic.
wireMaxSizeAlterReplicaLogDirTopic :: Int -> AlterReplicaLogDirTopic -> Int
wireMaxSizeAlterReplicaLogDirTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (alterReplicaLogDirTopicName msg))
  + (5 + (case P.unKafkaArray (alterReplicaLogDirTopicPartitions msg) of { P.NotNull v -> sum (fmap (\x -> 4 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterReplicaLogDirTopic.
wirePokeAlterReplicaLogDirTopic :: Int -> Ptr Word8 -> AlterReplicaLogDirTopic -> IO (Ptr Word8)
wirePokeAlterReplicaLogDirTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (alterReplicaLogDirTopicName msg))
  p2 <- WP.pokeVersionedArray version 2 W.pokeInt32BE p1 (alterReplicaLogDirTopicPartitions msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AlterReplicaLogDirTopic.
wirePeekAlterReplicaLogDirTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterReplicaLogDirTopic, Ptr Word8)
wirePeekAlterReplicaLogDirTopic version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_partitions, p2) <- WP.peekVersionedArray version 2 W.peekInt32BE p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AlterReplicaLogDirTopic { alterReplicaLogDirTopicName = f0_name, alterReplicaLogDirTopicPartitions = f1_partitions }, pTagsEnd)

-- | Worst-case wire size of a AlterReplicaLogDir.
wireMaxSizeAlterReplicaLogDir :: Int -> AlterReplicaLogDir -> Int
wireMaxSizeAlterReplicaLogDir _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (alterReplicaLogDirPath msg))
  + (5 + (case P.unKafkaArray (alterReplicaLogDirTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterReplicaLogDirTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterReplicaLogDir.
wirePokeAlterReplicaLogDir :: Int -> Ptr Word8 -> AlterReplicaLogDir -> IO (Ptr Word8)
wirePokeAlterReplicaLogDir version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (alterReplicaLogDirPath msg))
  p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAlterReplicaLogDirTopic version p x) p1 (alterReplicaLogDirTopics msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for AlterReplicaLogDir.
wirePeekAlterReplicaLogDir :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterReplicaLogDir, Ptr Word8)
wirePeekAlterReplicaLogDir version _fp _basePtr p0 endPtr = do
  (f0_path, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_topics, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAlterReplicaLogDirTopic version _fp _basePtr p e) p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (AlterReplicaLogDir { alterReplicaLogDirPath = f0_path, alterReplicaLogDirTopics = f1_topics }, pTagsEnd)

-- | Worst-case wire size of a AlterReplicaLogDirsRequest.
wireMaxSizeAlterReplicaLogDirsRequest :: Int -> AlterReplicaLogDirsRequest -> Int
wireMaxSizeAlterReplicaLogDirsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (alterReplicaLogDirsRequestDirs msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeAlterReplicaLogDir _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for AlterReplicaLogDirsRequest.
wirePokeAlterReplicaLogDirsRequest :: Int -> Ptr Word8 -> AlterReplicaLogDirsRequest -> IO (Ptr Word8)
wirePokeAlterReplicaLogDirsRequest version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAlterReplicaLogDir version p x) p0 (alterReplicaLogDirsRequestDirs msg)
    pure p1
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeAlterReplicaLogDir version p x) p0 (alterReplicaLogDirsRequestDirs msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke AlterReplicaLogDirsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for AlterReplicaLogDirsRequest.
wirePeekAlterReplicaLogDirsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AlterReplicaLogDirsRequest, Ptr Word8)
wirePeekAlterReplicaLogDirsRequest version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_dirs, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAlterReplicaLogDir version _fp _basePtr p e) p0 endPtr
    pure (AlterReplicaLogDirsRequest { alterReplicaLogDirsRequestDirs = f0_dirs }, p1)
  | version == 2 = do
    (f0_dirs, p1) <- WP.peekVersionedArray version 2 (\p e -> wirePeekAlterReplicaLogDir version _fp _basePtr p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (AlterReplicaLogDirsRequest { alterReplicaLogDirsRequestDirs = f0_dirs }, pTagsEnd)
  | otherwise = error $ "wirePeek AlterReplicaLogDirsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec AlterReplicaLogDirsRequest where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAlterReplicaLogDirsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAlterReplicaLogDirsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAlterReplicaLogDirsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}