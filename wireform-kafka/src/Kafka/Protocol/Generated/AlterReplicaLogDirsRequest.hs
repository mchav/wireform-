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
import Data.Bytes.Get (MonadGet)
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
import qualified Kafka.Protocol.Wire.Codec as WC


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

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec AlterReplicaLogDirsRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
