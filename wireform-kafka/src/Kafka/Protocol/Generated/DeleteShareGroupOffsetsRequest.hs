{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteShareGroupOffsetsRequest
Description : Kafka DeleteShareGroupOffsetsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 92.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteShareGroupOffsetsRequest
  (
    DeleteShareGroupOffsetsRequest(..),
    DeleteShareGroupOffsetsRequestTopic(..),
    encodeDeleteShareGroupOffsetsRequest,
    decodeDeleteShareGroupOffsetsRequest,
    maxDeleteShareGroupOffsetsRequestVersion
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


-- | The topics to delete offsets for.
data DeleteShareGroupOffsetsRequestTopic = DeleteShareGroupOffsetsRequestTopic
  {

  -- | The topic name.

  -- Versions: 0+
  deleteShareGroupOffsetsRequestTopicTopicName :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode DeleteShareGroupOffsetsRequestTopic with version-aware field handling.
encodeDeleteShareGroupOffsetsRequestTopic :: MonadPut m => E.ApiVersion -> DeleteShareGroupOffsetsRequestTopic -> m ()
encodeDeleteShareGroupOffsetsRequestTopic version dmsg =
  do
    if version >= 0 then serialize (toCompactString (deleteShareGroupOffsetsRequestTopicTopicName dmsg)) else serialize (deleteShareGroupOffsetsRequestTopicTopicName dmsg)
    when (version >= 0) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeleteShareGroupOffsetsRequestTopic with version-aware field handling.
decodeDeleteShareGroupOffsetsRequestTopic :: MonadGet m => E.ApiVersion -> m DeleteShareGroupOffsetsRequestTopic
decodeDeleteShareGroupOffsetsRequestTopic version =
  do
    fieldtopicname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
    _ <- if version >= 0 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeleteShareGroupOffsetsRequestTopic
      {
      deleteShareGroupOffsetsRequestTopicTopicName = fieldtopicname
      }



data DeleteShareGroupOffsetsRequest = DeleteShareGroupOffsetsRequest
  {

  -- | The group identifier.

  -- Versions: 0+
  deleteShareGroupOffsetsRequestGroupId :: !(KafkaString)
,

  -- | The topics to delete offsets for.

  -- Versions: 0+
  deleteShareGroupOffsetsRequestTopics :: !(KafkaArray (DeleteShareGroupOffsetsRequestTopic))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteShareGroupOffsetsRequest.
maxDeleteShareGroupOffsetsRequestVersion :: Int16
maxDeleteShareGroupOffsetsRequestVersion = 0

-- | KafkaMessage instance for DeleteShareGroupOffsetsRequest.
instance KafkaMessage DeleteShareGroupOffsetsRequest where
  messageApiKey = 92
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode DeleteShareGroupOffsetsRequest with the given API version.
encodeDeleteShareGroupOffsetsRequest :: MonadPut m => E.ApiVersion -> DeleteShareGroupOffsetsRequest -> m ()
encodeDeleteShareGroupOffsetsRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (deleteShareGroupOffsetsRequestGroupId msg))
      E.encodeVersionedArray version 0 encodeDeleteShareGroupOffsetsRequestTopic (case P.unKafkaArray (deleteShareGroupOffsetsRequestTopics msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteShareGroupOffsetsRequest with the given API version.
decodeDeleteShareGroupOffsetsRequest :: MonadGet m => E.ApiVersion -> m DeleteShareGroupOffsetsRequest
decodeDeleteShareGroupOffsetsRequest version
  | version == 0 =
    do
      fieldgroupid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtopics <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 decodeDeleteShareGroupOffsetsRequestTopic
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteShareGroupOffsetsRequest
        {
        deleteShareGroupOffsetsRequestGroupId = fieldgroupid
        ,
        deleteShareGroupOffsetsRequestTopics = fieldtopics
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a DeleteShareGroupOffsetsRequestTopic.
wireMaxSizeDeleteShareGroupOffsetsRequestTopic :: Int -> DeleteShareGroupOffsetsRequestTopic -> Int
wireMaxSizeDeleteShareGroupOffsetsRequestTopic _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (deleteShareGroupOffsetsRequestTopicTopicName msg))
  + 1

-- | Direct-poke encoder for DeleteShareGroupOffsetsRequestTopic.
wirePokeDeleteShareGroupOffsetsRequestTopic :: Int -> Ptr Word8 -> DeleteShareGroupOffsetsRequestTopic -> IO (Ptr Word8)
wirePokeDeleteShareGroupOffsetsRequestTopic version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (deleteShareGroupOffsetsRequestTopicTopicName msg))
  if version >= 0 then WP.pokeEmptyTaggedFields p1 else pure p1

-- | Direct-poke decoder for DeleteShareGroupOffsetsRequestTopic.
wirePeekDeleteShareGroupOffsetsRequestTopic :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteShareGroupOffsetsRequestTopic, Ptr Word8)
wirePeekDeleteShareGroupOffsetsRequestTopic version _fp _basePtr p0 endPtr = do
  (f0_topicname, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p1 endPtr else pure p1
  pure (DeleteShareGroupOffsetsRequestTopic { deleteShareGroupOffsetsRequestTopicTopicName = f0_topicname }, pTagsEnd)

-- | Worst-case wire size of a DeleteShareGroupOffsetsRequest.
wireMaxSizeDeleteShareGroupOffsetsRequest :: Int -> DeleteShareGroupOffsetsRequest -> Int
wireMaxSizeDeleteShareGroupOffsetsRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (deleteShareGroupOffsetsRequestGroupId msg))
  + (5 + (case P.unKafkaArray (deleteShareGroupOffsetsRequestTopics msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeleteShareGroupOffsetsRequestTopic _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteShareGroupOffsetsRequest.
wirePokeDeleteShareGroupOffsetsRequest :: Int -> Ptr Word8 -> DeleteShareGroupOffsetsRequest -> IO (Ptr Word8)
wirePokeDeleteShareGroupOffsetsRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (deleteShareGroupOffsetsRequestGroupId msg))
    p2 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDeleteShareGroupOffsetsRequestTopic version p x) p1 (deleteShareGroupOffsetsRequestTopics msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke DeleteShareGroupOffsetsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteShareGroupOffsetsRequest.
wirePeekDeleteShareGroupOffsetsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteShareGroupOffsetsRequest, Ptr Word8)
wirePeekDeleteShareGroupOffsetsRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_groupid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_topics, p2) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDeleteShareGroupOffsetsRequestTopic version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DeleteShareGroupOffsetsRequest { deleteShareGroupOffsetsRequestGroupId = f0_groupid, deleteShareGroupOffsetsRequestTopics = f1_topics }, pTagsEnd)
  | otherwise = error $ "wirePeek DeleteShareGroupOffsetsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DeleteShareGroupOffsetsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteShareGroupOffsetsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteShareGroupOffsetsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteShareGroupOffsetsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}