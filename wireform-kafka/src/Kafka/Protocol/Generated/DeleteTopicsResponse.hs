{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteTopicsResponse
Description : Kafka DeleteTopicsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 20.



Valid versions: 1-6
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteTopicsResponse
  (
    DeleteTopicsResponse(..),
    DeletableTopicResult(..),
    encodeDeleteTopicsResponse,
    decodeDeleteTopicsResponse,
    maxDeleteTopicsResponseVersion
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


-- | The results for each topic we tried to delete.
data DeletableTopicResult = DeletableTopicResult
  {

  -- | The topic name.

  -- Versions: 0+
  deletableTopicResultName :: !(KafkaString)
,

  -- | The unique topic ID.

  -- Versions: 6+
  deletableTopicResultTopicId :: !(KafkaUuid)
,

  -- | The deletion error, or 0 if the deletion succeeded.

  -- Versions: 0+
  deletableTopicResultErrorCode :: !(Int16)
,

  -- | The error message, or null if there was no error.

  -- Versions: 5+
  deletableTopicResultErrorMessage :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode DeletableTopicResult with version-aware field handling.
encodeDeletableTopicResult :: MonadPut m => E.ApiVersion -> DeletableTopicResult -> m ()
encodeDeletableTopicResult version dmsg =
  do
    if version >= 4 then serialize (toCompactString (deletableTopicResultName dmsg)) else serialize (deletableTopicResultName dmsg)
    when (version >= 6) $
      serialize (deletableTopicResultTopicId dmsg)
    serialize (deletableTopicResultErrorCode dmsg)
    when (version >= 5) $
      if version >= 4 then serialize (toCompactString (deletableTopicResultErrorMessage dmsg)) else serialize (deletableTopicResultErrorMessage dmsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DeletableTopicResult with version-aware field handling.
decodeDeletableTopicResult :: MonadGet m => E.ApiVersion -> m DeletableTopicResult
decodeDeletableTopicResult version =
  do
    fieldname <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldtopicid <- if version >= 6
      then deserialize
      else pure (P.nullUuid)
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 5
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DeletableTopicResult
      {
      deletableTopicResultName = fieldname
      ,
      deletableTopicResultTopicId = fieldtopicid
      ,
      deletableTopicResultErrorCode = fielderrorcode
      ,
      deletableTopicResultErrorMessage = fielderrormessage
      }



data DeleteTopicsResponse = DeleteTopicsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  deleteTopicsResponseThrottleTimeMs :: !(Int32)
,

  -- | The results for each topic we tried to delete.

  -- Versions: 0+
  deleteTopicsResponseResponses :: !(KafkaArray (DeletableTopicResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteTopicsResponse.
maxDeleteTopicsResponseVersion :: Int16
maxDeleteTopicsResponseVersion = 6

-- | KafkaMessage instance for DeleteTopicsResponse.
instance KafkaMessage DeleteTopicsResponse where
  messageApiKey = 20
  messageMinVersion = 1
  messageMaxVersion = 6
  messageFlexibleVersion = Just 4

-- | Encode DeleteTopicsResponse with the given API version.
encodeDeleteTopicsResponse :: MonadPut m => E.ApiVersion -> DeleteTopicsResponse -> m ()
encodeDeleteTopicsResponse version msg
  | version >= 1 && version <= 3 =
    do
      serialize (deleteTopicsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 4 encodeDeletableTopicResult (case P.unKafkaArray (deleteTopicsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 4 && version <= 6 =
    do
      serialize (deleteTopicsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 4 encodeDeletableTopicResult (case P.unKafkaArray (deleteTopicsResponseResponses msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DeleteTopicsResponse with the given API version.
decodeDeleteTopicsResponse :: MonadGet m => E.ApiVersion -> m DeleteTopicsResponse
decodeDeleteTopicsResponse version
  | version >= 1 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDeletableTopicResult
      pure DeleteTopicsResponse
        {
        deleteTopicsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteTopicsResponseResponses = fieldresponses
        }

  | version >= 4 && version <= 6 =
    do
      fieldthrottletimems <- deserialize
      fieldresponses <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDeletableTopicResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DeleteTopicsResponse
        {
        deleteTopicsResponseThrottleTimeMs = fieldthrottletimems
        ,
        deleteTopicsResponseResponses = fieldresponses
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a DeletableTopicResult.
wireMaxSizeDeletableTopicResult :: Int -> DeletableTopicResult -> Int
wireMaxSizeDeletableTopicResult _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (deletableTopicResultName msg))
  + 16
  + 2
  + WP.compactStringMaxSize (P.toCompactString (deletableTopicResultErrorMessage msg))
  + 1

-- | Direct-poke encoder for DeletableTopicResult.
wirePokeDeletableTopicResult :: Int -> Ptr Word8 -> DeletableTopicResult -> IO (Ptr Word8)
wirePokeDeletableTopicResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (deletableTopicResultName msg))
  p2 <- WP.pokeKafkaUuid p1 (deletableTopicResultTopicId msg)
  p3 <- W.pokeInt16BE p2 (deletableTopicResultErrorCode msg)
  p4 <- WP.pokeCompactString p3 (P.toCompactString (deletableTopicResultErrorMessage msg))
  if version >= 4 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for DeletableTopicResult.
wirePeekDeletableTopicResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeletableTopicResult, Ptr Word8)
wirePeekDeletableTopicResult version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_topicid, p2) <- WP.peekKafkaUuid p1 endPtr
  (f2_errorcode, p3) <- W.peekInt16BE p2 endPtr
  (f3_errormessage, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (DeletableTopicResult { deletableTopicResultName = f0_name, deletableTopicResultTopicId = f1_topicid, deletableTopicResultErrorCode = f2_errorcode, deletableTopicResultErrorMessage = f3_errormessage }, pTagsEnd)

-- | Worst-case wire size of a DeleteTopicsResponse.
wireMaxSizeDeleteTopicsResponse :: Int -> DeleteTopicsResponse -> Int
wireMaxSizeDeleteTopicsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (deleteTopicsResponseResponses msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeletableTopicResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteTopicsResponse.
wirePokeDeleteTopicsResponse :: Int -> Ptr Word8 -> DeleteTopicsResponse -> IO (Ptr Word8)
wirePokeDeleteTopicsResponse version basePtr msg
  | version >= 1 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (deleteTopicsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeDeletableTopicResult version p x) p1 (deleteTopicsResponseResponses msg)
    pure p2
  | version >= 4 && version <= 6 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (deleteTopicsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeDeletableTopicResult version p x) p1 (deleteTopicsResponseResponses msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke DeleteTopicsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteTopicsResponse.
wirePeekDeleteTopicsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteTopicsResponse, Ptr Word8)
wirePeekDeleteTopicsResponse version _fp _basePtr p0 endPtr
  | version >= 1 && version <= 3 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_responses, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekDeletableTopicResult version _fp _basePtr p e) p1 endPtr
    pure (DeleteTopicsResponse { deleteTopicsResponseThrottleTimeMs = f0_throttletimems, deleteTopicsResponseResponses = f1_responses }, p2)
  | version >= 4 && version <= 6 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_responses, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekDeletableTopicResult version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DeleteTopicsResponse { deleteTopicsResponseThrottleTimeMs = f0_throttletimems, deleteTopicsResponseResponses = f1_responses }, pTagsEnd)
  | otherwise = error $ "wirePeek DeleteTopicsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec DeleteTopicsResponse where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteTopicsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteTopicsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteTopicsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}