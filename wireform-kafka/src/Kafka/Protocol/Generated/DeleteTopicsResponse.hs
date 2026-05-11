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
    maxDeleteTopicsResponseVersion
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

-- | Worst-case wire size of a DeletableTopicResult.
wireMaxSizeDeletableTopicResult :: Int -> DeletableTopicResult -> Int
wireMaxSizeDeletableTopicResult _version msg =
  0
  + WP.dualStringMaxSize (deletableTopicResultName msg)
  + 16
  + 2
  + WP.dualStringMaxSize (deletableTopicResultErrorMessage msg)
  + 1

-- | Direct-poke encoder for DeletableTopicResult.
wirePokeDeletableTopicResult :: Int -> Ptr Word8 -> DeletableTopicResult -> IO (Ptr Word8)
wirePokeDeletableTopicResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 4 then WP.pokeCompactString p0 (P.toCompactString (deletableTopicResultName msg)) else WP.pokeKafkaString p0 (deletableTopicResultName msg))
  p2 <- (if version >= 6 then WP.pokeKafkaUuid p1 (deletableTopicResultTopicId msg) else pure p1)
  p3 <- W.pokeInt16BE p2 (deletableTopicResultErrorCode msg)
  p4 <- (if version >= 5 then (if version >= 4 then WP.pokeCompactString p3 (P.toCompactString (deletableTopicResultErrorMessage msg)) else WP.pokeKafkaString p3 (deletableTopicResultErrorMessage msg)) else pure p3)
  if version >= 4 then WP.pokeEmptyTaggedFields p4 else pure p4

-- | Direct-poke decoder for DeletableTopicResult.
wirePeekDeletableTopicResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeletableTopicResult, Ptr Word8)
wirePeekDeletableTopicResult version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_topicid, p2) <- (if version >= 6 then WP.peekKafkaUuid p1 endPtr else pure (P.nullUuid, p1))
  (f2_errorcode, p3) <- W.peekInt16BE p2 endPtr
  (f3_errormessage, p4) <- (if version >= 5 then (if version >= 4 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr) else pure (P.KafkaString Null, p3))
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p4 endPtr else pure p4
  pure (DeletableTopicResult { deletableTopicResultName = f0_name, deletableTopicResultTopicId = f1_topicid, deletableTopicResultErrorCode = f2_errorcode, deletableTopicResultErrorMessage = f3_errormessage }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDeletableTopicResult :: DeletableTopicResult
defaultDeletableTopicResult = DeletableTopicResult { deletableTopicResultName = P.KafkaString Null, deletableTopicResultTopicId = P.nullUuid, deletableTopicResultErrorCode = 0, deletableTopicResultErrorMessage = P.KafkaString Null }

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
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (deleteTopicsResponseThrottleTimeMs msg) else pure p0)
    p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeDeletableTopicResult version p x) p1 (deleteTopicsResponseResponses msg)
    pure p2
  | version >= 4 && version <= 6 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then W.pokeInt32BE p0 (deleteTopicsResponseThrottleTimeMs msg) else pure p0)
    p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeDeletableTopicResult version p x) p1 (deleteTopicsResponseResponses msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke DeleteTopicsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteTopicsResponse.
wirePeekDeleteTopicsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteTopicsResponse, Ptr Word8)
wirePeekDeleteTopicsResponse version _fp _basePtr p0 endPtr
  | version >= 1 && version <= 3 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_responses, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekDeletableTopicResult version _fp _basePtr p e) p1 endPtr
    pure (DeleteTopicsResponse { deleteTopicsResponseThrottleTimeMs = f0_throttletimems, deleteTopicsResponseResponses = f1_responses }, p2)
  | version >= 4 && version <= 6 = do
    (f0_throttletimems, p1) <- (if version >= 1 then W.peekInt32BE p0 endPtr else pure (0, p0))
    (f1_responses, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekDeletableTopicResult version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DeleteTopicsResponse { deleteTopicsResponseThrottleTimeMs = f0_throttletimems, deleteTopicsResponseResponses = f1_responses }, pTagsEnd)
  | otherwise = error $ "wirePeek DeleteTopicsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec DeleteTopicsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteTopicsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteTopicsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteTopicsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}