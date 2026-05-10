{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteGroupsResponse
Description : Kafka DeleteGroupsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 42.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteGroupsResponse
  (
    DeleteGroupsResponse(..),
    DeletableGroupResult(..),
    maxDeleteGroupsResponseVersion
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


-- | The deletion results.
data DeletableGroupResult = DeletableGroupResult
  {

  -- | The group id.

  -- Versions: 0+
  deletableGroupResultGroupId :: !(KafkaString)
,

  -- | The deletion error, or 0 if the deletion succeeded.

  -- Versions: 0+
  deletableGroupResultErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)


data DeleteGroupsResponse = DeleteGroupsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  deleteGroupsResponseThrottleTimeMs :: !(Int32)
,

  -- | The deletion results.

  -- Versions: 0+
  deleteGroupsResponseResults :: !(KafkaArray (DeletableGroupResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteGroupsResponse.
maxDeleteGroupsResponseVersion :: Int16
maxDeleteGroupsResponseVersion = 2

-- | KafkaMessage instance for DeleteGroupsResponse.
instance KafkaMessage DeleteGroupsResponse where
  messageApiKey = 42
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2

-- | Worst-case wire size of a DeletableGroupResult.
wireMaxSizeDeletableGroupResult :: Int -> DeletableGroupResult -> Int
wireMaxSizeDeletableGroupResult _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (deletableGroupResultGroupId msg))
  + 2
  + 1

-- | Direct-poke encoder for DeletableGroupResult.
wirePokeDeletableGroupResult :: Int -> Ptr Word8 -> DeletableGroupResult -> IO (Ptr Word8)
wirePokeDeletableGroupResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- (if version >= 2 then WP.pokeCompactString p0 (P.toCompactString (deletableGroupResultGroupId msg)) else WP.pokeKafkaString p0 (deletableGroupResultGroupId msg))
  p2 <- W.pokeInt16BE p1 (deletableGroupResultErrorCode msg)
  if version >= 2 then WP.pokeEmptyTaggedFields p2 else pure p2

-- | Direct-poke decoder for DeletableGroupResult.
wirePeekDeletableGroupResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeletableGroupResult, Ptr Word8)
wirePeekDeletableGroupResult version _fp _basePtr p0 endPtr = do
  (f0_groupid, p1) <- (if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr else WP.peekKafkaString p0 endPtr)
  (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
  pTagsEnd <- if version >= 2 then WP.peekAndSkipTaggedFields p2 endPtr else pure p2
  pure (DeletableGroupResult { deletableGroupResultGroupId = f0_groupid, deletableGroupResultErrorCode = f1_errorcode }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDeletableGroupResult :: DeletableGroupResult
defaultDeletableGroupResult = DeletableGroupResult { deletableGroupResultGroupId = P.KafkaString Null, deletableGroupResultErrorCode = 0 }

-- | Worst-case wire size of a DeleteGroupsResponse.
wireMaxSizeDeleteGroupsResponse :: Int -> DeleteGroupsResponse -> Int
wireMaxSizeDeleteGroupsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (deleteGroupsResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDeletableGroupResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteGroupsResponse.
wirePokeDeleteGroupsResponse :: Int -> Ptr Word8 -> DeleteGroupsResponse -> IO (Ptr Word8)
wirePokeDeleteGroupsResponse version basePtr msg
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (deleteGroupsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDeletableGroupResult version p x) p1 (deleteGroupsResponseResults msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (deleteGroupsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 2 (\p x -> wirePokeDeletableGroupResult version p x) p1 (deleteGroupsResponseResults msg)
    pure p2
  | otherwise = error $ "wirePoke DeleteGroupsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteGroupsResponse.
wirePeekDeleteGroupsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteGroupsResponse, Ptr Word8)
wirePeekDeleteGroupsResponse version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_results, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDeletableGroupResult version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DeleteGroupsResponse { deleteGroupsResponseThrottleTimeMs = f0_throttletimems, deleteGroupsResponseResults = f1_results }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_results, p2) <- WP.peekVersionedArray version 2 (\p e -> wirePeekDeletableGroupResult version _fp _basePtr p e) p1 endPtr
    pure (DeleteGroupsResponse { deleteGroupsResponseThrottleTimeMs = f0_throttletimems, deleteGroupsResponseResults = f1_results }, p2)
  | otherwise = error $ "wirePeek DeleteGroupsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DeleteGroupsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteGroupsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteGroupsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteGroupsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}