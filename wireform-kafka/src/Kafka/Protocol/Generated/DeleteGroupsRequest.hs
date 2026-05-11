{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DeleteGroupsRequest
Description : Kafka DeleteGroupsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 42.



Valid versions: 0-2
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DeleteGroupsRequest
  (
    DeleteGroupsRequest(..),
    maxDeleteGroupsRequestVersion
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




data DeleteGroupsRequest = DeleteGroupsRequest
  {

  -- | The group names to delete.

  -- Versions: 0+
  deleteGroupsRequestGroupsNames :: !(KafkaArray (KafkaString))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DeleteGroupsRequest.
maxDeleteGroupsRequestVersion :: Int16
maxDeleteGroupsRequestVersion = 2

-- | KafkaMessage instance for DeleteGroupsRequest.
instance KafkaMessage DeleteGroupsRequest where
  messageApiKey = 42
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 2


-- | Worst-case wire size of a DeleteGroupsRequest.
wireMaxSizeDeleteGroupsRequest :: Int -> DeleteGroupsRequest -> Int
wireMaxSizeDeleteGroupsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (deleteGroupsRequestGroupsNames msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DeleteGroupsRequest.
wirePokeDeleteGroupsRequest :: Int -> Ptr Word8 -> DeleteGroupsRequest -> IO (Ptr Word8)
wirePokeDeleteGroupsRequest version basePtr msg
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p s -> if version >= 2 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (deleteGroupsRequestGroupsNames msg)
    WP.pokeEmptyTaggedFields p1
  | version >= 0 && version <= 1 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 2 (\p s -> if version >= 2 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (deleteGroupsRequestGroupsNames msg)
    pure p1
  | otherwise = error $ "wirePoke DeleteGroupsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DeleteGroupsRequest.
wirePeekDeleteGroupsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DeleteGroupsRequest, Ptr Word8)
wirePeekDeleteGroupsRequest version _fp _basePtr p0 endPtr
  | version == 2 = do
    (f0_groupsnames, p1) <- WP.peekVersionedArray version 2 (\p e -> if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (DeleteGroupsRequest { deleteGroupsRequestGroupsNames = f0_groupsnames }, pTagsEnd)
  | version >= 0 && version <= 1 = do
    (f0_groupsnames, p1) <- WP.peekVersionedArray version 2 (\p e -> if version >= 2 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    pure (DeleteGroupsRequest { deleteGroupsRequestGroupsNames = f0_groupsnames }, p1)
  | otherwise = error $ "wirePeek DeleteGroupsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec DeleteGroupsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDeleteGroupsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDeleteGroupsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDeleteGroupsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}