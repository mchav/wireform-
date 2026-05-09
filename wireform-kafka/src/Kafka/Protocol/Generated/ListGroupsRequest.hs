{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListGroupsRequest
Description : Kafka ListGroupsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 16.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListGroupsRequest
  (
    ListGroupsRequest(..),
    maxListGroupsRequestVersion
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




data ListGroupsRequest = ListGroupsRequest
  {

  -- | The states of the groups we want to list. If empty, all groups are returned with their state.

  -- Versions: 4+
  listGroupsRequestStatesFilter :: !(KafkaArray (KafkaString))
,

  -- | The types of the groups we want to list. If empty, all groups are returned with their type.

  -- Versions: 5+
  listGroupsRequestTypesFilter :: !(KafkaArray (KafkaString))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListGroupsRequest.
maxListGroupsRequestVersion :: Int16
maxListGroupsRequestVersion = 5

-- | KafkaMessage instance for ListGroupsRequest.
instance KafkaMessage ListGroupsRequest where
  messageApiKey = 16
  messageMinVersion = 0
  messageMaxVersion = 5
  messageFlexibleVersion = Just 3


-- | Worst-case wire size of a ListGroupsRequest.
wireMaxSizeListGroupsRequest :: Int -> ListGroupsRequest -> Int
wireMaxSizeListGroupsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (listGroupsRequestStatesFilter msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + (5 + (case P.unKafkaArray (listGroupsRequestTypesFilter msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ListGroupsRequest.
wirePokeListGroupsRequest :: Int -> Ptr Word8 -> ListGroupsRequest -> IO (Ptr Word8)
wirePokeListGroupsRequest version basePtr msg
  | version == 3 = do
    p0 <- pure basePtr
    WP.pokeEmptyTaggedFields p0
  | version == 4 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 3 (\p s -> if version >= 3 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (listGroupsRequestStatesFilter msg)
    WP.pokeEmptyTaggedFields p1
  | version == 5 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 3 (\p s -> if version >= 3 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (listGroupsRequestStatesFilter msg)
    p2 <- WP.pokeVersionedArray version 3 (\p s -> if version >= 3 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p1 (listGroupsRequestTypesFilter msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    pure p0
  | otherwise = error $ "wirePoke ListGroupsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ListGroupsRequest.
wirePeekListGroupsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListGroupsRequest, Ptr Word8)
wirePeekListGroupsRequest version _fp _basePtr p0 endPtr
  | version == 3 = do
    pTagsEnd <- WP.peekAndSkipTaggedFields p0 endPtr
    pure (ListGroupsRequest { listGroupsRequestStatesFilter = P.mkKafkaArray V.empty, listGroupsRequestTypesFilter = P.mkKafkaArray V.empty }, pTagsEnd)
  | version == 4 = do
    (f0_statesfilter, p1) <- WP.peekVersionedArray version 3 (\p e -> if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (ListGroupsRequest { listGroupsRequestStatesFilter = f0_statesfilter, listGroupsRequestTypesFilter = P.mkKafkaArray V.empty }, pTagsEnd)
  | version == 5 = do
    (f0_statesfilter, p1) <- WP.peekVersionedArray version 3 (\p e -> if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    (f1_typesfilter, p2) <- WP.peekVersionedArray version 3 (\p e -> if version >= 3 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (ListGroupsRequest { listGroupsRequestStatesFilter = f0_statesfilter, listGroupsRequestTypesFilter = f1_typesfilter }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    pure (ListGroupsRequest { listGroupsRequestStatesFilter = P.mkKafkaArray V.empty, listGroupsRequestTypesFilter = P.mkKafkaArray V.empty }, p0)
  | otherwise = error $ "wirePeek ListGroupsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ListGroupsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeListGroupsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeListGroupsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekListGroupsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}