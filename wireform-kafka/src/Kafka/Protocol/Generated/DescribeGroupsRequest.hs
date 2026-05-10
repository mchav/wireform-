{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeGroupsRequest
Description : Kafka DescribeGroupsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 15.



Valid versions: 0-6
Flexible versions: 5+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeGroupsRequest
  (
    DescribeGroupsRequest(..),
    maxDescribeGroupsRequestVersion
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




data DescribeGroupsRequest = DescribeGroupsRequest
  {

  -- | The names of the groups to describe.

  -- Versions: 0+
  describeGroupsRequestGroups :: !(KafkaArray (KafkaString))
,

  -- | Whether to include authorized operations.

  -- Versions: 3+
  describeGroupsRequestIncludeAuthorizedOperations :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeGroupsRequest.
maxDescribeGroupsRequestVersion :: Int16
maxDescribeGroupsRequestVersion = 6

-- | KafkaMessage instance for DescribeGroupsRequest.
instance KafkaMessage DescribeGroupsRequest where
  messageApiKey = 15
  messageMinVersion = 0
  messageMaxVersion = 6
  messageFlexibleVersion = Just 5


-- | Worst-case wire size of a DescribeGroupsRequest.
wireMaxSizeDescribeGroupsRequest :: Int -> DescribeGroupsRequest -> Int
wireMaxSizeDescribeGroupsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (describeGroupsRequestGroups msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + 1
  + 1

-- | Direct-poke encoder for DescribeGroupsRequest.
wirePokeDescribeGroupsRequest :: Int -> Ptr Word8 -> DescribeGroupsRequest -> IO (Ptr Word8)
wirePokeDescribeGroupsRequest version basePtr msg
  | version >= 3 && version <= 4 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 5 (\p s -> if version >= 5 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (describeGroupsRequestGroups msg)
    p2 <- (if version >= 3 then W.pokeWord8 p1 (if (describeGroupsRequestIncludeAuthorizedOperations msg) then 1 else 0) else pure p1)
    pure p2
  | version >= 5 && version <= 6 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 5 (\p s -> if version >= 5 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (describeGroupsRequestGroups msg)
    p2 <- (if version >= 3 then W.pokeWord8 p1 (if (describeGroupsRequestIncludeAuthorizedOperations msg) then 1 else 0) else pure p1)
    WP.pokeEmptyTaggedFields p2
  | version >= 0 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 5 (\p s -> if version >= 5 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (describeGroupsRequestGroups msg)
    pure p1
  | otherwise = error $ "wirePoke DescribeGroupsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeGroupsRequest.
wirePeekDescribeGroupsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeGroupsRequest, Ptr Word8)
wirePeekDescribeGroupsRequest version _fp _basePtr p0 endPtr
  | version >= 3 && version <= 4 = do
    (f0_groups, p1) <- WP.peekVersionedArray version 5 (\p e -> if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    (f1_includeauthorizedoperations, p2) <- (if version >= 3 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr else pure (False, p1))
    pure (DescribeGroupsRequest { describeGroupsRequestGroups = f0_groups, describeGroupsRequestIncludeAuthorizedOperations = f1_includeauthorizedoperations }, p2)
  | version >= 5 && version <= 6 = do
    (f0_groups, p1) <- WP.peekVersionedArray version 5 (\p e -> if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    (f1_includeauthorizedoperations, p2) <- (if version >= 3 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p1 endPtr else pure (False, p1))
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DescribeGroupsRequest { describeGroupsRequestGroups = f0_groups, describeGroupsRequestIncludeAuthorizedOperations = f1_includeauthorizedoperations }, pTagsEnd)
  | version >= 0 && version <= 2 = do
    (f0_groups, p1) <- WP.peekVersionedArray version 5 (\p e -> if version >= 5 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    pure (DescribeGroupsRequest { describeGroupsRequestGroups = f0_groups, describeGroupsRequestIncludeAuthorizedOperations = False }, p1)
  | otherwise = error $ "wirePeek DescribeGroupsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeGroupsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeGroupsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeGroupsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeGroupsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}