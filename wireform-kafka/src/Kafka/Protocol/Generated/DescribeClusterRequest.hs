{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeClusterRequest
Description : Kafka DescribeClusterRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 60.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeClusterRequest
  (
    DescribeClusterRequest(..),
    maxDescribeClusterRequestVersion
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




data DescribeClusterRequest = DescribeClusterRequest
  {

  -- | Whether to include cluster authorized operations.

  -- Versions: 0+
  describeClusterRequestIncludeClusterAuthorizedOperations :: !(Bool)
,

  -- | The endpoint type to describe. 1=brokers, 2=controllers.

  -- Versions: 1+
  describeClusterRequestEndpointType :: !(Int8)
,

  -- | Whether to include fenced brokers when listing brokers.

  -- Versions: 2+
  describeClusterRequestIncludeFencedBrokers :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeClusterRequest.
maxDescribeClusterRequestVersion :: Int16
maxDescribeClusterRequestVersion = 2

-- | KafkaMessage instance for DescribeClusterRequest.
instance KafkaMessage DescribeClusterRequest where
  messageApiKey = 60
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a DescribeClusterRequest.
wireMaxSizeDescribeClusterRequest :: Int -> DescribeClusterRequest -> Int
wireMaxSizeDescribeClusterRequest _version msg =
  0
  + 1
  + 1
  + 1
  + 1

-- | Direct-poke encoder for DescribeClusterRequest.
wirePokeDescribeClusterRequest :: Int -> Ptr Word8 -> DescribeClusterRequest -> IO (Ptr Word8)
wirePokeDescribeClusterRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeWord8 p0 (if (describeClusterRequestIncludeClusterAuthorizedOperations msg) then 1 else 0)
    WP.pokeEmptyTaggedFields p1
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeWord8 p0 (if (describeClusterRequestIncludeClusterAuthorizedOperations msg) then 1 else 0)
    p2 <- (if version >= 1 then W.pokeWord8 p1 (fromIntegral (describeClusterRequestEndpointType msg)) else pure p1)
    WP.pokeEmptyTaggedFields p2
  | version == 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeWord8 p0 (if (describeClusterRequestIncludeClusterAuthorizedOperations msg) then 1 else 0)
    p2 <- (if version >= 1 then W.pokeWord8 p1 (fromIntegral (describeClusterRequestEndpointType msg)) else pure p1)
    p3 <- (if version >= 2 then W.pokeWord8 p2 (if (describeClusterRequestIncludeFencedBrokers msg) then 1 else 0) else pure p2)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke DescribeClusterRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeClusterRequest.
wirePeekDescribeClusterRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeClusterRequest, Ptr Word8)
wirePeekDescribeClusterRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_includeclusterauthorizedoperations, p1) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (DescribeClusterRequest { describeClusterRequestIncludeClusterAuthorizedOperations = f0_includeclusterauthorizedoperations, describeClusterRequestEndpointType = 0, describeClusterRequestIncludeFencedBrokers = False }, pTagsEnd)
  | version == 1 = do
    (f0_includeclusterauthorizedoperations, p1) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p0 endPtr
    (f1_endpointtype, p2) <- (if version >= 1 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p1 endPtr else pure (0, p1))
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DescribeClusterRequest { describeClusterRequestIncludeClusterAuthorizedOperations = f0_includeclusterauthorizedoperations, describeClusterRequestEndpointType = f1_endpointtype, describeClusterRequestIncludeFencedBrokers = False }, pTagsEnd)
  | version == 2 = do
    (f0_includeclusterauthorizedoperations, p1) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p0 endPtr
    (f1_endpointtype, p2) <- (if version >= 1 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p1 endPtr else pure (0, p1))
    (f2_includefencedbrokers, p3) <- (if version >= 2 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr else pure (False, p2))
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (DescribeClusterRequest { describeClusterRequestIncludeClusterAuthorizedOperations = f0_includeclusterauthorizedoperations, describeClusterRequestEndpointType = f1_endpointtype, describeClusterRequestIncludeFencedBrokers = f2_includefencedbrokers }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeClusterRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeClusterRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeClusterRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeClusterRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeClusterRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}