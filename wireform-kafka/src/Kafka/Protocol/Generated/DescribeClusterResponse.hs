{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeClusterResponse
Description : Kafka DescribeClusterResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 60.



Valid versions: 0-2
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeClusterResponse
  (
    DescribeClusterResponse(..),
    DescribeClusterBroker(..),
    maxDescribeClusterResponseVersion
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


-- | Each broker in the response.
data DescribeClusterBroker = DescribeClusterBroker
  {

  -- | The broker ID.

  -- Versions: 0+
  describeClusterBrokerBrokerId :: !(Int32)
,

  -- | The broker hostname.

  -- Versions: 0+
  describeClusterBrokerHost :: !(KafkaString)
,

  -- | The broker port.

  -- Versions: 0+
  describeClusterBrokerPort :: !(Int32)
,

  -- | The rack of the broker, or null if it has not been assigned to a rack.

  -- Versions: 0+
  describeClusterBrokerRack :: !(KafkaString)
,

  -- | Whether the broker is fenced

  -- Versions: 2+
  describeClusterBrokerIsFenced :: !(Bool)

  }
  deriving (Eq, Show, Generic)


data DescribeClusterResponse = DescribeClusterResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeClusterResponseThrottleTimeMs :: !(Int32)
,

  -- | The top-level error code, or 0 if there was no error.

  -- Versions: 0+
  describeClusterResponseErrorCode :: !(Int16)
,

  -- | The top-level error message, or null if there was no error.

  -- Versions: 0+
  describeClusterResponseErrorMessage :: !(KafkaString)
,

  -- | The endpoint type that was described. 1=brokers, 2=controllers.

  -- Versions: 1+
  describeClusterResponseEndpointType :: !(Int8)
,

  -- | The cluster ID that responding broker belongs to.

  -- Versions: 0+
  describeClusterResponseClusterId :: !(KafkaString)
,

  -- | The ID of the controller. When handled by a controller, returns the current voter leader ID. When ha

  -- Versions: 0+
  describeClusterResponseControllerId :: !(Int32)
,

  -- | Each broker in the response.

  -- Versions: 0+
  describeClusterResponseBrokers :: !(KafkaArray (DescribeClusterBroker))
,

  -- | 32-bit bitfield to represent authorized operations for this cluster.

  -- Versions: 0+
  describeClusterResponseClusterAuthorizedOperations :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeClusterResponse.
maxDescribeClusterResponseVersion :: Int16
maxDescribeClusterResponseVersion = 2

-- | KafkaMessage instance for DescribeClusterResponse.
instance KafkaMessage DescribeClusterResponse where
  messageApiKey = 60
  messageMinVersion = 0
  messageMaxVersion = 2
  messageFlexibleVersion = Just 0

-- | Worst-case wire size of a DescribeClusterBroker.
wireMaxSizeDescribeClusterBroker :: Int -> DescribeClusterBroker -> Int
wireMaxSizeDescribeClusterBroker _version msg =
  0
  + 4
  + WP.dualStringMaxSize (describeClusterBrokerHost msg)
  + 4
  + WP.dualStringMaxSize (describeClusterBrokerRack msg)
  + 1
  + 1

-- | Direct-poke encoder for DescribeClusterBroker.
wirePokeDescribeClusterBroker :: Int -> Ptr Word8 -> DescribeClusterBroker -> IO (Ptr Word8)
wirePokeDescribeClusterBroker version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt32BE p0 (describeClusterBrokerBrokerId msg)
  p2 <- (if version >= 0 then WP.pokeCompactString p1 (P.toCompactString (describeClusterBrokerHost msg)) else WP.pokeKafkaString p1 (describeClusterBrokerHost msg))
  p3 <- W.pokeInt32BE p2 (describeClusterBrokerPort msg)
  p4 <- (if version >= 0 then WP.pokeCompactString p3 (P.toCompactString (describeClusterBrokerRack msg)) else WP.pokeKafkaString p3 (describeClusterBrokerRack msg))
  p5 <- (if version >= 2 then W.pokeWord8 p4 (if (describeClusterBrokerIsFenced msg) then 1 else 0) else pure p4)
  if version >= 0 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for DescribeClusterBroker.
wirePeekDescribeClusterBroker :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeClusterBroker, Ptr Word8)
wirePeekDescribeClusterBroker version _fp _basePtr p0 endPtr = do
  (f0_brokerid, p1) <- W.peekInt32BE p0 endPtr
  (f1_host, p2) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr else WP.peekKafkaString p1 endPtr)
  (f2_port, p3) <- W.peekInt32BE p2 endPtr
  (f3_rack, p4) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
  (f4_isfenced, p5) <- (if version >= 2 then (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p4 endPtr else pure (False, p4))
  pTagsEnd <- if version >= 0 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (DescribeClusterBroker { describeClusterBrokerBrokerId = f0_brokerid, describeClusterBrokerHost = f1_host, describeClusterBrokerPort = f2_port, describeClusterBrokerRack = f3_rack, describeClusterBrokerIsFenced = f4_isfenced }, pTagsEnd)

-- | Per-struct default value referenced by 'generateFieldDefaultDoc'
-- when an absent-version field elsewhere needs a placeholder.
defaultDescribeClusterBroker :: DescribeClusterBroker
defaultDescribeClusterBroker = DescribeClusterBroker { describeClusterBrokerBrokerId = 0, describeClusterBrokerHost = P.KafkaString Null, describeClusterBrokerPort = 0, describeClusterBrokerRack = P.KafkaString Null, describeClusterBrokerIsFenced = False }

-- | Worst-case wire size of a DescribeClusterResponse.
wireMaxSizeDescribeClusterResponse :: Int -> DescribeClusterResponse -> Int
wireMaxSizeDescribeClusterResponse _version msg =
  0
  + 4
  + 2
  + WP.dualStringMaxSize (describeClusterResponseErrorMessage msg)
  + 1
  + WP.dualStringMaxSize (describeClusterResponseClusterId msg)
  + 4
  + (5 + (case P.unKafkaArray (describeClusterResponseBrokers msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeClusterBroker _version x ) v); P.Null -> 0 }))
  + 4
  + 1

-- | Direct-poke encoder for DescribeClusterResponse.
wirePokeDescribeClusterResponse :: Int -> Ptr Word8 -> DescribeClusterResponse -> IO (Ptr Word8)
wirePokeDescribeClusterResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeClusterResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (describeClusterResponseErrorCode msg)
    p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (describeClusterResponseErrorMessage msg)) else WP.pokeKafkaString p2 (describeClusterResponseErrorMessage msg))
    p4 <- (if version >= 0 then WP.pokeCompactString p3 (P.toCompactString (describeClusterResponseClusterId msg)) else WP.pokeKafkaString p3 (describeClusterResponseClusterId msg))
    p5 <- W.pokeInt32BE p4 (describeClusterResponseControllerId msg)
    p6 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDescribeClusterBroker version p x) p5 (describeClusterResponseBrokers msg)
    p7 <- W.pokeInt32BE p6 (describeClusterResponseClusterAuthorizedOperations msg)
    WP.pokeEmptyTaggedFields p7
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeClusterResponseThrottleTimeMs msg)
    p2 <- W.pokeInt16BE p1 (describeClusterResponseErrorCode msg)
    p3 <- (if version >= 0 then WP.pokeCompactString p2 (P.toCompactString (describeClusterResponseErrorMessage msg)) else WP.pokeKafkaString p2 (describeClusterResponseErrorMessage msg))
    p4 <- (if version >= 1 then W.pokeWord8 p3 (fromIntegral (describeClusterResponseEndpointType msg)) else pure p3)
    p5 <- (if version >= 0 then WP.pokeCompactString p4 (P.toCompactString (describeClusterResponseClusterId msg)) else WP.pokeKafkaString p4 (describeClusterResponseClusterId msg))
    p6 <- W.pokeInt32BE p5 (describeClusterResponseControllerId msg)
    p7 <- WP.pokeVersionedArray version 0 (\p x -> wirePokeDescribeClusterBroker version p x) p6 (describeClusterResponseBrokers msg)
    p8 <- W.pokeInt32BE p7 (describeClusterResponseClusterAuthorizedOperations msg)
    WP.pokeEmptyTaggedFields p8
  | otherwise = error $ "wirePoke DescribeClusterResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeClusterResponse.
wirePeekDescribeClusterResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeClusterResponse, Ptr Word8)
wirePeekDescribeClusterResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_clusterid, p4) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr else WP.peekKafkaString p3 endPtr)
    (f4_controllerid, p5) <- W.peekInt32BE p4 endPtr
    (f5_brokers, p6) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDescribeClusterBroker version _fp _basePtr p e) p5 endPtr
    (f6_clusterauthorizedoperations, p7) <- W.peekInt32BE p6 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p7 endPtr
    pure (DescribeClusterResponse { describeClusterResponseThrottleTimeMs = f0_throttletimems, describeClusterResponseErrorCode = f1_errorcode, describeClusterResponseErrorMessage = f2_errormessage, describeClusterResponseEndpointType = 0, describeClusterResponseClusterId = f3_clusterid, describeClusterResponseControllerId = f4_controllerid, describeClusterResponseBrokers = f5_brokers, describeClusterResponseClusterAuthorizedOperations = f6_clusterauthorizedoperations }, pTagsEnd)
  | version >= 1 && version <= 2 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_errorcode, p2) <- W.peekInt16BE p1 endPtr
    (f2_errormessage, p3) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p2 endPtr else WP.peekKafkaString p2 endPtr)
    (f3_endpointtype, p4) <- (if version >= 1 then (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p3 endPtr else pure (0, p3))
    (f4_clusterid, p5) <- (if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr else WP.peekKafkaString p4 endPtr)
    (f5_controllerid, p6) <- W.peekInt32BE p5 endPtr
    (f6_brokers, p7) <- WP.peekVersionedArray version 0 (\p e -> wirePeekDescribeClusterBroker version _fp _basePtr p e) p6 endPtr
    (f7_clusterauthorizedoperations, p8) <- W.peekInt32BE p7 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p8 endPtr
    pure (DescribeClusterResponse { describeClusterResponseThrottleTimeMs = f0_throttletimems, describeClusterResponseErrorCode = f1_errorcode, describeClusterResponseErrorMessage = f2_errormessage, describeClusterResponseEndpointType = f3_endpointtype, describeClusterResponseClusterId = f4_clusterid, describeClusterResponseControllerId = f5_controllerid, describeClusterResponseBrokers = f6_brokers, describeClusterResponseClusterAuthorizedOperations = f7_clusterauthorizedoperations }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeClusterResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeClusterResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeClusterResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeClusterResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeClusterResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}