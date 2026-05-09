{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeAclsRequest
Description : Kafka DescribeAclsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 29.



Valid versions: 1-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeAclsRequest
  (
    DescribeAclsRequest(..),
    maxDescribeAclsRequestVersion
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




data DescribeAclsRequest = DescribeAclsRequest
  {

  -- | The resource type.

  -- Versions: 0+
  describeAclsRequestResourceTypeFilter :: !(Int8)
,

  -- | The resource name, or null to match any resource name.

  -- Versions: 0+
  describeAclsRequestResourceNameFilter :: !(KafkaString)
,

  -- | The resource pattern to match.

  -- Versions: 1+
  describeAclsRequestPatternTypeFilter :: !(Int8)
,

  -- | The principal to match, or null to match any principal.

  -- Versions: 0+
  describeAclsRequestPrincipalFilter :: !(KafkaString)
,

  -- | The host to match, or null to match any host.

  -- Versions: 0+
  describeAclsRequestHostFilter :: !(KafkaString)
,

  -- | The operation to match.

  -- Versions: 0+
  describeAclsRequestOperation :: !(Int8)
,

  -- | The permission type to match.

  -- Versions: 0+
  describeAclsRequestPermissionType :: !(Int8)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeAclsRequest.
maxDescribeAclsRequestVersion :: Int16
maxDescribeAclsRequestVersion = 3

-- | KafkaMessage instance for DescribeAclsRequest.
instance KafkaMessage DescribeAclsRequest where
  messageApiKey = 29
  messageMinVersion = 1
  messageMaxVersion = 3
  messageFlexibleVersion = Just 2


-- | Worst-case wire size of a DescribeAclsRequest.
wireMaxSizeDescribeAclsRequest :: Int -> DescribeAclsRequest -> Int
wireMaxSizeDescribeAclsRequest _version msg =
  0
  + 1
  + WP.compactStringMaxSize (P.toCompactString (describeAclsRequestResourceNameFilter msg))
  + 1
  + WP.compactStringMaxSize (P.toCompactString (describeAclsRequestPrincipalFilter msg))
  + WP.compactStringMaxSize (P.toCompactString (describeAclsRequestHostFilter msg))
  + 1
  + 1
  + 1

-- | Direct-poke encoder for DescribeAclsRequest.
wirePokeDescribeAclsRequest :: Int -> Ptr Word8 -> DescribeAclsRequest -> IO (Ptr Word8)
wirePokeDescribeAclsRequest version basePtr msg
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- W.pokeWord8 p0 (fromIntegral (describeAclsRequestResourceTypeFilter msg))
    p2 <- WP.pokeCompactString p1 (P.toCompactString (describeAclsRequestResourceNameFilter msg))
    p3 <- W.pokeWord8 p2 (fromIntegral (describeAclsRequestPatternTypeFilter msg))
    p4 <- WP.pokeCompactString p3 (P.toCompactString (describeAclsRequestPrincipalFilter msg))
    p5 <- WP.pokeCompactString p4 (P.toCompactString (describeAclsRequestHostFilter msg))
    p6 <- W.pokeWord8 p5 (fromIntegral (describeAclsRequestOperation msg))
    p7 <- W.pokeWord8 p6 (fromIntegral (describeAclsRequestPermissionType msg))
    pure p7
  | version >= 2 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeWord8 p0 (fromIntegral (describeAclsRequestResourceTypeFilter msg))
    p2 <- WP.pokeCompactString p1 (P.toCompactString (describeAclsRequestResourceNameFilter msg))
    p3 <- W.pokeWord8 p2 (fromIntegral (describeAclsRequestPatternTypeFilter msg))
    p4 <- WP.pokeCompactString p3 (P.toCompactString (describeAclsRequestPrincipalFilter msg))
    p5 <- WP.pokeCompactString p4 (P.toCompactString (describeAclsRequestHostFilter msg))
    p6 <- W.pokeWord8 p5 (fromIntegral (describeAclsRequestOperation msg))
    p7 <- W.pokeWord8 p6 (fromIntegral (describeAclsRequestPermissionType msg))
    WP.pokeEmptyTaggedFields p7
  | otherwise = error $ "wirePoke DescribeAclsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeAclsRequest.
wirePeekDescribeAclsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeAclsRequest, Ptr Word8)
wirePeekDescribeAclsRequest version _fp _basePtr p0 endPtr
  | version == 1 = do
    (f0_resourcetypefilter, p1) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p0 endPtr
    (f1_resourcenamefilter, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_patterntypefilter, p3) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p2 endPtr
    (f3_principalfilter, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
    (f4_hostfilter, p5) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr
    (f5_operation, p6) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p5 endPtr
    (f6_permissiontype, p7) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p6 endPtr
    pure (DescribeAclsRequest { describeAclsRequestResourceTypeFilter = f0_resourcetypefilter, describeAclsRequestResourceNameFilter = f1_resourcenamefilter, describeAclsRequestPatternTypeFilter = f2_patterntypefilter, describeAclsRequestPrincipalFilter = f3_principalfilter, describeAclsRequestHostFilter = f4_hostfilter, describeAclsRequestOperation = f5_operation, describeAclsRequestPermissionType = f6_permissiontype }, p7)
  | version >= 2 && version <= 3 = do
    (f0_resourcetypefilter, p1) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p0 endPtr
    (f1_resourcenamefilter, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
    (f2_patterntypefilter, p3) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p2 endPtr
    (f3_principalfilter, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
    (f4_hostfilter, p5) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p4 endPtr
    (f5_operation, p6) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p5 endPtr
    (f6_permissiontype, p7) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p6 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p7 endPtr
    pure (DescribeAclsRequest { describeAclsRequestResourceTypeFilter = f0_resourcetypefilter, describeAclsRequestResourceNameFilter = f1_resourcenamefilter, describeAclsRequestPatternTypeFilter = f2_patterntypefilter, describeAclsRequestPrincipalFilter = f3_principalfilter, describeAclsRequestHostFilter = f4_hostfilter, describeAclsRequestOperation = f5_operation, describeAclsRequestPermissionType = f6_permissiontype }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeAclsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeAclsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeAclsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeAclsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeAclsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}