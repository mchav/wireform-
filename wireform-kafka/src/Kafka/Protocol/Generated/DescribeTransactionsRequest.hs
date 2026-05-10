{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeTransactionsRequest
Description : Kafka DescribeTransactionsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 65.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeTransactionsRequest
  (
    DescribeTransactionsRequest(..),
    maxDescribeTransactionsRequestVersion
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




data DescribeTransactionsRequest = DescribeTransactionsRequest
  {

  -- | Array of transactionalIds to include in describe results. If empty, then no results will be returned

  -- Versions: 0+
  describeTransactionsRequestTransactionalIds :: !(KafkaArray (KafkaString))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeTransactionsRequest.
maxDescribeTransactionsRequestVersion :: Int16
maxDescribeTransactionsRequestVersion = 0

-- | KafkaMessage instance for DescribeTransactionsRequest.
instance KafkaMessage DescribeTransactionsRequest where
  messageApiKey = 65
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a DescribeTransactionsRequest.
wireMaxSizeDescribeTransactionsRequest :: Int -> DescribeTransactionsRequest -> Int
wireMaxSizeDescribeTransactionsRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (describeTransactionsRequestTransactionalIds msg) of { P.NotNull v -> sum (fmap (\x -> WP.compactStringMaxSize (P.toCompactString x) ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeTransactionsRequest.
wirePokeDescribeTransactionsRequest :: Int -> Ptr Word8 -> DescribeTransactionsRequest -> IO (Ptr Word8)
wirePokeDescribeTransactionsRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeVersionedArray version 0 (\p s -> if version >= 0 then WP.pokeCompactString p (P.toCompactString s) else WP.pokeKafkaString p s) p0 (describeTransactionsRequestTransactionalIds msg)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke DescribeTransactionsRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeTransactionsRequest.
wirePeekDescribeTransactionsRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeTransactionsRequest, Ptr Word8)
wirePeekDescribeTransactionsRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_transactionalids, p1) <- WP.peekVersionedArray version 0 (\p e -> if version >= 0 then (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p e else WP.peekKafkaString p e) p0 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (DescribeTransactionsRequest { describeTransactionsRequestTransactionalIds = f0_transactionalids }, pTagsEnd)
  | otherwise = error $ "wirePeek DescribeTransactionsRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeTransactionsRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeTransactionsRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeTransactionsRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeTransactionsRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}