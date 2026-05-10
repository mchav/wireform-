{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListConfigResourcesRequest
Description : Kafka ListConfigResourcesRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 74.



Valid versions: 0-1
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListConfigResourcesRequest
  (
    ListConfigResourcesRequest(..),
    maxListConfigResourcesRequestVersion
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




data ListConfigResourcesRequest = ListConfigResourcesRequest
  {

  -- | The list of resource type. If the list is empty, it uses default supported config resource types.

  -- Versions: 1+
  listConfigResourcesRequestResourceTypes :: !(KafkaArray (Int8))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListConfigResourcesRequest.
maxListConfigResourcesRequestVersion :: Int16
maxListConfigResourcesRequestVersion = 1

-- | KafkaMessage instance for ListConfigResourcesRequest.
instance KafkaMessage ListConfigResourcesRequest where
  messageApiKey = 74
  messageMinVersion = 0
  messageMaxVersion = 1
  messageFlexibleVersion = Just 0


-- | Worst-case wire size of a ListConfigResourcesRequest.
wireMaxSizeListConfigResourcesRequest :: Int -> ListConfigResourcesRequest -> Int
wireMaxSizeListConfigResourcesRequest _version msg =
  0
  + (5 + (case P.unKafkaArray (listConfigResourcesRequestResourceTypes msg) of { P.NotNull v -> sum (fmap (\x -> 1 ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for ListConfigResourcesRequest.
wirePokeListConfigResourcesRequest :: Int -> Ptr Word8 -> ListConfigResourcesRequest -> IO (Ptr Word8)
wirePokeListConfigResourcesRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    WP.pokeEmptyTaggedFields p0
  | version == 1 = do
    p0 <- pure basePtr
    p1 <- (if version >= 1 then WP.pokeVersionedArray version 0 (\p x -> W.pokeWord8 p (fromIntegral (x :: Int8))) p0 (listConfigResourcesRequestResourceTypes msg) else pure p0)
    WP.pokeEmptyTaggedFields p1
  | otherwise = error $ "wirePoke ListConfigResourcesRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for ListConfigResourcesRequest.
wirePeekListConfigResourcesRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ListConfigResourcesRequest, Ptr Word8)
wirePeekListConfigResourcesRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    pTagsEnd <- WP.peekAndSkipTaggedFields p0 endPtr
    pure (ListConfigResourcesRequest { listConfigResourcesRequestResourceTypes = P.mkKafkaArray V.empty }, pTagsEnd)
  | version == 1 = do
    (f0_resourcetypes, p1) <- (if version >= 1 then WP.peekVersionedArray version 0 (\p e -> (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p e) p0 endPtr else pure (P.mkKafkaArray V.empty, p0))
    pTagsEnd <- WP.peekAndSkipTaggedFields p1 endPtr
    pure (ListConfigResourcesRequest { listConfigResourcesRequestResourceTypes = f0_resourcetypes }, pTagsEnd)
  | otherwise = error $ "wirePeek ListConfigResourcesRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ListConfigResourcesRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeListConfigResourcesRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeListConfigResourcesRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekListConfigResourcesRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}