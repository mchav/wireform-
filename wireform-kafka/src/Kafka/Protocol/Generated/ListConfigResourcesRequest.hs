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
    encodeListConfigResourcesRequest,
    decodeListConfigResourcesRequest,
    maxListConfigResourcesRequestVersion
  ) where

import Control.Monad (when)
import Data.Bytes.Get (MonadGet)
import Data.Bytes.Put (MonadPut)
import Data.Bytes.Serial (Serial(..), serialize, deserialize)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( VarInt(..), VarLong(..), UVarInt(..)
  , KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , CompactString, CompactBytes, CompactArray
  , TaggedFields, emptyTaggedFields, Nullable(..)
  , toCompactString, toCompactBytes, toCompactArray
  )
import qualified Kafka.Protocol.Encoding as E
import qualified Kafka.Protocol.Wire.Codec as WC




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

-- | Encode ListConfigResourcesRequest with the given API version.
encodeListConfigResourcesRequest :: MonadPut m => E.ApiVersion -> ListConfigResourcesRequest -> m ()
encodeListConfigResourcesRequest version msg
  | version == 0 =
    do
      
      serialize (emptyTaggedFields :: TaggedFields)

  | version == 1 =
    do
      E.encodeVersionedArray version 0 (\_ x -> serialize x) (case P.unKafkaArray (listConfigResourcesRequestResourceTypes msg) of { P.NotNull v -> v; P.Null -> V.empty }) -- ArrayType: PrimitiveType "int8"
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ListConfigResourcesRequest with the given API version.
decodeListConfigResourcesRequest :: MonadGet m => E.ApiVersion -> m ListConfigResourcesRequest
decodeListConfigResourcesRequest version
  | version == 0 =
    do
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListConfigResourcesRequest
        {
        listConfigResourcesRequestResourceTypes = P.mkKafkaArray V.empty
        }

  | version == 1 =
    do
      fieldresourcetypes <- P.mkKafkaArray <$> E.decodeVersionedArray version 0 (\_ -> deserialize)
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListConfigResourcesRequest
        {
        listConfigResourcesRequestResourceTypes = fieldresourcetypes
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeListConfigResourcesRequest' / 'decodeListConfigResourcesRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec ListConfigResourcesRequest where
  wireCodec = Just (WC.serialShimCodec encodeListConfigResourcesRequest decodeListConfigResourcesRequest)
  {-# INLINE wireCodec #-}
