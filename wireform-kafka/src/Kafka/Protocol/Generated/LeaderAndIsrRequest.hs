{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.LeaderAndIsrRequest
Description : Kafka LeaderAndIsrRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 4.



Valid versions: none
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.LeaderAndIsrRequest
  (
    LeaderAndIsrRequest(..),
    encodeLeaderAndIsrRequest,
    decodeLeaderAndIsrRequest,
    maxLeaderAndIsrRequestVersion
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




data LeaderAndIsrRequest = LeaderAndIsrRequest
  {

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for LeaderAndIsrRequest.
maxLeaderAndIsrRequestVersion :: Int16
maxLeaderAndIsrRequestVersion = -1 -- No valid versions

-- | Encode LeaderAndIsrRequest with the given API version.
encodeLeaderAndIsrRequest :: MonadPut m => E.ApiVersion -> LeaderAndIsrRequest -> m ()
encodeLeaderAndIsrRequest version msg
  = error "No valid versions"


-- | Decode LeaderAndIsrRequest with the given API version.
decodeLeaderAndIsrRequest :: MonadGet m => E.ApiVersion -> m LeaderAndIsrRequest
decodeLeaderAndIsrRequest version
  = fail "No valid versions"

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeLeaderAndIsrRequest' / 'decodeLeaderAndIsrRequest' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec LeaderAndIsrRequest where
  wireCodec = Just (WC.serialShimCodec encodeLeaderAndIsrRequest decodeLeaderAndIsrRequest)
  {-# INLINE wireCodec #-}
