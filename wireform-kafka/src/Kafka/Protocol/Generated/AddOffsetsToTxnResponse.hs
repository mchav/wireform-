{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AddOffsetsToTxnResponse
Description : Kafka AddOffsetsToTxnResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 25.



Valid versions: 0-4
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AddOffsetsToTxnResponse
  (
    AddOffsetsToTxnResponse(..),
    encodeAddOffsetsToTxnResponse,
    decodeAddOffsetsToTxnResponse,
    maxAddOffsetsToTxnResponseVersion
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




data AddOffsetsToTxnResponse = AddOffsetsToTxnResponse
  {

  -- | Duration in milliseconds for which the request was throttled due to a quota violation, or zero if th

  -- Versions: 0+
  addOffsetsToTxnResponseThrottleTimeMs :: !(Int32)
,

  -- | The response error code, or 0 if there was no error.

  -- Versions: 0+
  addOffsetsToTxnResponseErrorCode :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AddOffsetsToTxnResponse.
maxAddOffsetsToTxnResponseVersion :: Int16
maxAddOffsetsToTxnResponseVersion = 4

-- | Encode AddOffsetsToTxnResponse with the given API version.
encodeAddOffsetsToTxnResponse :: MonadPut m => E.ApiVersion -> AddOffsetsToTxnResponse -> m ()
encodeAddOffsetsToTxnResponse version msg
  | version >= 3 && version <= 4 =
    do
      serialize (addOffsetsToTxnResponseThrottleTimeMs msg)
      serialize (addOffsetsToTxnResponseErrorCode msg)
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 0 && version <= 2 =
    do
      serialize (addOffsetsToTxnResponseThrottleTimeMs msg)
      serialize (addOffsetsToTxnResponseErrorCode msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode AddOffsetsToTxnResponse with the given API version.
decodeAddOffsetsToTxnResponse :: MonadGet m => E.ApiVersion -> m AddOffsetsToTxnResponse
decodeAddOffsetsToTxnResponse version
  | version >= 3 && version <= 4 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure AddOffsetsToTxnResponse
        {
        addOffsetsToTxnResponseThrottleTimeMs = fieldthrottletimems
        ,
        addOffsetsToTxnResponseErrorCode = fielderrorcode
        }

  | version >= 0 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      pure AddOffsetsToTxnResponse
        {
        addOffsetsToTxnResponseThrottleTimeMs = fieldthrottletimems
        ,
        addOffsetsToTxnResponseErrorCode = fielderrorcode
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeAddOffsetsToTxnResponse' / 'decodeAddOffsetsToTxnResponse' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec AddOffsetsToTxnResponse where
  wireCodec = Just (WC.serialShimCodec encodeAddOffsetsToTxnResponse decodeAddOffsetsToTxnResponse)
  {-# INLINE wireCodec #-}
