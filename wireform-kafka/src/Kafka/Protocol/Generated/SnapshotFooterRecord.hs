{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.SnapshotFooterRecord
Description : Kafka SnapshotFooterRecord message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka data (no API key).



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.SnapshotFooterRecord
  (
    SnapshotFooterRecord(..),
    encodeSnapshotFooterRecord,
    decodeSnapshotFooterRecord,
    maxSnapshotFooterRecordVersion
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




data SnapshotFooterRecord = SnapshotFooterRecord
  {

  -- | The version of the snapshot footer record.

  -- Versions: 0+
  snapshotFooterRecordVersion :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for SnapshotFooterRecord.
maxSnapshotFooterRecordVersion :: Int16
maxSnapshotFooterRecordVersion = 0

-- | Encode SnapshotFooterRecord with the given API version.
encodeSnapshotFooterRecord :: MonadPut m => E.ApiVersion -> SnapshotFooterRecord -> m ()
encodeSnapshotFooterRecord version msg
  | version == 0 =
    do
      serialize (snapshotFooterRecordVersion msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode SnapshotFooterRecord with the given API version.
decodeSnapshotFooterRecord :: MonadGet m => E.ApiVersion -> m SnapshotFooterRecord
decodeSnapshotFooterRecord version
  | version == 0 =
    do
      fieldversion <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure SnapshotFooterRecord
        {
        snapshotFooterRecordVersion = fieldversion
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | 'WC.WireCodec' instance via the Serial shim. The
-- WireGenerator can't yet emit a native codec for this
-- schema (it carries arrays or nested struct fields the
-- generator hasn't been taught yet), so we lift the legacy
-- 'encodeSnapshotFooterRecord' / 'decodeSnapshotFooterRecord' pair into a
-- 'WireCodecImpl' via 'WC.serialShimCodec'. The dispatch
-- shape is identical to the native case — every
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' goes through a
-- 'Just'-valued codec, no 'Nothing' fallback survives in
-- the generated output.
instance WC.WireCodec SnapshotFooterRecord where
  wireCodec = Just (WC.serialShimCodec encodeSnapshotFooterRecord decodeSnapshotFooterRecord)
  {-# INLINE wireCodec #-}
