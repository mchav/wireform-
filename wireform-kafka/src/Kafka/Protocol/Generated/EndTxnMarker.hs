{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.EndTxnMarker
Description : Kafka EndTxnMarker message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka data (no API key).



Valid versions: 0
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.EndTxnMarker
  (
    EndTxnMarker(..),
    encodeEndTxnMarker,
    decodeEndTxnMarker,
    maxEndTxnMarkerVersion
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




data EndTxnMarker = EndTxnMarker
  {

  -- | The coordinator epoch when appending the record

  -- Versions: 0+
  endTxnMarkerCoordinatorEpoch :: !(Int32)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for EndTxnMarker.
maxEndTxnMarkerVersion :: Int16
maxEndTxnMarkerVersion = 0

-- | Encode EndTxnMarker with the given API version.
encodeEndTxnMarker :: MonadPut m => E.ApiVersion -> EndTxnMarker -> m ()
encodeEndTxnMarker version msg
  | version == 0 =
    do
      serialize (endTxnMarkerCoordinatorEpoch msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode EndTxnMarker with the given API version.
decodeEndTxnMarker :: MonadGet m => E.ApiVersion -> m EndTxnMarker
decodeEndTxnMarker version
  | version == 0 =
    do
      fieldcoordinatorepoch <- deserialize
      pure EndTxnMarker
        {
        endTxnMarkerCoordinatorEpoch = fieldcoordinatorepoch
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec EndTxnMarker where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
