{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.KRaftVersionRecord
Description : Kafka KRaftVersionRecord message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka data (no API key).



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.KRaftVersionRecord
  (
    KRaftVersionRecord(..),
    encodeKRaftVersionRecord,
    decodeKRaftVersionRecord,
    maxKRaftVersionRecordVersion
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




data KRaftVersionRecord = KRaftVersionRecord
  {

  -- | The version of the kraft version record.

  -- Versions: 0+
  kRaftVersionRecordVersion :: !(Int16)
,

  -- | The kraft protocol version.

  -- Versions: 0+
  kRaftVersionRecordKRaftVersion :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for KRaftVersionRecord.
maxKRaftVersionRecordVersion :: Int16
maxKRaftVersionRecordVersion = 0

-- | Encode KRaftVersionRecord with the given API version.
encodeKRaftVersionRecord :: MonadPut m => E.ApiVersion -> KRaftVersionRecord -> m ()
encodeKRaftVersionRecord version msg
  | version == 0 =
    do
      serialize (kRaftVersionRecordVersion msg)
      serialize (kRaftVersionRecordKRaftVersion msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode KRaftVersionRecord with the given API version.
decodeKRaftVersionRecord :: MonadGet m => E.ApiVersion -> m KRaftVersionRecord
decodeKRaftVersionRecord version
  | version == 0 =
    do
      fieldversion <- deserialize
      fieldkraftversion <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure KRaftVersionRecord
        {
        kRaftVersionRecordVersion = fieldversion
        ,
        kRaftVersionRecordKRaftVersion = fieldkraftversion
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec KRaftVersionRecord where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
