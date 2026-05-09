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
    maxKRaftVersionRecordVersion
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




-- | Worst-case wire size of a KRaftVersionRecord.
wireMaxSizeKRaftVersionRecord :: Int -> KRaftVersionRecord -> Int
wireMaxSizeKRaftVersionRecord _version msg =
  0
  + 2
  + 2
  + 1

-- | Direct-poke encoder for KRaftVersionRecord.
wirePokeKRaftVersionRecord :: Int -> Ptr Word8 -> KRaftVersionRecord -> IO (Ptr Word8)
wirePokeKRaftVersionRecord version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (kRaftVersionRecordVersion msg)
    p2 <- W.pokeInt16BE p1 (kRaftVersionRecordKRaftVersion msg)
    WP.pokeEmptyTaggedFields p2
  | otherwise = error $ "wirePoke KRaftVersionRecord : unsupported version: " ++ show version

-- | Direct-poke decoder for KRaftVersionRecord.
wirePeekKRaftVersionRecord :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (KRaftVersionRecord, Ptr Word8)
wirePeekKRaftVersionRecord version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_version, p1) <- W.peekInt16BE p0 endPtr
    (f1_kraftversion, p2) <- W.peekInt16BE p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (KRaftVersionRecord { kRaftVersionRecordVersion = f0_version, kRaftVersionRecordKRaftVersion = f1_kraftversion }, pTagsEnd)
  | otherwise = error $ "wirePeek KRaftVersionRecord : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec KRaftVersionRecord where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeKRaftVersionRecord (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeKRaftVersionRecord (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekKRaftVersionRecord (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}