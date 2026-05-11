{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.AbortedTxn
Description : Kafka AbortedTxn message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka data (no API key).



Valid versions: 0
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.AbortedTxn
  (
    AbortedTxn(..),
    maxAbortedTxnVersion
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




data AbortedTxn = AbortedTxn
  {

  -- | The producer id associated with the aborted transaction

  -- Versions: 0+
  abortedTxnProducerId :: !(Int64)
,

  -- | The first offset in the aborted transaction

  -- Versions: 0+
  abortedTxnFirstOffset :: !(Int64)
,

  -- | The last offset in the aborted transaction

  -- Versions: 0+
  abortedTxnLastOffset :: !(Int64)
,

  -- | The last stable offset at the time the transaction was aborted

  -- Versions: 0+
  abortedTxnLastStableOffset :: !(Int64)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for AbortedTxn.
maxAbortedTxnVersion :: Int16
maxAbortedTxnVersion = 0




-- | Worst-case wire size of a AbortedTxn.
wireMaxSizeAbortedTxn :: Int -> AbortedTxn -> Int
wireMaxSizeAbortedTxn _version msg =
  0
  + 8
  + 8
  + 8
  + 8


-- | Direct-poke encoder for AbortedTxn.
wirePokeAbortedTxn :: Int -> Ptr Word8 -> AbortedTxn -> IO (Ptr Word8)
wirePokeAbortedTxn version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt64BE p0 (abortedTxnProducerId msg)
    p2 <- W.pokeInt64BE p1 (abortedTxnFirstOffset msg)
    p3 <- W.pokeInt64BE p2 (abortedTxnLastOffset msg)
    p4 <- W.pokeInt64BE p3 (abortedTxnLastStableOffset msg)
    pure p4
  | otherwise = error $ "wirePoke AbortedTxn : unsupported version: " ++ show version

-- | Direct-poke decoder for AbortedTxn.
wirePeekAbortedTxn :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (AbortedTxn, Ptr Word8)
wirePeekAbortedTxn version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_producerid, p1) <- W.peekInt64BE p0 endPtr
    (f1_firstoffset, p2) <- W.peekInt64BE p1 endPtr
    (f2_lastoffset, p3) <- W.peekInt64BE p2 endPtr
    (f3_laststableoffset, p4) <- W.peekInt64BE p3 endPtr
    pure (AbortedTxn { abortedTxnProducerId = f0_producerid, abortedTxnFirstOffset = f1_firstoffset, abortedTxnLastOffset = f2_lastoffset, abortedTxnLastStableOffset = f3_laststableoffset }, p4)
  | otherwise = error $ "wirePeek AbortedTxn : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above.
instance WC.WireCodec AbortedTxn where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeAbortedTxn (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeAbortedTxn (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekAbortedTxn (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}