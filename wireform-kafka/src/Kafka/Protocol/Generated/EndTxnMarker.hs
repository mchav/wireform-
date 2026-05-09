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
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
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


-- | Worst-case wire size of a EndTxnMarker.
wireMaxSizeEndTxnMarker :: Int -> EndTxnMarker -> Int
wireMaxSizeEndTxnMarker _version msg =
  0
  + 4


-- | Direct-poke encoder for EndTxnMarker.
wirePokeEndTxnMarker :: Int -> Ptr Word8 -> EndTxnMarker -> IO (Ptr Word8)
wirePokeEndTxnMarker version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (endTxnMarkerCoordinatorEpoch msg)
    pure p1
  | otherwise = error $ "wirePoke EndTxnMarker : unsupported version: " ++ show version

-- | Direct-poke decoder for EndTxnMarker.
wirePeekEndTxnMarker :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (EndTxnMarker, Ptr Word8)
wirePeekEndTxnMarker version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_coordinatorepoch, p1) <- W.peekInt32BE p0 endPtr
    pure (EndTxnMarker { endTxnMarkerCoordinatorEpoch = f0_coordinatorepoch }, p1)
  | otherwise = error $ "wirePeek EndTxnMarker : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec EndTxnMarker where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeEndTxnMarker (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeEndTxnMarker (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekEndTxnMarker (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}