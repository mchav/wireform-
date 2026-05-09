{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ControlRecordTypeSchema
Description : Kafka ControlRecordTypeSchema message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka data (no API key).



Valid versions: 0
Flexible versions: none

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ControlRecordTypeSchema
  (
    ControlRecordTypeSchema(..),
    encodeControlRecordTypeSchema,
    decodeControlRecordTypeSchema,
    maxControlRecordTypeSchemaVersion
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
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP




data ControlRecordTypeSchema = ControlRecordTypeSchema
  {

  -- | The type of the control record, such as commit or abort

  -- Versions: 0+
  controlRecordTypeSchemaType :: !(Int16)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ControlRecordTypeSchema.
maxControlRecordTypeSchemaVersion :: Int16
maxControlRecordTypeSchemaVersion = 0



-- | Encode ControlRecordTypeSchema with the given API version.
encodeControlRecordTypeSchema :: MonadPut m => E.ApiVersion -> ControlRecordTypeSchema -> m ()
encodeControlRecordTypeSchema version msg
  | version == 0 =
    do
      serialize (controlRecordTypeSchemaType msg)

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ControlRecordTypeSchema with the given API version.
decodeControlRecordTypeSchema :: MonadGet m => E.ApiVersion -> m ControlRecordTypeSchema
decodeControlRecordTypeSchema version
  | version == 0 =
    do
      fieldtype <- deserialize
      pure ControlRecordTypeSchema
        {
        controlRecordTypeSchemaType = fieldtype
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


-- | Worst-case wire size of a ControlRecordTypeSchema.
wireMaxSizeControlRecordTypeSchema :: Int -> ControlRecordTypeSchema -> Int
wireMaxSizeControlRecordTypeSchema _version msg =
  0
  + 2


-- | Direct-poke encoder for ControlRecordTypeSchema.
wirePokeControlRecordTypeSchema :: Int -> Ptr Word8 -> ControlRecordTypeSchema -> IO (Ptr Word8)
wirePokeControlRecordTypeSchema version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (controlRecordTypeSchemaType msg)
    pure p1
  | otherwise = error $ "wirePoke ControlRecordTypeSchema : unsupported version: " ++ show version

-- | Direct-poke decoder for ControlRecordTypeSchema.
wirePeekControlRecordTypeSchema :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ControlRecordTypeSchema, Ptr Word8)
wirePeekControlRecordTypeSchema version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_type, p1) <- W.peekInt16BE p0 endPtr
    pure (ControlRecordTypeSchema { controlRecordTypeSchemaType = f0_type }, p1)
  | otherwise = error $ "wirePeek ControlRecordTypeSchema : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated below, skipping the 'Data.Bytes.Serial' runner.
instance WC.WireCodec ControlRecordTypeSchema where
  wireCodec = Just WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeControlRecordTypeSchema (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeControlRecordTypeSchema (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekControlRecordTypeSchema (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}