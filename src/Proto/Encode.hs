-- | High-level encoding interface for protobuf messages.
--
-- This module provides the 'MessageEncoder' typeclass and utilities for
-- encoding messages to 'ByteString'. Key performance characteristics:
--
-- * Single-pass size calculation + encoding for non-nested messages
-- * Builder-based output for zero-copy concatenation
-- * Packed encoding for repeated scalar fields
-- * Pre-computed tag bytes for generated code
module Proto.Encode
  ( -- * Encoding typeclass
    MessageEncode (..)

    -- * Running encoders
  , encodeMessage
  , encodeLazy

    -- * Field encoding helpers
  , encodeFieldVarint
  , encodeFieldSVarint32
  , encodeFieldSVarint64
  , encodeFieldFixed32
  , encodeFieldFixed64
  , encodeFieldFloat
  , encodeFieldDouble
  , encodeFieldBool
  , encodeFieldString
  , encodeFieldBytes
  , encodeFieldMessage
  , encodeFieldEnum

    -- * Packed repeated field encoding
  , encodePackedVarint
  , encodePackedFixed32
  , encodePackedFixed64
  , encodePackedFloat
  , encodePackedDouble
  , encodePackedSVarint32
  , encodePackedSVarint64

    -- * Map field encoding
  , encodeMapField

    -- * Raw builders
  , messageToByteString
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word32, Word64)
import Data.Text (Text)

import Proto.Wire (WireType (..))
import Proto.Wire.Encode

-- | Typeclass for types that can be encoded as protobuf messages.
class MessageEncode a where
  -- | Build the wire-format representation (fields only, no outer length prefix).
  buildMessage :: a -> B.Builder

-- | Encode a message to a strict 'ByteString'.
encodeMessage :: MessageEncode a => a -> ByteString
encodeMessage = BL.toStrict . B.toLazyByteString . buildMessage
{-# INLINE encodeMessage #-}

-- | Encode a message to a lazy 'ByteString' (useful for streaming).
encodeLazy :: MessageEncode a => a -> BL.ByteString
encodeLazy = B.toLazyByteString . buildMessage
{-# INLINE encodeLazy #-}

-- | Convert a builder to strict ByteString (for submessage size calculation).
messageToByteString :: B.Builder -> ByteString
messageToByteString = BL.toStrict . B.toLazyByteString

-- | Encode a varint field (int32, int64, uint32, uint64).
encodeFieldVarint :: Int -> Word64 -> B.Builder
encodeFieldVarint fn val =
  putTag fn WireVarint <> putVarint val
{-# INLINE encodeFieldVarint #-}

-- | Encode a sint32 field.
encodeFieldSVarint32 :: Int -> Int32 -> B.Builder
encodeFieldSVarint32 fn val =
  putTag fn WireVarint <> putSVarint32 val
{-# INLINE encodeFieldSVarint32 #-}

-- | Encode a sint64 field.
encodeFieldSVarint64 :: Int -> Int64 -> B.Builder
encodeFieldSVarint64 fn val =
  putTag fn WireVarint <> putSVarint64 val
{-# INLINE encodeFieldSVarint64 #-}

-- | Encode a fixed32 field.
encodeFieldFixed32 :: Int -> Word32 -> B.Builder
encodeFieldFixed32 fn val =
  putTag fn Wire32Bit <> putFixed32 val
{-# INLINE encodeFieldFixed32 #-}

-- | Encode a fixed64 field.
encodeFieldFixed64 :: Int -> Word64 -> B.Builder
encodeFieldFixed64 fn val =
  putTag fn Wire64Bit <> putFixed64 val
{-# INLINE encodeFieldFixed64 #-}

-- | Encode a float field.
encodeFieldFloat :: Int -> Float -> B.Builder
encodeFieldFloat fn val =
  putTag fn Wire32Bit <> putFloat val
{-# INLINE encodeFieldFloat #-}

-- | Encode a double field.
encodeFieldDouble :: Int -> Double -> B.Builder
encodeFieldDouble fn val =
  putTag fn Wire64Bit <> putDouble val
{-# INLINE encodeFieldDouble #-}

-- | Encode a bool field.
encodeFieldBool :: Int -> Bool -> B.Builder
encodeFieldBool fn val =
  putTag fn WireVarint <> putVarint (if val then 1 else 0)
{-# INLINE encodeFieldBool #-}

-- | Encode a string field.
encodeFieldString :: Int -> Text -> B.Builder
encodeFieldString fn val =
  putTag fn WireLengthDelimited <> putText val
{-# INLINE encodeFieldString #-}

-- | Encode a bytes field.
encodeFieldBytes :: Int -> ByteString -> B.Builder
encodeFieldBytes fn val =
  putTag fn WireLengthDelimited <> putByteString val
{-# INLINE encodeFieldBytes #-}

-- | Encode a submessage field. Materializes the submessage to calculate its length.
encodeFieldMessage :: MessageEncode a => Int -> a -> B.Builder
encodeFieldMessage fn msg =
  let payload = messageToByteString (buildMessage msg)
  in putTag fn WireLengthDelimited <> putLengthDelimited payload
{-# INLINE encodeFieldMessage #-}

-- | Encode an enum field (as varint).
encodeFieldEnum :: Enum a => Int -> a -> B.Builder
encodeFieldEnum fn val =
  putTag fn WireVarint <> putVarint (fromIntegral (fromEnum val))
{-# INLINE encodeFieldEnum #-}

-- | Encode a packed repeated varint field.
encodePackedVarint :: Int -> VU.Vector Word64 -> B.Builder
encodePackedVarint fn vals
  | VU.null vals = mempty
  | otherwise =
      let payload = messageToByteString (VU.foldl' (\acc v -> acc <> putVarint v) mempty vals)
      in putTag fn WireLengthDelimited <> putLengthDelimited payload
{-# INLINE encodePackedVarint #-}

-- | Encode a packed repeated fixed32 field.
encodePackedFixed32 :: Int -> VU.Vector Word32 -> B.Builder
encodePackedFixed32 fn vals
  | VU.null vals = mempty
  | otherwise =
      let payload = messageToByteString (VU.foldl' (\acc v -> acc <> putFixed32 v) mempty vals)
      in putTag fn WireLengthDelimited <> putLengthDelimited payload
{-# INLINE encodePackedFixed32 #-}

-- | Encode a packed repeated fixed64 field.
encodePackedFixed64 :: Int -> VU.Vector Word64 -> B.Builder
encodePackedFixed64 fn vals
  | VU.null vals = mempty
  | otherwise =
      let payload = messageToByteString (VU.foldl' (\acc v -> acc <> putFixed64 v) mempty vals)
      in putTag fn WireLengthDelimited <> putLengthDelimited payload
{-# INLINE encodePackedFixed64 #-}

-- | Encode a packed repeated float field.
encodePackedFloat :: Int -> VU.Vector Float -> B.Builder
encodePackedFloat fn vals
  | VU.null vals = mempty
  | otherwise =
      let payload = messageToByteString (VU.foldl' (\acc v -> acc <> putFloat v) mempty vals)
      in putTag fn WireLengthDelimited <> putLengthDelimited payload
{-# INLINE encodePackedFloat #-}

-- | Encode a packed repeated double field.
encodePackedDouble :: Int -> VU.Vector Double -> B.Builder
encodePackedDouble fn vals
  | VU.null vals = mempty
  | otherwise =
      let payload = messageToByteString (VU.foldl' (\acc v -> acc <> putDouble v) mempty vals)
      in putTag fn WireLengthDelimited <> putLengthDelimited payload
{-# INLINE encodePackedDouble #-}

-- | Encode a packed repeated sint32 field.
encodePackedSVarint32 :: Int -> VU.Vector Int32 -> B.Builder
encodePackedSVarint32 fn vals
  | VU.null vals = mempty
  | otherwise =
      let payload = messageToByteString (VU.foldl' (\acc v -> acc <> putSVarint32 v) mempty vals)
      in putTag fn WireLengthDelimited <> putLengthDelimited payload
{-# INLINE encodePackedSVarint32 #-}

-- | Encode a packed repeated sint64 field.
encodePackedSVarint64 :: Int -> VU.Vector Int64 -> B.Builder
encodePackedSVarint64 fn vals
  | VU.null vals = mempty
  | otherwise =
      let payload = messageToByteString (VU.foldl' (\acc v -> acc <> putSVarint64 v) mempty vals)
      in putTag fn WireLengthDelimited <> putLengthDelimited payload
{-# INLINE encodePackedSVarint64 #-}

-- | Encode a map field entry.
-- Map fields are encoded as repeated message fields with key=1, value=2.
encodeMapField
  :: Int          -- ^ Field number of the map field
  -> B.Builder    -- ^ Key encoding (field 1)
  -> B.Builder    -- ^ Value encoding (field 2)
  -> B.Builder
encodeMapField fn keyEnc valEnc =
  let entry = messageToByteString (keyEnc <> valEnc)
  in putTag fn WireLengthDelimited <> putLengthDelimited entry
{-# INLINE encodeMapField #-}
