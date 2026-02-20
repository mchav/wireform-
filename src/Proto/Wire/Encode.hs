{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- | Low-level, high-performance wire format encoding primitives.
--
-- All encoding is done via 'Data.ByteString.Builder' for zero-copy
-- concatenation and efficient output. Varint encoding is unrolled for
-- common small values.
module Proto.Wire.Encode
  ( -- * Varint encoding
    putVarint
  , putVarintSigned
  , putSVarint32
  , putSVarint64

    -- * Fixed-width encoding
  , putFixed32
  , putFixed64
  , putFloat
  , putDouble

    -- * Length-delimited
  , putLengthDelimited
  , putByteString
  , putText

    -- * Tags
  , putTag

    -- * Helpers
  , zigZag32
  , zigZag64
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)

import Proto.Wire (WireType, fieldTag)

-- | Encode a varint (unsigned). Unrolled for values that fit in 1-2 bytes
-- (the common case for field tags and small values).
putVarint :: Word64 -> Builder
putVarint !n
  | n < 0x80 =
      B.word8 (fromIntegral n)
  | n < 0x4000 =
      B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <>
      B.word8 (fromIntegral (n `shiftR` 7))
  | otherwise = putVarintSlow n
{-# INLINE putVarint #-}

putVarintSlow :: Word64 -> Builder
putVarintSlow = go
  where
    go !n
      | n < 0x80  = B.word8 (fromIntegral n)
      | otherwise = B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80) <> go (n `shiftR` 7)

-- | Encode a signed varint (two's complement, always 10 bytes for negatives).
putVarintSigned :: Int64 -> Builder
putVarintSigned n = putVarint (fromIntegral n)
{-# INLINE putVarintSigned #-}

-- | Encode a sint32 using zigzag encoding.
putSVarint32 :: Int32 -> Builder
putSVarint32 n = putVarint (fromIntegral (zigZag32 n))
{-# INLINE putSVarint32 #-}

-- | Encode a sint64 using zigzag encoding.
putSVarint64 :: Int64 -> Builder
putSVarint64 n = putVarint (zigZag64 n)
{-# INLINE putSVarint64 #-}

-- | ZigZag encoding for 32-bit signed integers.
-- Maps signed integers to unsigned: 0 -> 0, -1 -> 1, 1 -> 2, -2 -> 3, ...
zigZag32 :: Int32 -> Word32
zigZag32 n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 31))
{-# INLINE zigZag32 #-}

-- | ZigZag encoding for 64-bit signed integers.
zigZag64 :: Int64 -> Word64
zigZag64 n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))
{-# INLINE zigZag64 #-}

-- | Encode a 32-bit fixed value (little-endian).
putFixed32 :: Word32 -> Builder
putFixed32 = B.word32LE
{-# INLINE putFixed32 #-}

-- | Encode a 64-bit fixed value (little-endian).
putFixed64 :: Word64 -> Builder
putFixed64 = B.word64LE
{-# INLINE putFixed64 #-}

-- | Encode a float (little-endian).
putFloat :: Float -> Builder
putFloat = B.floatLE
{-# INLINE putFloat #-}

-- | Encode a double (little-endian).
putDouble :: Double -> Builder
putDouble = B.doubleLE
{-# INLINE putDouble #-}

-- | Encode a length-delimited field: varint length prefix + payload.
putLengthDelimited :: ByteString -> Builder
putLengthDelimited bs =
  putVarint (fromIntegral (BS.length bs)) <> B.byteString bs
{-# INLINE putLengthDelimited #-}

-- | Encode a bytes field.
putByteString :: ByteString -> Builder
putByteString = putLengthDelimited
{-# INLINE putByteString #-}

-- | Encode a string field (UTF-8).
putText :: Text -> Builder
putText t =
  let bs = TE.encodeUtf8 t
  in putLengthDelimited bs
{-# INLINE putText #-}

-- | Encode a field tag (field number + wire type) as a varint.
putTag :: Int -> WireType -> Builder
putTag fn wt = putVarint (fieldTag fn wt)
{-# INLINE putTag #-}
