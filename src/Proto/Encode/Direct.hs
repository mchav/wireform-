{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- | Direct-write encoder: bypasses ByteString.Builder entirely.
--
-- Builder uses a continuation-passing style with buffer-overflow handlers.
-- GHC generates closure allocations for these handlers even when the buffer
-- is always large enough (which it is when we pre-compute the exact size).
--
-- This module writes directly to a pre-allocated pinned MutableByteArray#
-- using primitive pointer arithmetic. Zero closures, zero continuations.
-- Each write is a raw store instruction.
module Proto.Encode.Direct
  ( -- * Direct write operations
    WriteCtx (..)
  , directEncode
  , dWord8
  , dWord32LE
  , dWord64LE
  , dFloatLE
  , dDoubleLE
  , dVarint
  , dByteString
  , dText
  ) where

import Data.Bits ((.&.), (.|.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr, mallocForeignPtrBytes)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (poke)
import GHC.Float (castFloatToWord32, castDoubleToWord64)
import qualified Data.ByteString as BS

-- | Write context: pointer to current position + end.
-- Passed as strict arguments through all write operations.
data WriteCtx = WriteCtx
  { wcPtr :: {-# UNPACK #-} !(Ptr Word8)
  , wcEnd :: {-# UNPACK #-} !(Ptr Word8)
  }

-- | Allocate a buffer of exactly @sz@ bytes, run the writer, return ByteString.
directEncode :: Int -> (WriteCtx -> IO WriteCtx) -> ByteString
directEncode !sz writer = BSI.unsafeCreate sz $ \ptr -> do
  _ <- writer (WriteCtx ptr (ptr `plusPtr` sz))
  pure ()
{-# INLINE directEncode #-}

-- | Write a single byte.
dWord8 :: Word8 -> WriteCtx -> IO WriteCtx
dWord8 !b (WriteCtx !p e) = do
  poke p b
  pure $! WriteCtx (p `plusPtr` 1) e
{-# INLINE dWord8 #-}

-- | Write a 32-bit LE value.
dWord32LE :: Word32 -> WriteCtx -> IO WriteCtx
dWord32LE !v (WriteCtx !p e) = do
  poke (castPtr p :: Ptr Word32) v
  pure $! WriteCtx (p `plusPtr` 4) e
{-# INLINE dWord32LE #-}

-- | Write a 64-bit LE value.
dWord64LE :: Word64 -> WriteCtx -> IO WriteCtx
dWord64LE !v (WriteCtx !p e) = do
  poke (castPtr p :: Ptr Word64) v
  pure $! WriteCtx (p `plusPtr` 8) e
{-# INLINE dWord64LE #-}

dFloatLE :: Float -> WriteCtx -> IO WriteCtx
dFloatLE !v = dWord32LE (castFloatToWord32 v)
{-# INLINE dFloatLE #-}

dDoubleLE :: Double -> WriteCtx -> IO WriteCtx
dDoubleLE !v = dWord64LE (castDoubleToWord64 v)
{-# INLINE dDoubleLE #-}

-- | Write a varint. Fully unrolled for 1-3 bytes (the common case).
dVarint :: Word64 -> WriteCtx -> IO WriteCtx
dVarint !n ctx@(WriteCtx !p e)
  | n < 0x80 = do
      poke p (fromIntegral n :: Word8)
      pure $! WriteCtx (p `plusPtr` 1) e
  | n < 0x4000 = do
      poke p (fromIntegral (n .&. 0x7F .|. 0x80) :: Word8)
      poke (p `plusPtr` 1) (fromIntegral (n `shiftR` 7) :: Word8)
      pure $! WriteCtx (p `plusPtr` 2) e
  | n < 0x200000 = do
      poke p (fromIntegral (n .&. 0x7F .|. 0x80) :: Word8)
      poke (p `plusPtr` 1) (fromIntegral ((n `shiftR` 7) .&. 0x7F .|. 0x80) :: Word8)
      poke (p `plusPtr` 2) (fromIntegral (n `shiftR` 14) :: Word8)
      pure $! WriteCtx (p `plusPtr` 3) e
  | otherwise = dVarintSlow n ctx
{-# INLINE dVarint #-}

dVarintSlow :: Word64 -> WriteCtx -> IO WriteCtx
dVarintSlow !n (WriteCtx !p e) = go n p
  where
    go !v !ptr
      | v < 0x80 = do
          poke ptr (fromIntegral v :: Word8)
          pure $! WriteCtx (ptr `plusPtr` 1) e
      | otherwise = do
          poke ptr (fromIntegral (v .&. 0x7F .|. 0x80) :: Word8)
          go (v `shiftR` 7) (ptr `plusPtr` 1)

-- | Write a ByteString (length-delimited): varint length + raw bytes.
dByteString :: ByteString -> WriteCtx -> IO WriteCtx
dByteString !bs ctx = do
  ctx' <- dVarint (fromIntegral (BS.length bs)) ctx
  let WriteCtx p' e' = ctx'
  let (BSI.BS fp _) = bs
  withForeignPtr fp $ \src ->
    BSI.memcpy p' src (BS.length bs)
  pure $! WriteCtx (p' `plusPtr` BS.length bs) e'
{-# INLINE dByteString #-}

-- | Write a Text field: varint length + UTF-8 bytes.
dText :: Text -> WriteCtx -> IO WriteCtx
dText !t = dByteString (TE.encodeUtf8 t)
{-# INLINE dText #-}
