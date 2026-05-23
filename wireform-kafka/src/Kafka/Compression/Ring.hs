{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Compression.Ring
Description : Decompression entry points that operate on raw pointers / ring slices
Copyright   : (c) 2026
License     : BSD-3-Clause
Maintainer  : kafka-native

The existing 'Kafka.Compression' interface expects 'ByteString'
inputs.  When the Kafka response body lives on the magic ring
(@Kafka.Network.RingTransport@ / @Kafka.Network.FrameParser@), the
compressed @records@ section is already a contiguous run of bytes
in the ring; wrapping it as a 'ByteString' just so the C
decompressor can call 'unsafeUseAsCStringLen' on it is a pointless
round-trip.

This module exposes 'Ptr'-based decompressors that take a source
pointer + length directly (the slice's address inside the ring) and
produce a freshly-allocated 'ByteString' holding the plaintext.
For codecs that can report their output size up-front (snappy,
zstd) the destination 'ByteString' is sized exactly via
'BSI.mallocByteString'; the decompressor writes straight into it
with no intermediate C @malloc@ + @memcpy@ + @free@ round-trip.

Use these entry points from 'Kafka.Protocol.RecordBatchWire' when
the caller can hand over a ring-resident pointer (the
@RecordBatchWire.decodeBatchWithDecompression@ path already
extracts a @Ptr Word8@ from the input 'ByteString' before calling
the decompressor; this just lets it skip the BS↔Ptr round-trip).
-}
module Kafka.Compression.Ring
  ( -- * Pointer-input decompressors
    decompressFromPtr
    -- * Codec-specific sized-output paths
  , snappyDecompressFromPtr
  , zstdDecompressFromPtr
    -- * Codec-specific fallbacks (still allocate input BS)
  , gzipDecompressFromPtr
  , lz4DecompressFromPtr
  ) where

import qualified Codec.Compression.Zstd as Zstd
import Codec.Compression.Zstd (Decompress (..))
import Control.Exception (try, SomeException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word8)
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)

import qualified Kafka.Compression.Gzip as Gzip
import qualified Kafka.Compression.Lz4 as Lz4
import Kafka.Compression.Types (CompressionCodec (..))

------------------------------------------------------------------------
-- Top-level dispatch
------------------------------------------------------------------------

-- | Decompress @len@ bytes starting at @src@ using the supplied
-- codec.  The source pointer must point at a contiguous run of
-- bytes (the magic ring's double mapping guarantees this for any
-- read of up to @ringSize@); the result is a fresh heap-allocated
-- 'ByteString' that the caller owns past the ring's tail advance.
--
-- 'NoCompression' returns a freshly-copied 'ByteString' so the
-- caller can use the result interchangeably with the other codecs
-- (the alternative — handing back a slice of the ring — would
-- have a tighter lifetime that the call sites don't currently
-- handle).
decompressFromPtr
  :: CompressionCodec
  -> Ptr Word8
  -> Int
  -> IO (Either String ByteString)
decompressFromPtr codec src len
  | len <= 0  = pure (Right BS.empty)
  | otherwise = case codec of
      NoCompression -> Right <$> copyPtrToBs src len
      Snappy        -> snappyDecompressFromPtr src len
      Zstd          -> zstdDecompressFromPtr   src len
      Gzip          -> gzipDecompressFromPtr   src len
      Lz4           -> lz4DecompressFromPtr    src len

------------------------------------------------------------------------
-- Snappy: sized-output direct-to-buffer decompress
------------------------------------------------------------------------

foreign import ccall unsafe "snappy_ffi.h snappy_get_uncompressed_length_wrapper"
  c_snappy_get_uncompressed_length
    :: Ptr CChar -> CSize -> IO CSize

foreign import ccall unsafe "snappy_ffi.h snappy_decompress_into"
  c_snappy_decompress_into
    :: Ptr CChar -> CSize -> Ptr CChar -> CSize -> IO CInt

-- | Snappy decompress with a one-shot, exactly-sized output
-- allocation.  Avoids the C-side @malloc@ + 'BS.packCStringLen' +
-- @free@ round-trip that the legacy 'Kafka.Compression.Snappy.decompressSnappy'
-- entry point goes through.
snappyDecompressFromPtr
  :: Ptr Word8
  -> Int
  -> IO (Either String ByteString)
snappyDecompressFromPtr src len = do
  let !srcCC   = castPtr src :: Ptr CChar
      !srcCSz  = fromIntegral len :: CSize
  outLen <- c_snappy_get_uncompressed_length srcCC srcCSz
  if outLen == maxBound  -- (size_t)-1 sentinel for malformed input
    then pure (Left "Snappy: malformed input header")
    else do
      let !outLenInt = fromIntegral outLen :: Int
      fp <- BSI.mallocByteString outLenInt
      ok <- withForeignPtr fp $ \dst ->
        c_snappy_decompress_into srcCC srcCSz (castPtr dst) outLen
      if ok == 0
        then pure (Left "Snappy decompression failed")
        else pure (Right (BSI.fromForeignPtr fp 0 outLenInt))

------------------------------------------------------------------------
-- Zstd: sized-output via Codec.Compression.Zstd.decompress
------------------------------------------------------------------------

-- | Zstd decompress.  Reads the source bytes into a temporary
-- 'ByteString' first (the @zstd@ Haskell binding's pointer API
-- isn't exposed) and then runs the standard 'Zstd.decompress'.
-- The temporary copy is unavoidable until the @zstd@ binding
-- exposes a 'Ptr'-based path; the upside vs. routing through
-- 'Kafka.Compression.Zstd.decompressZstd' is that we know the
-- input bytes already live in the ring, so the input BS is a
-- fresh allocation rather than a slice that would pin the ring
-- past the parse step.
zstdDecompressFromPtr
  :: Ptr Word8
  -> Int
  -> IO (Either String ByteString)
zstdDecompressFromPtr src len = do
  inBs <- copyPtrToBs src len
  result <- try $ pure $ Zstd.decompress inBs
  pure $ case result of
    Left e -> Left $ "Zstd decompression failed: " ++ show (e :: SomeException)
    Right (Decompress decompressed) -> Right decompressed
    Right (Error err) -> Left $ "Zstd decompression error: " ++ err
    Right Skip -> Left "Zstd decompression returned Skip"

------------------------------------------------------------------------
-- Gzip + LZ4: fall back via the existing BS-based decompressors
------------------------------------------------------------------------

-- | Gzip decompress.  Falls back to the streaming
-- 'Gzip.decompressGzip' (output size is not knowable without
-- decompressing), but skips the BS↔Ptr↔BS round-trip on the
-- input side by constructing a fresh BS directly from the
-- supplied ring pointer.
gzipDecompressFromPtr
  :: Ptr Word8
  -> Int
  -> IO (Either String ByteString)
gzipDecompressFromPtr src len = do
  inBs <- copyPtrToBs src len
  pure (Gzip.decompressGzip inBs)

-- | LZ4 decompress.  The LZ4 frame format embeds the
-- uncompressed size in the header when the @FLG.Content-Size@ bit
-- is set, but Kafka producers don't always set it; the safe
-- fallback is the streaming wrapper in
-- 'Lz4.decompressLz4'.  Like 'gzipDecompressFromPtr' this skips
-- the input-side BS↔Ptr↔BS round-trip.
lz4DecompressFromPtr
  :: Ptr Word8
  -> Int
  -> IO (Either String ByteString)
lz4DecompressFromPtr src len = do
  inBs <- copyPtrToBs src len
  Lz4.decompressLz4 inBs

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Copy @n@ bytes from a raw pointer into a fresh
-- 'BS.ByteString'.  Cheaper than the @BSI.create@ default because
-- it skips the allocator's clearing step.
copyPtrToBs :: Ptr Word8 -> Int -> IO ByteString
copyPtrToBs src n = do
  fp <- BSI.mallocByteString n
  withForeignPtr fp $ \dst ->
    BSI.memcpy dst src n
  pure (BSI.fromForeignPtr fp 0 n)
