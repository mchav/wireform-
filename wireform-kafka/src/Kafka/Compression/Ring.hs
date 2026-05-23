{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Compression.Ring
Description : Direct-into-buffer decompression for the magic ring
Copyright   : (c) 2026
License     : BSD-3-Clause
Maintainer  : kafka-native

When the Kafka response body lives on the magic ring
(@Kafka.Network.RingTransport@ + @Kafka.Network.FrameParser@) the
compressed @records@ section is a contiguous run of bytes inside
the ring.  Going through the legacy ByteString-based codec
interface ('Kafka.Compression.compress' \/ 'decompress') wraps that
slice as a 'ByteString' just so the codec's @unsafeUseAsCStringLen@
can extract the same pointer back — pointless round-trip.

This module wraps the underlying C decompression libraries directly
(zlib \/ liblz4 \/ libzstd \/ libsnappy) so the source is a 'Ptr Word8'
that can point straight into the magic ring's backing memory, and
the destination is either:

  * a freshly heap-allocated 'BS.ByteString' sized exactly via
    'BSI.mallocByteString' (the snappy + zstd happy path — both
    codecs report the decompressed size up front), or

  * a /caller-supplied/ destination region — typically a fresh
    magic ring allocated once per connection \/ batch — that the
    decompressor writes into incrementally.  Used for the streaming
    codecs (gzip + lz4) whose frame headers don't encode the
    plaintext size and for which the natural shape is "decompress
    into this buffer and tell me how many bytes you wrote".

The "decompress into a magic ring" path is exposed as
'decompressIntoRing'; callers allocate a 'MagicRing' (which is
just two mmap regions on Linux — virtual address space, no
resident memory until the bytes are touched) sized to the largest
plausible decompressed batch and reuse it across decompressions.
A subsequent decode against the destination slice walks the bytes
without any heap copy.
-}
module Kafka.Compression.Ring
  ( -- * Pointer-input decompressors
    decompressFromPtr
    -- * Codec-specific sized-output paths (snappy, zstd)
  , snappyDecompressFromPtr
  , zstdDecompressFromPtr
  , gzipDecompressFromPtr
  , lz4DecompressFromPtr

    -- * Decompress into a caller-supplied magic ring
  , decompressIntoRing
  , RingDecompressError (..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.ByteString (ByteString)
import Data.Word (Word8)
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import GHC.Generics (Generic)

import Wireform.Ring.Internal
  ( MagicRing
  , ringBase
  , ringSize
  )

import Kafka.Compression.Types (CompressionCodec (..))

------------------------------------------------------------------------
-- C imports — direct decompression on raw pointers
------------------------------------------------------------------------

-- Sentinel return codes mirrored from cbits/wireform_decompress.h.
pattern WF_OK, WF_NEEDS_MORE_OUT, WF_BAD_INPUT, WF_INIT_FAIL :: CInt
pattern WF_OK              = 0
pattern WF_NEEDS_MORE_OUT  = -1
pattern WF_BAD_INPUT       = -2
pattern WF_INIT_FAIL       = -4
{-# COMPLETE WF_OK, WF_NEEDS_MORE_OUT, WF_BAD_INPUT, WF_INIT_FAIL #-}

foreign import ccall unsafe "wireform_decompress.h wf_gzip_inflate_into"
  c_gzip_inflate_into
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt

foreign import ccall unsafe "wireform_decompress.h wf_lz4f_decompress_into"
  c_lz4f_decompress_into
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize
    -> Ptr CSize -> Ptr CSize -> IO CInt

foreign import ccall unsafe "wireform_decompress.h wf_zstd_get_frame_content_size"
  c_zstd_get_frame_content_size
    :: Ptr Word8 -> CSize -> IO CSize

foreign import ccall unsafe "wireform_decompress.h wf_zstd_decompress_into"
  c_zstd_decompress_into
    :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt

-- Snappy already lives in cbits/snappy_ffi.c.
foreign import ccall unsafe "snappy_ffi.h snappy_get_uncompressed_length_wrapper"
  c_snappy_get_uncompressed_length
    :: Ptr CChar -> CSize -> IO CSize

foreign import ccall unsafe "snappy_ffi.h snappy_decompress_into"
  c_snappy_decompress_into
    :: Ptr CChar -> CSize -> Ptr CChar -> CSize -> IO CInt

------------------------------------------------------------------------
-- Top-level dispatch: produce a fresh heap ByteString
------------------------------------------------------------------------

-- | Decompress @len@ bytes starting at @src@ using the supplied
-- codec.  The result is a fresh 'BS.ByteString' on the heap; for
-- snappy + zstd the destination is sized exactly via
-- 'BSI.mallocByteString' so the C decompressor writes straight
-- into it.  For gzip + lz4 the destination is sized to a
-- generous upper bound (8× input length, minimum 256 KiB) and
-- resized once if that proves insufficient.
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
-- Snappy
------------------------------------------------------------------------

-- | Snappy block-format decompress with exactly-sized output.
-- The destination ByteString is allocated via 'BSI.mallocByteString'
-- and the C decompressor writes directly into it — no intermediate
-- C-side malloc / Haskell-side memcpy round-trip.
snappyDecompressFromPtr
  :: Ptr Word8
  -> Int
  -> IO (Either String ByteString)
snappyDecompressFromPtr src len = do
  let !srcCC  = ptrToCChar src
      !srcSz  = fromIntegral len :: CSize
  outLen <- c_snappy_get_uncompressed_length srcCC srcSz
  if outLen == maxBound  -- (size_t)-1 sentinel for malformed input
    then pure (Left "Snappy: malformed input header")
    else do
      let !outLenInt = fromIntegral outLen :: Int
      fp <- BSI.mallocByteString outLenInt
      ok <- withForeignPtr fp $ \dst ->
        c_snappy_decompress_into srcCC srcSz (ptrToCChar dst) outLen
      if ok == 0
        then pure (Left "Snappy decompression failed")
        else pure (Right (BSI.fromForeignPtr fp 0 outLenInt))

------------------------------------------------------------------------
-- Zstd
------------------------------------------------------------------------

-- | Zstd decompress.  Reads the frame content size from the header;
-- when present (the common case in Kafka) allocates the destination
-- ByteString to exactly that size and calls 'wf_zstd_decompress_into'
-- once with no intermediate buffering.  When the size is not
-- embedded in the frame, falls back to a doubling-output strategy.
zstdDecompressFromPtr
  :: Ptr Word8
  -> Int
  -> IO (Either String ByteString)
zstdDecompressFromPtr src len = do
  let !srcSz = fromIntegral len :: CSize
  contentSize <- c_zstd_get_frame_content_size src srcSz
  let !unknown  = maxBound :: CSize          -- ZSTD_CONTENTSIZE_UNKNOWN
      !badInput = maxBound - 1 :: CSize      -- ZSTD_CONTENTSIZE_ERROR
  if contentSize == badInput
    then pure (Left "Zstd: malformed frame header")
    else if contentSize == unknown
      then doublingOutput src len 0 (max (len * 4) (256 * 1024))
      else do
        let !outLenInt = fromIntegral contentSize :: Int
        fp <- BSI.mallocByteString outLenInt
        alloca $ \producedP -> do
          rc <- withForeignPtr fp $ \dst ->
            c_zstd_decompress_into src srcSz dst contentSize producedP
          case rc of
            WF_OK -> do
              produced <- peek producedP
              pure (Right (BSI.fromForeignPtr fp 0 (fromIntegral produced)))
            WF_NEEDS_MORE_OUT ->
              -- The frame header claimed N bytes but the payload
              -- actually produced more.  Fall back to the doubling
              -- path with a fresh, larger destination.
              doublingOutput src len 0 (outLenInt * 2)
            WF_BAD_INPUT  -> pure (Left "Zstd decompression failed")
            WF_INIT_FAIL  -> pure (Left "Zstd init failed")
  where
    doublingOutput _ _ _ cap | cap <= 0 = pure (Left "Zstd: output too large")
    doublingOutput s sl _ cap = do
      fp <- BSI.mallocByteString cap
      alloca $ \producedP -> do
        rc <- withForeignPtr fp $ \dst ->
          c_zstd_decompress_into s (fromIntegral sl) dst (fromIntegral cap)
                                 producedP
        case rc of
          WF_OK -> do
            produced <- peek producedP
            pure (Right (BSI.fromForeignPtr fp 0 (fromIntegral produced)))
          WF_NEEDS_MORE_OUT -> doublingOutput s sl 0 (cap * 2)
          _                 -> pure (Left "Zstd decompression failed")

------------------------------------------------------------------------
-- Gzip
------------------------------------------------------------------------

-- | Gzip decompress via libz directly.  No upfront output size;
-- starts with an 8× upper bound (typical gzip compression ratio
-- on text + protobuf is 3-8×) and doubles on @NEEDS_MORE_OUT@.
gzipDecompressFromPtr
  :: Ptr Word8
  -> Int
  -> IO (Either String ByteString)
gzipDecompressFromPtr src len = go (max (len * 8) (256 * 1024))
  where
    go cap
      | cap <= 0 = pure (Left "Gzip: output too large")
      | otherwise = do
          fp <- BSI.mallocByteString cap
          alloca $ \producedP -> do
            rc <- withForeignPtr fp $ \dst ->
              c_gzip_inflate_into src (fromIntegral len)
                                   dst (fromIntegral cap) producedP
            case rc of
              WF_OK -> do
                produced <- peek producedP
                pure (Right (BSI.fromForeignPtr fp 0 (fromIntegral produced)))
              WF_NEEDS_MORE_OUT -> go (cap * 2)
              WF_BAD_INPUT      -> pure (Left "Gzip: malformed input")
              WF_INIT_FAIL      -> pure (Left "Gzip: inflateInit2 failed")

------------------------------------------------------------------------
-- LZ4 frame
------------------------------------------------------------------------

-- | LZ4 frame-format decompress via liblz4 directly.  Same doubling
-- strategy as gzip — LZ4 frame headers don't reliably encode the
-- plaintext size (the JVM client + librdkafka omit it).
lz4DecompressFromPtr
  :: Ptr Word8
  -> Int
  -> IO (Either String ByteString)
lz4DecompressFromPtr src len = go (max (len * 4) (64 * 1024))
  where
    go cap
      | cap <= 0 = pure (Left "LZ4: output too large")
      | otherwise = do
          fp <- BSI.mallocByteString cap
          alloca $ \producedP -> alloca $ \consumedP -> do
            rc <- withForeignPtr fp $ \dst ->
              c_lz4f_decompress_into src (fromIntegral len)
                                      dst (fromIntegral cap)
                                      producedP consumedP
            case rc of
              WF_OK -> do
                produced <- peek producedP
                pure (Right (BSI.fromForeignPtr fp 0 (fromIntegral produced)))
              WF_NEEDS_MORE_OUT -> go (cap * 2)
              WF_BAD_INPUT      -> pure (Left "LZ4: malformed frame")
              WF_INIT_FAIL      -> pure (Left "LZ4: dctx alloc failed")

------------------------------------------------------------------------
-- Decompress into a caller-supplied magic ring
------------------------------------------------------------------------

data RingDecompressError
  = RingTooSmall !Int          -- ^ ring can hold at most this many bytes
  | RingBadInput !String
  | RingInitFailed !String
  deriving stock (Show, Generic)

-- | Decompress @len@ bytes from @src@ into the supplied destination
-- magic ring starting at byte offset @0@.  Returns the number of
-- bytes written.  The decompressed bytes occupy
-- @[ringBase dst, ringBase dst + producedLen)@ and are valid for
-- the caller to read directly (zero-copy); the next call to
-- 'decompressIntoRing' against the same ring overwrites them.
--
-- The ring's capacity ('ringSize') is the hard cap on the
-- decompressed output size; if the codec needs more space the
-- call fails with 'RingTooSmall' and the caller should retry
-- against a larger ring (sized to the connection's
-- @receive.message.max.bytes@ or equivalent).
--
-- Snappy and zstd report the expected size up front so we can fail
-- fast.  Gzip and LZ4 are tried once with the full ring capacity;
-- if the decompressor returns @NEEDS_MORE_OUT@ the caller gets
-- 'RingTooSmall' with the current capacity so they can size the
-- next attempt accordingly.
decompressIntoRing
  :: CompressionCodec
  -> Ptr Word8                                   -- ^ src
  -> Int                                         -- ^ src length
  -> MagicRing                                   -- ^ dst
  -> IO (Either RingDecompressError Int)
decompressIntoRing codec src len dst
  | len <= 0  = pure (Right 0)
  | otherwise = do
      let !dstPtr = ringBase dst
          !dstCap = ringSize dst
      case codec of
        NoCompression ->
          if len > dstCap
            then pure (Left (RingTooSmall dstCap))
            else do
              BSI.memcpy dstPtr src len
              pure (Right len)
        Snappy -> snappyIntoRing src len dstPtr dstCap
        Zstd   -> zstdIntoRing   src len dstPtr dstCap
        Gzip   -> gzipIntoRing   src len dstPtr dstCap
        Lz4    -> lz4IntoRing    src len dstPtr dstCap

snappyIntoRing
  :: Ptr Word8 -> Int -> Ptr Word8 -> Int
  -> IO (Either RingDecompressError Int)
snappyIntoRing src len dst cap = do
  outLen <- c_snappy_get_uncompressed_length (ptrToCChar src) (fromIntegral len)
  if outLen == maxBound
    then pure (Left (RingBadInput "Snappy: malformed input header"))
    else do
      let !outLenInt = fromIntegral outLen :: Int
      if outLenInt > cap
        then pure (Left (RingTooSmall cap))
        else do
          ok <- c_snappy_decompress_into (ptrToCChar src) (fromIntegral len)
                                          (ptrToCChar dst) outLen
          if ok == 0
            then pure (Left (RingBadInput "Snappy decompression failed"))
            else pure (Right outLenInt)

zstdIntoRing
  :: Ptr Word8 -> Int -> Ptr Word8 -> Int
  -> IO (Either RingDecompressError Int)
zstdIntoRing src len dst cap = do
  contentSize <- c_zstd_get_frame_content_size src (fromIntegral len)
  let !unknown  = maxBound :: CSize
      !badInput = maxBound - 1 :: CSize
  if contentSize == badInput
    then pure (Left (RingBadInput "Zstd: malformed frame header"))
    else
      let !sz = if contentSize == unknown
                  then fromIntegral cap :: CSize
                  else contentSize
      in if fromIntegral sz > cap
        then pure (Left (RingTooSmall cap))
        else alloca $ \producedP -> do
          rc <- c_zstd_decompress_into src (fromIntegral len) dst sz producedP
          case rc of
            WF_OK -> do
              produced <- peek producedP
              pure (Right (fromIntegral produced))
            WF_NEEDS_MORE_OUT -> pure (Left (RingTooSmall cap))
            WF_BAD_INPUT      -> pure (Left (RingBadInput "Zstd decompression failed"))
            WF_INIT_FAIL      -> pure (Left (RingInitFailed "Zstd init failed"))

gzipIntoRing
  :: Ptr Word8 -> Int -> Ptr Word8 -> Int
  -> IO (Either RingDecompressError Int)
gzipIntoRing src len dst cap = alloca $ \producedP -> do
  rc <- c_gzip_inflate_into src (fromIntegral len)
                             dst (fromIntegral cap) producedP
  case rc of
    WF_OK -> do
      produced <- peek producedP
      pure (Right (fromIntegral produced))
    WF_NEEDS_MORE_OUT -> pure (Left (RingTooSmall cap))
    WF_BAD_INPUT      -> pure (Left (RingBadInput "Gzip: malformed input"))
    WF_INIT_FAIL      -> pure (Left (RingInitFailed "Gzip: inflateInit2 failed"))

lz4IntoRing
  :: Ptr Word8 -> Int -> Ptr Word8 -> Int
  -> IO (Either RingDecompressError Int)
lz4IntoRing src len dst cap = alloca $ \producedP -> alloca $ \consumedP -> do
  rc <- c_lz4f_decompress_into src (fromIntegral len)
                                dst (fromIntegral cap)
                                producedP consumedP
  case rc of
    WF_OK -> do
      produced <- peek producedP
      pure (Right (fromIntegral produced))
    WF_NEEDS_MORE_OUT -> pure (Left (RingTooSmall cap))
    WF_BAD_INPUT      -> pure (Left (RingBadInput "LZ4: malformed frame"))
    WF_INIT_FAIL      -> pure (Left (RingInitFailed "LZ4: dctx alloc failed"))

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

copyPtrToBs :: Ptr Word8 -> Int -> IO ByteString
copyPtrToBs src n = do
  fp <- BSI.mallocByteString n
  withForeignPtr fp $ \dst -> BSI.memcpy dst src n
  pure (BSI.fromForeignPtr fp 0 n)

-- 'Ptr Word8' and 'Ptr CChar' have the same representation; this
-- shim keeps the call sites tidy without sprinkling 'castPtr'.
ptrToCChar :: Ptr Word8 -> Ptr CChar
ptrToCChar = castPtr
