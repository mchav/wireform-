{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | Direct FFI bindings to @liblz4@ for the raw LZ4 block
-- format. No framing, no size headers — exactly the byte
-- layout that Apache Parquet's @LZ4_RAW@ codec (codec id 7)
-- and Apache ORC's @LZ4@ stream codec specify.
--
-- The Haskell @lz4@ package on Hackage wraps its compress /
-- decompress in its own 8-byte size header (matching the C
-- reference's demo @LZ4_compress_default@ -> @LZ4_uncompress@
-- contract); that's wire-incompatible with both Parquet and
-- ORC. We bind the C functions directly so we own the I/O
-- shape.
--
-- Build requires @liblz4-dev@ (or your distribution's
-- equivalent) at link time. The @-flz4@ Cabal flag (default
-- on) controls whether this module is exposed.
--
-- == Functions used
--
-- @
-- int LZ4_compressBound(int inputSize);
-- int LZ4_compress_default(const char* src, char* dst,
--                          int srcSize, int dstCapacity);
-- int LZ4_decompress_safe(const char* src, char* dst,
--                         int compressedSize, int dstCapacity);
-- @
--
-- == Test coverage
--
-- Round-trip property tests over random inputs in
-- @wireform-columnar-test/Main.hs@. End-to-end interop with
-- pyarrow / duckdb / polars Parquet and pyarrow ORC files
-- exercised by the probes under
-- @wireform-{parquet,orc}/scripts/@.
module Columnar.LZ4
  ( -- * Block codec
    decompress
  , compress
    -- * Errors
  , LZ4Error (..)
  , prettyLZ4Error
    -- * Internals (exposed for tests)
  , compressBound
  ) where

import Control.Exception (mask_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Foreign.C.Types (CChar, CInt (..))
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)

-- ============================================================
-- FFI
-- ============================================================

foreign import ccall unsafe "LZ4_compressBound"
  c_LZ4_compressBound :: CInt -> CInt

foreign import ccall unsafe "LZ4_compress_default"
  c_LZ4_compress_default
    :: Ptr CChar  -- src
    -> Ptr CChar  -- dst
    -> CInt       -- srcSize
    -> CInt       -- dstCapacity
    -> IO CInt    -- compressedSize, or 0 on failure

foreign import ccall unsafe "LZ4_decompress_safe"
  c_LZ4_decompress_safe
    :: Ptr CChar  -- src
    -> Ptr CChar  -- dst
    -> CInt       -- compressedSize
    -> CInt       -- dstCapacity
    -> IO CInt    -- decompressedSize, or negative on failure

-- ============================================================
-- Errors
-- ============================================================

data LZ4Error
  = LZ4DecodeFailed !Int  -- ^ liblz4 returned a negative size; the value is the magnitude.
  | LZ4OutputOverrun      -- ^ Caller-supplied @maxOutput@ was negative.
  | LZ4CompressFailed     -- ^ liblz4 returned 0 from compress (bound too small).
  deriving (Show, Eq)

prettyLZ4Error :: LZ4Error -> String
prettyLZ4Error = \case
  LZ4DecodeFailed n ->
    "LZ4: decompression failed at byte " ++ show n
      ++ " of the compressed input (malformed block or truncated)"
  LZ4OutputOverrun ->
    "LZ4: caller-supplied maxOutput was negative"
  LZ4CompressFailed ->
    "LZ4: compression failed (output bound was too small for the input)"

-- ============================================================
-- Public API
-- ============================================================

-- | Worst-case compressed size for an input of @n@ bytes.
-- Mirrors @LZ4_compressBound@. Returns @-1@ if @n@ is
-- non-positive or larger than @LZ4_MAX_INPUT_SIZE@.
compressBound :: Int -> Int
compressBound !n = fromIntegral (c_LZ4_compressBound (fromIntegral n))

-- | Decode one raw LZ4 block.
--
-- @decompress maxOutput input@ asks liblz4 to expand @input@
-- into a buffer of size @maxOutput@. On success returns the
-- decoded bytes (a slice of that buffer); on failure returns
-- a diagnostic string.
--
-- @maxOutput@ should be the value the caller already knows
-- from upstream metadata — Parquet's page header has
-- @uncompressed_page_size@; ORC's PostScript has
-- @compressionBlockSize@. Passing a value that's too small
-- causes liblz4 to fail (it won't truncate); passing too
-- large just allocates extra memory.
decompress :: Int -> ByteString -> Either String ByteString
decompress !maxOutput !input
  | maxOutput < 0 = Left (prettyLZ4Error LZ4OutputOverrun)
  | BS.null input && maxOutput == 0 = Right BS.empty
  | BS.null input = Right BS.empty
  | otherwise = unsafePerformIO $ mask_ $ do
      -- mask_ + mallocForeignPtrBytes: we want the buffer to
      -- exist for the duration of the C call without GC
      -- moving it. ByteString.fromForeignPtr keeps it alive
      -- for the slice's lifetime.
      fptr <- mallocForeignPtrBytes maxOutput
      withForeignPtr fptr $ \dstPtr ->
        BSU.unsafeUseAsCStringLen input $ \(srcPtr, srcLen) -> do
          rc <- c_LZ4_decompress_safe
                  srcPtr
                  (castPtr dstPtr)
                  (fromIntegral srcLen)
                  (fromIntegral maxOutput)
          if rc < 0
            then pure (Left (prettyLZ4Error (LZ4DecodeFailed
                  (fromIntegral (negate rc)))))
            else pure (Right (BSI.fromForeignPtr
                  fptr 0 (fromIntegral rc)))

-- | Encode one raw LZ4 block.
--
-- Output is byte-identical to what @LZ4_compress_default@
-- writes, so any reference decoder (the C reference,
-- arrow-rs, parquet-mr, parquet-cpp) accepts it without
-- needing a frame parser.
compress :: ByteString -> ByteString
compress !input
  | BS.null input = BS.empty
  | otherwise = unsafePerformIO $ mask_ $ do
      let !srcLen = BS.length input
          !dstCap = compressBound srcLen
      if dstCap <= 0
        then pure BS.empty  -- liblz4 says input is too large; surface as empty.
        else do
          fptr <- mallocForeignPtrBytes dstCap
          withForeignPtr fptr $ \dstPtr ->
            BSU.unsafeUseAsCStringLen input $ \(srcPtr, _) -> do
              written <- c_LZ4_compress_default
                           srcPtr
                           (castPtr dstPtr)
                           (fromIntegral srcLen)
                           (fromIntegral dstCap)
              if written <= 0
                then pure BS.empty
                else pure (BSI.fromForeignPtr fptr 0 (fromIntegral written))
