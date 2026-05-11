{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : Kafka.Compression.Lz4
Description : LZ4 compression implementation for Kafka
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

LZ4 compression implementation for Kafka using FFI bindings to the C library.
LZ4 provides very fast compression and decompression at the cost
of lower compression ratios compared to gzip or zstd.

Kafka uses the LZ4 frame format (not the block format).
-}
module Kafka.Compression.Lz4
  ( compressLz4
  , compressLz4WithLevel
  , decompressLz4
  , defaultLz4Level
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr
import Foreign.Storable

-- | Default LZ4 compression level (0 = fast mode).
-- Levels range from 0 (fast mode) to 16 (high compression mode).
-- Level 0 prioritizes speed, higher levels prioritize compression ratio.
defaultLz4Level :: Int
defaultLz4Level = 0  -- Fast mode for better throughput

-- | FFI binding to lz4_compress_wrapper_level (with compression level).
-- The previous default-level @c_lz4_compress@ binding is gone since
-- 'c_lz4_compress_level' subsumes it: callers pass
-- 'defaultLz4Level' (0) to get the default-level fast path without
-- the broker noticing.
foreign import ccall unsafe "lz4_ffi.h lz4_compress_wrapper_level"
  c_lz4_compress_level :: Ptr CChar -> CSize -> Ptr (Ptr CChar) -> Ptr CSize -> CInt -> IO CSize

-- | FFI binding to lz4_decompress_wrapper
foreign import ccall unsafe "lz4_ffi.h lz4_decompress_wrapper"
  c_lz4_decompress :: Ptr CChar -> CSize -> Ptr (Ptr CChar) -> Ptr CSize -> IO CSize

-- | Compress data using LZ4 frame format with default compression level.
-- Kafka uses the LZ4 frame format which includes a header and checksum.
compressLz4 :: ByteString -> IO (Either String ByteString)
compressLz4 = compressLz4WithLevel defaultLz4Level

-- | Compress data using LZ4 frame format with specified compression level.
-- Level must be 0-16 (0=fast, higher=better compression but slower).
compressLz4WithLevel :: Int -> ByteString -> IO (Either String ByteString)
compressLz4WithLevel level bs
  | BS.null bs = return $ Right BS.empty  -- Handle empty input
  | otherwise = BSU.unsafeUseAsCStringLen bs $ \(inputPtr, inputLen) ->
      alloca $ \outputPtr ->
      alloca $ \outputLen -> do
        result <- c_lz4_compress_level inputPtr (fromIntegral inputLen) outputPtr outputLen (fromIntegral level)
        if result == 0
          then return $ Left "LZ4 compression failed"
          else do
            compressedPtr <- peek outputPtr
            compressedLen <- peek outputLen
            compressedBS <- BS.packCStringLen (compressedPtr, fromIntegral compressedLen)
            free compressedPtr
            return $ Right compressedBS

-- | Decompress LZ4-compressed data.
-- Expects data in LZ4 frame format.
decompressLz4 :: ByteString -> IO (Either String ByteString)
decompressLz4 bs
  | BS.null bs = return $ Right BS.empty  -- Handle empty input
  | otherwise = BSU.unsafeUseAsCStringLen bs $ \(inputPtr, inputLen) ->
      alloca $ \outputPtr ->
      alloca $ \outputLen -> do
        result <- c_lz4_decompress inputPtr (fromIntegral inputLen) outputPtr outputLen
        if result == 0
          then return $ Left "LZ4 decompression failed"
          else do
            decompressedPtr <- peek outputPtr
            decompressedLen <- peek outputLen
            decompressedBS <- BS.packCStringLen (decompressedPtr, fromIntegral decompressedLen)
            free decompressedPtr
            return $ Right decompressedBS

