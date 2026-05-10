{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : Kafka.Compression.Snappy
Description : Snappy compression implementation for Kafka
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

Snappy compression implementation for Kafka using FFI bindings to the C library.
Snappy provides fast compression with moderate compression ratios.
It was widely used in older Kafka deployments but is now being
superseded by LZ4 and Zstd.

Kafka uses the standard Snappy block format (not the framing format).
-}
module Kafka.Compression.Snappy
  ( compressSnappy
  , compressSnappyWithLevel
  , decompressSnappy
  , defaultSnappyLevel
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Foreign.C.Types
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Ptr
import Foreign.Storable

-- | Default Snappy compression level (placeholder).
-- Note: The Haskell snappy bindings don't support configurable compression levels.
-- This constant is provided for API consistency but the level is not used.
defaultSnappyLevel :: Int
defaultSnappyLevel = 0

-- (The 'snappy_max_compressed_length' FFI binding is unused by us;
-- the wrapper calls do their own buffer sizing inside the C side.)

-- | FFI binding to snappy_compress_wrapper
foreign import ccall unsafe "snappy_ffi.h snappy_compress_wrapper"
  c_snappy_compress :: Ptr CChar -> CSize -> Ptr (Ptr CChar) -> Ptr CSize -> IO CSize

-- | FFI binding to snappy_decompress_wrapper
foreign import ccall unsafe "snappy_ffi.h snappy_decompress_wrapper"
  c_snappy_decompress :: Ptr CChar -> CSize -> Ptr (Ptr CChar) -> Ptr CSize -> IO CSize

-- | Compress data using Snappy with default (placeholder) compression level.
-- Uses the standard Snappy block format as required by Kafka.
--
-- An empty input is returned unchanged. All non-empty inputs are passed
-- through Snappy unconditionally so that the inverse (`decompressSnappy`)
-- can be applied without any out-of-band length signalling — which is the
-- contract Kafka's RecordBatch / Message format relies on (the codec ID
-- in the batch header tells the consumer whether to decompress at all,
-- so this layer must be a pure encoding bijection on non-empty inputs).
compressSnappy :: ByteString -> IO (Either String ByteString)
compressSnappy = compressSnappyWithLevel defaultSnappyLevel

-- | Compress data using Snappy with specified compression level.
--
-- Note: This function accepts a compression level parameter for API
-- consistency with other codecs, but the underlying snappy library does
-- not support configurable compression levels. The level parameter is
-- ignored.
compressSnappyWithLevel :: Int -> ByteString -> IO (Either String ByteString)
compressSnappyWithLevel _level bs  -- level ignored
  | BS.null bs = return $ Right BS.empty
  | otherwise = BSU.unsafeUseAsCStringLen bs $ \(inputPtr, inputSize) ->
      alloca $ \outputPtr ->
      alloca $ \outputLen -> do
        result <- c_snappy_compress inputPtr (fromIntegral inputSize) outputPtr outputLen
        if result == 0
          then return $ Left "Snappy compression failed"
          else do
            compressedPtr <- peek outputPtr
            compressedLen <- peek outputLen
            compressedBS <- BS.packCStringLen (compressedPtr, fromIntegral compressedLen)
            free compressedPtr
            return $ Right compressedBS

-- | Decompress Snappy-compressed data. Expects data in the standard
-- Snappy block format (i.e. the output of 'compressSnappy'). An empty
-- input is returned unchanged.
decompressSnappy :: ByteString -> IO (Either String ByteString)
decompressSnappy bs
  | BS.null bs = return $ Right BS.empty
  | otherwise = BSU.unsafeUseAsCStringLen bs $ \(inputPtr, inputLen) ->
      alloca $ \outputPtr ->
      alloca $ \outputLen -> do
        result <- c_snappy_decompress inputPtr (fromIntegral inputLen) outputPtr outputLen
        if result == 0
          then return $ Left "Snappy decompression failed"
          else do
            decompressedPtr <- peek outputPtr
            decompressedLen <- peek outputLen
            decompressedBS <- BS.packCStringLen (decompressedPtr, fromIntegral decompressedLen)
            free decompressedPtr
            return $ Right decompressedBS

