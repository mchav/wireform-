{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Module      : Kafka.Protocol.CRC32C
Description : Fast CRC32C (Castagnoli) checksum implementation
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module provides a fast CRC32C checksum implementation using hardware
acceleration when available, falling back to a software lookup table
implementation on unsupported architectures.

The C implementation is based on https://github.com/corsix/fast-crc32
(MIT License, Copyright (c) 2016 Peter Cawley).

= CRC32C vs CRC32

CRC32C uses the Castagnoli polynomial (0x1EDC6F41), which differs from the
standard CRC32 polynomial (0x04C11DB7). Kafka uses CRC32C for RecordBatch v2
checksums.

= Performance

Hardware acceleration is automatically detected and used when available:

* __x86/x64__: AVX512 + VPCLMULQDQ (Ice Lake+, Genoa+) at ~30-72 GB/s
* __x86/x64__: SSE4.2 CRC32C instructions at ~10-30 GB/s
* __ARM/AArch64__: Hardware CRC32 instructions (ARMv8.1-A+, Apple Silicon) at ~5-10 GB/s
* __Fallback__: Software lookup table at ~1-2 GB/s (all architectures)

The implementation automatically selects the best available method at runtime.

= Example

@
import qualified Data.ByteString as BS
import Kafka.Protocol.CRC32C

-- Simple one-shot checksum
let checksum = crc32c myByteString

-- Incremental checksum (useful for streaming)
let crc = crc32cInit
    crc' = crc32cAppend crc chunk1
    crc'' = crc32cAppend crc' chunk2
    finalChecksum = crc32cFinalize crc''
@
-}
module Kafka.Protocol.CRC32C (
  -- * One-shot checksum
  crc32c,
  crc32cPtr,

  -- * Incremental checksum
  crc32cInit,
  crc32cAppend,
  crc32cFinalize,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as BSU
import Data.Word (Word32, Word8)
import Foreign.C.Types (CChar, CSize (..))
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)


{- | Initialize a CRC32C computation.

Returns the initial CRC value that should be passed to 'crc32cAppend'.
-}
foreign import ccall unsafe "crc32c.h crc32c_init"
  c_crc32c_init :: Word32


{- | Append data to an ongoing CRC32C computation.

This function is safe to call from pure code because the C implementation
is thread-safe (it only reads from the input buffer and uses thread-local
or immutable state for CPU feature detection).
-}
foreign import ccall unsafe "crc32c.h crc32c_append"
  c_crc32c_append :: Word32 -> Ptr Word8 -> CSize -> Word32


{- | Finalize a CRC32C computation.

Takes the current CRC value from 'crc32cAppend' and returns the final
checksum.
-}
foreign import ccall unsafe "crc32c.h crc32c_finalize"
  c_crc32c_finalize :: Word32 -> Word32


{- | Compute CRC32C checksum of a data buffer in one call.

This is the most convenient interface for computing a checksum of a complete
ByteString.
-}
foreign import ccall unsafe "crc32c.h crc32c"
  c_crc32c :: Ptr Word8 -> CSize -> Word32


{- | Initialize a CRC32C computation.

Use this with 'crc32cAppend' and 'crc32cFinalize' for incremental checksum
computation.

@
let crc = crc32cInit
@
-}
crc32cInit :: Word32
crc32cInit = c_crc32c_init
{-# INLINE crc32cInit #-}


{- | Append data to an ongoing CRC32C computation.

This function can be called multiple times to incrementally compute a
checksum over multiple chunks of data.

@
let crc' = crc32cAppend crc chunk1
    crc'' = crc32cAppend crc' chunk2
@
-}
crc32cAppend :: Word32 -> ByteString -> Word32
crc32cAppend crc bs =
  unsafePerformIO $
    BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
      return $! c_crc32c_append crc (castPtr ptr :: Ptr Word8) (fromIntegral len)
{-# INLINE crc32cAppend #-}


{- | Finalize a CRC32C computation.

Takes the current CRC value and returns the final checksum.

@
let finalChecksum = crc32cFinalize crc
@
-}
crc32cFinalize :: Word32 -> Word32
crc32cFinalize = c_crc32c_finalize
{-# INLINE crc32cFinalize #-}


{- | Compute CRC32C checksum of a ByteString.

This is the most convenient function for computing a checksum of a complete
ByteString. Uses the CRC-32C (Castagnoli) polynomial.

Hardware acceleration is automatically detected and used:

* x86/x64: AVX512 or SSE4.2 instructions
* ARM/AArch64: Hardware CRC32 instructions (Apple Silicon, ARMv8.1-A+)
* Fallback: Software lookup table (all architectures)

This function is referentially transparent despite using 'unsafePerformIO'
because the underlying C function is pure and thread-safe.

@
let checksum = crc32c myByteString
@
-}
crc32c :: ByteString -> Word32
crc32c bs =
  unsafePerformIO $
    BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
      return $! c_crc32c (castPtr ptr :: Ptr Word8) (fromIntegral len)
{-# INLINE crc32c #-}


{- | Compute CRC32C of a raw memory range without first wrapping it
in a 'ByteString'. Used by the direct-poke encoder
("Kafka.Protocol.RecordBatchWire") so it can checksum a slice of
its own output buffer without an intermediate copy.

Caller is responsible for keeping the buffer alive across the
call (typically via 'Foreign.ForeignPtr.withForeignPtr').
-}
crc32cPtr :: Ptr Word8 -> Int -> IO Word32
crc32cPtr p n = pure $! c_crc32c p (fromIntegral n)
{-# INLINE crc32cPtr #-}
