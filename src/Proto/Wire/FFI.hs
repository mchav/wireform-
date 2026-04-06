{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- | FFI bindings to the SWAR/SIMD-optimized C decoding primitives
-- in @cbits/fast_decode.c@.
--
-- These supplement the pure-Haskell decoders for hot paths where C
-- can leverage SWAR (SIMD Within A Register) for batch operations on
-- packed fields.
module Proto.Wire.FFI
  ( -- * Packed varint helpers
    countPackedVarints
  , packedAllSingleByte
  , decodeSingleByteVarints

    -- * SWAR UTF-8 validation
  , validateUtf8SWAR

    -- * SWAR varint decode
  , decodeVarintSWAR

    -- * Page boundary relocation
  , relocatePageBoundary
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import qualified Foreign.Marshal.Alloc
import qualified Foreign.Marshal.Array
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr, castPtr)
import qualified Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Unboxed as VU

foreign import ccall unsafe "hs_proto_count_packed_varints"
  c_count_packed_varints :: Ptr () -> CInt -> CInt

foreign import ccall unsafe "hs_proto_packed_all_single_byte"
  c_packed_all_single_byte :: Ptr () -> CInt -> CInt

foreign import ccall unsafe "hs_proto_decode_packed_single_byte_varints"
  c_decode_single_byte_varints :: Ptr () -> CInt -> Ptr Word64 -> CInt

foreign import ccall unsafe "hs_proto_validate_utf8_fast"
  c_validate_utf8_fast :: Ptr () -> CInt -> CInt

foreign import ccall unsafe "hs_proto_decode_varint_swar"
  c_decode_varint_swar :: Ptr () -> CInt -> Ptr Word64 -> CInt

foreign import ccall unsafe "hs_proto_relocate_page_boundary"
  c_relocate_page_boundary :: Ptr () -> CInt -> Ptr () -> CInt -> CInt

-- | Count the number of varints in a packed buffer using SWAR.
-- Each byte with its high bit clear terminates one varint.
countPackedVarints :: ByteString -> Int
countPackedVarints bs = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    pure $! fromIntegral (c_count_packed_varints (castPtr ptr) (fromIntegral len))
{-# INLINE countPackedVarints #-}

-- | Check if every varint in a packed buffer is a single byte (0x00-0x7F).
-- When true, the buffer can be zero-copy decoded by reading bytes directly.
packedAllSingleByte :: ByteString -> Bool
packedAllSingleByte bs = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    pure $! c_packed_all_single_byte (castPtr ptr) (fromIntegral len) /= 0
{-# INLINE packedAllSingleByte #-}

-- | Decode packed single-byte varints into a Vector.
-- Precondition: 'packedAllSingleByte' returned True for this buffer.
-- Each byte is expanded to a Word64 in the output vector.
decodeSingleByteVarints :: ByteString -> VU.Vector Word64
decodeSingleByteVarints bs = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    allocaArray len $ \outPtr -> do
      _ <- pure $! c_decode_single_byte_varints (castPtr ptr) (fromIntegral len) outPtr
      VU.fromList <$> peekArray len outPtr
{-# INLINE decodeSingleByteVarints #-}

-- | Validate UTF-8 using SWAR ASCII fast path.
-- Processes 8 bytes at a time for ASCII (the common case), only entering
-- the full multibyte validator when non-ASCII bytes are found.
validateUtf8SWAR :: ByteString -> Bool
validateUtf8SWAR bs = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    pure $! c_validate_utf8_fast (castPtr ptr) (fromIntegral len) /= 0
{-# INLINE validateUtf8SWAR #-}

-- | SWAR branchless varint decode.
--
-- Ported from hyperpb's number: block in vm/run.go.
-- Loads 8 bytes, XORs sign bits, uses CTZ to find the terminator,
-- masks and compacts in one shot. Zero per-byte branches.
--
-- REQUIRES: at least 8 readable bytes from the given offset.
-- Returns (value, bytesConsumed) or Nothing on overflow.
decodeVarintSWAR :: ByteString -> Int -> Maybe (Word64, Int)
decodeVarintSWAR bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, _len) ->
    alloca $ \outPtr -> do
      let consumed = c_decode_varint_swar (castPtr ptr) (fromIntegral off) outPtr
      if consumed == 0
        then pure Nothing
        else do
          val <- Foreign.Storable.peek outPtr
          pure $! Just (val, fromIntegral consumed)
{-# INLINE decodeVarintSWAR #-}

-- | Pad a buffer for safe 8-byte overreads at any position.
--
-- hyperpb's RelocatePageBoundary: if the end of the buffer is within
-- 7 bytes of a page boundary, returns a copy with 7 bytes of zero
-- padding. Otherwise returns the original buffer unchanged.
--
-- The padding zeros act as varint terminators (byte < 0x80), making
-- SWAR 8-byte loads safe at every position.
relocatePageBoundary :: ByteString -> ByteString
relocatePageBoundary bs
  | BS.null bs = bs
  | otherwise = unsafePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(ptr, len) -> do
        let outLen = len + 7
        outBuf <- Foreign.Marshal.Array.mallocArray outLen
        let result = c_relocate_page_boundary
              (castPtr ptr) (fromIntegral len)
              (castPtr outBuf) (fromIntegral outLen)
        if result == 1
          then BS.packCStringLen (outBuf, len)
          else do
            Foreign.Marshal.Alloc.free outBuf
            pure bs
{-# INLINE relocatePageBoundary #-}
