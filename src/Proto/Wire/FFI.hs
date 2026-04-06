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
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word64)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Unboxed as VU

foreign import ccall unsafe "hs_proto_count_packed_varints"
  c_count_packed_varints :: Ptr () -> CInt -> CInt

foreign import ccall unsafe "hs_proto_packed_all_single_byte"
  c_packed_all_single_byte :: Ptr () -> CInt -> CInt

foreign import ccall unsafe "hs_proto_decode_packed_single_byte_varints"
  c_decode_single_byte_varints :: Ptr () -> CInt -> Ptr Word64 -> CInt

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
