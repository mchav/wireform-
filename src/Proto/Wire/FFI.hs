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

    -- * SWAR UTF-8 validation
  , validateUtf8SWAR

    -- * SWAR varint decode
  , decodeVarintSWAR

    -- * Page boundary relocation
  , relocatePageBoundary

    -- * C-native encode primitives
  , encodeLengthDelimitedC
  , encodeVarintFieldC
  , encodeBoolFieldC

    -- * SIMD NUL scanner (for BSON cstrings)
  , findNul
  , findNulBS

    -- * SIMD ASCII check (for all string decoders)
  , isAscii
  , isAsciiBS
  , decodeTextFast

    -- * Endianness helpers (Haskell-side, single MOV + BSWAP)
  , readBE16H
  , readBE32H
  , readBE64H
  , writeBE16H
  , writeBE32H
  , writeBE64H
  , readLE16H
  , readLE32H
  , readLE64H
  , writeLE16H
  , writeLE32H
  , writeLE64H
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word16, Word32, Word64, byteSwap16, byteSwap32, byteSwap64)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import qualified Foreign.Marshal.Alloc
import qualified Foreign.Marshal.Array
import Foreign.Ptr (Ptr, castPtr)
import qualified Foreign.Storable
import Foreign.Storable (peekByteOff, pokeByteOff)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "hs_proto_count_packed_varints"
  c_count_packed_varints :: Ptr () -> CInt -> CInt

foreign import ccall unsafe "hs_proto_packed_all_single_byte"
  c_packed_all_single_byte :: Ptr () -> CInt -> CInt

foreign import ccall unsafe "hs_proto_validate_utf8_fast"
  c_validate_utf8_fast :: Ptr () -> CInt -> CInt

foreign import ccall unsafe "hs_proto_decode_varint_swar"
  c_decode_varint_swar :: Ptr () -> CInt -> Ptr Word64 -> CInt

foreign import ccall unsafe "hs_proto_relocate_page_boundary"
  c_relocate_page_boundary :: Ptr () -> CInt -> Ptr () -> CInt -> CInt

foreign import ccall unsafe "hs_proto_encode_length_delimited"
  c_encode_length_delimited :: Ptr () -> CInt -> Word8 -> Ptr () -> CInt -> CInt

foreign import ccall unsafe "hs_proto_encode_varint_field"
  c_encode_varint_field :: Ptr () -> CInt -> Word8 -> Word64 -> CInt

foreign import ccall unsafe "hs_proto_encode_bool_field"
  c_encode_bool_field :: Ptr () -> CInt -> Word8 -> CInt -> CInt

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

-- | Encode a length-delimited field (tag + length varint + data) in one C call.
encodeLengthDelimitedC :: Ptr Word8 -> Int -> Word8 -> Ptr Word8 -> Int -> IO Int
encodeLengthDelimitedC buf off tag dataPtr dataLen = pure $! fromIntegral $
  c_encode_length_delimited (castPtr buf) (fromIntegral off) tag (castPtr dataPtr) (fromIntegral dataLen)
{-# INLINE encodeLengthDelimitedC #-}

-- | Encode a varint field (tag + varint) in one C call.
encodeVarintFieldC :: Ptr Word8 -> Int -> Word8 -> Word64 -> IO Int
encodeVarintFieldC buf off tag val = pure $! fromIntegral $
  c_encode_varint_field (castPtr buf) (fromIntegral off) tag val
{-# INLINE encodeVarintFieldC #-}

-- | Encode a bool field (tag + 0/1) in one C call. Always 2 bytes.
encodeBoolFieldC :: Ptr Word8 -> Int -> Word8 -> Bool -> IO Int
encodeBoolFieldC buf off tag val = pure $! fromIntegral $
  c_encode_bool_field (castPtr buf) (fromIntegral off) tag (if val then 1 else 0)
{-# INLINE encodeBoolFieldC #-}

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

------------------------------------------------------------------------
-- SIMD NUL scanner
------------------------------------------------------------------------

foreign import ccall unsafe "hs_proto_find_nul"
  c_find_nul :: Ptr () -> CInt -> CInt -> CInt

-- | Find the first NUL byte starting at the given offset within a raw pointer.
-- Returns @Just idx@ (absolute offset from buf start) or @Nothing@.
findNul :: Ptr Word8 -> Int -> Int -> Maybe Int
findNul !ptr !offset !len =
  let !r = c_find_nul (castPtr ptr) (fromIntegral offset) (fromIntegral len)
  in if r < 0 then Nothing else Just (fromIntegral r)
{-# INLINE findNul #-}

-- | Find the first NUL byte in a 'ByteString' starting at the given offset.
-- Returns @Just idx@ (offset into the ByteString) or @Nothing@.
findNulBS :: ByteString -> Int -> Maybe Int
findNulBS bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    pure $! findNul (castPtr ptr) off len
{-# INLINE findNulBS #-}

------------------------------------------------------------------------
-- SIMD ASCII check
------------------------------------------------------------------------

foreign import ccall unsafe "hs_proto_is_ascii"
  c_is_ascii :: Ptr () -> CInt -> CInt -> CInt

-- | Check if @len@ bytes starting at @ptr+offset@ are all ASCII.
isAscii :: Ptr Word8 -> Int -> Int -> Bool
isAscii !ptr !offset !len =
  c_is_ascii (castPtr ptr) (fromIntegral offset) (fromIntegral len) /= 0
{-# INLINE isAscii #-}

-- | Check if a 'ByteString' is entirely ASCII.
isAsciiBS :: ByteString -> Bool
isAsciiBS bs = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
    pure $! isAscii (castPtr ptr) 0 len
{-# INLINE isAsciiBS #-}

-- | Fast text decoding with SIMD ASCII pre-check.
-- When the input is all ASCII, skip UTF-8 validation entirely.
-- Falls back to 'TE.decodeUtf8'' for non-ASCII, with lenient fallback.
decodeTextFast :: ByteString -> Either String Text
decodeTextFast !bs
  | BS.null bs = Right mempty
  | isAsciiBS bs = Right (TE.decodeLatin1 bs)
  | otherwise = case TE.decodeUtf8' bs of
      Right t -> Right t
      Left _  -> Left "invalid UTF-8"
{-# INLINE decodeTextFast #-}

------------------------------------------------------------------------
-- Haskell-side endianness helpers (single MOV + BSWAP on x86)
------------------------------------------------------------------------

readBE16H :: Ptr Word8 -> Int -> IO Word16
readBE16H p off = byteSwap16 <$> peekByteOff p off
{-# INLINE readBE16H #-}

readBE32H :: Ptr Word8 -> Int -> IO Word32
readBE32H p off = byteSwap32 <$> peekByteOff p off
{-# INLINE readBE32H #-}

readBE64H :: Ptr Word8 -> Int -> IO Word64
readBE64H p off = byteSwap64 <$> peekByteOff p off
{-# INLINE readBE64H #-}

writeBE16H :: Ptr Word8 -> Int -> Word16 -> IO ()
writeBE16H p off v = pokeByteOff p off (byteSwap16 v)
{-# INLINE writeBE16H #-}

writeBE32H :: Ptr Word8 -> Int -> Word32 -> IO ()
writeBE32H p off v = pokeByteOff p off (byteSwap32 v)
{-# INLINE writeBE32H #-}

writeBE64H :: Ptr Word8 -> Int -> Word64 -> IO ()
writeBE64H p off v = pokeByteOff p off (byteSwap64 v)
{-# INLINE writeBE64H #-}

readLE16H :: Ptr Word8 -> Int -> IO Word16
readLE16H p off = peekByteOff p off
{-# INLINE readLE16H #-}

readLE32H :: Ptr Word8 -> Int -> IO Word32
readLE32H p off = peekByteOff p off
{-# INLINE readLE32H #-}

readLE64H :: Ptr Word8 -> Int -> IO Word64
readLE64H p off = peekByteOff p off
{-# INLINE readLE64H #-}

writeLE16H :: Ptr Word8 -> Int -> Word16 -> IO ()
writeLE16H p off v = pokeByteOff p off v
{-# INLINE writeLE16H #-}

writeLE32H :: Ptr Word8 -> Int -> Word32 -> IO ()
writeLE32H p off v = pokeByteOff p off v
{-# INLINE writeLE32H #-}

writeLE64H :: Ptr Word8 -> Int -> Word64 -> IO ()
writeLE64H p off v = pokeByteOff p off v
{-# INLINE writeLE64H #-}
