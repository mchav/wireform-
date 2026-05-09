{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- | Shared helpers for working with @text@'s internal
-- 'Data.Text.Array.Array' (a UTF-8 'ByteArray#' under the
-- hood) — used by both 'Fory.Encode' and 'Fory.Direct' to
-- scan and copy 'Text' bytes without going through
-- 'Data.Text.Encoding.encodeUtf8' (which always allocates
-- a fresh 'ByteString').
module Fory.TextHelpers
  ( byteArrayIsAscii
  , copyTextArrayToPtr
  ) where

import Data.Bits ((.&.))
import qualified Data.Text.Array as TA
import Data.Word (Word8, Word64)
import Foreign.Ptr (Ptr)
import GHC.Ptr (Ptr (Ptr))
import GHC.Exts (Int (I#), Int#, copyByteArrayToAddr#,
                 indexWord64Array#)
import GHC.IO (IO (IO))
import GHC.Word (Word64 (W64#))

-- | Detect whether the bytes @arr[off, end)@ are all ASCII
-- (high bit clear). Word64-stride OR-fold when the offset
-- is 8-aligned (the typical 'T.pack' / 'TE.decodeUtf8'
-- shape — offset 0); falls back to a per-byte recursion
-- when misaligned.
byteArrayIsAscii :: TA.Array -> Int -> Int -> Bool
byteArrayIsAscii !arr !off !end
  | off `rem` 8 == 0 = goWord64Aligned arr off end
  | otherwise        = goPerByte arr off end
{-# INLINABLE byteArrayIsAscii #-}

-- | Per-byte fallback. Recursive but compiles to a tight
-- @indexWord8Array#@ loop.
goPerByte :: TA.Array -> Int -> Int -> Bool
goPerByte !arr !i !end
  | i >= end                       = True
  | TA.unsafeIndex arr i >= 0x80   = False
  | otherwise                      = goPerByte arr (i + 1) end
{-# INLINABLE goPerByte #-}

-- | 'Word64'-stride OR-scan starting from an 8-aligned byte
-- offset.
goWord64Aligned :: TA.Array -> Int -> Int -> Bool
goWord64Aligned arr@(TA.ByteArray ba#) !off !end =
  let !w0 = off `quot` 8
      !wEnd = end `quot` 8
  in goW8 ba# w0 wEnd
  where
    goW8 !ba1# !w !wEnd
      | w >= wEnd = goPerByte arr (w * 8) end
      | otherwise = case indexWord64Array# ba1# (unI# w) of
          x# -> if hasHighBitW64 (W64# x#)
                  then False
                  else goW8 ba1# (w + 1) wEnd

    hasHighBitW64 :: Word64 -> Bool
    hasHighBitW64 w = (w .&. 0x8080808080808080) /= 0
    {-# INLINE hasHighBitW64 #-}

    unI# :: Int -> Int#
    unI# (I# i#) = i#
    {-# INLINE unI# #-}
{-# INLINABLE goWord64Aligned #-}

-- | 'memcpy' from a 'TA.Array' (the 'ByteArray#' that backs
-- Text 2.x) into a raw 'Ptr Word8'. Compiles to a single
-- 'copyByteArrayToAddr#' primop call.
copyTextArrayToPtr :: TA.Array -> Int -> Ptr Word8 -> Int -> IO ()
copyTextArrayToPtr (TA.ByteArray arr#) (I# srcOff#) (Ptr dstAddr#) (I# n#) =
  IO $ \s ->
    case copyByteArrayToAddr# arr# srcOff# dstAddr# n# s of
      s' -> (# s', () #)
{-# INLINE copyTextArrayToPtr #-}
