{-# LANGUAGE BangPatterns #-}
-- | SIMD-accelerated helpers for Parquet @ColumnIndex.null_pages@.
--
-- @ColumnIndex@ stores a @Vector Bool@ where index @i@ is @True@ when the
-- corresponding page is entirely null. Scan planners want fast answers
-- to two questions:
--
-- 1. /How many/ pages can be skipped because they are all-null?
-- 2. /Which/ pages overlap with a given delete-position range or
--    deletion vector? The complement (\"which pages have at least one
--    non-null row\") is the working set.
--
-- The @null_pages@ vector is dense (one bit per page; tables typically
-- have hundreds to thousands of pages per row group), so packing it as
-- a 1-bit-per-page bitmap and using "Wireform.Hash"'s Roaring
-- 32-bit kernels (or "Columnar.SIMD"'s @popcount@ for non-zero counts)
-- gives a much tighter scan than the boxed boolean vector.
module Parquet.NullPagesBitmap
  ( packNullPages
  , unpackNullPages
  , nullPageCount
  , nonNullPages
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Word (Word8)

import Columnar.SIMD (bitmapPopCount)

-- | Pack a @null_pages@ vector into an LSB-first bitmap, one bit per
-- page (1 = null page, 0 = page has at least one non-null row).
packNullPages :: V.Vector Bool -> ByteString
packNullPages vs =
  let !n = V.length vs
      !numBytes = (n + 7) `shiftR` 3
   in BS.pack [byteAt i | i <- [0 .. numBytes - 1]]
  where
    !n = V.length vs
    byteAt :: Int -> Word8
    byteAt byteIdx =
      let go !bit !acc
            | bit >= 8                = acc
            | byteIdx * 8 + bit >= n  = acc
            | otherwise =
                let !v = V.unsafeIndex vs (byteIdx * 8 + bit)
                    !flag = if v
                              then acc .|. (1 `shiftL` bit)
                              else acc
                 in go (bit + 1) flag
       in fromIntegral (go 0 (0 :: Int))

-- | Inverse of 'packNullPages'.
unpackNullPages :: Int -> ByteString -> V.Vector Bool
unpackNullPages n bs = V.generate n $ \i ->
  let !byteIdx = i `shiftR` 3
      !bit = i .&. 7
      !b = if byteIdx < BS.length bs then BS.index bs byteIdx else 0
   in (b `shiftR` bit) .&. 1 == 1

-- | Number of null pages in the bitmap. Backed by the SIMD
-- 'bitmapPopCount' kernel from "Columnar.SIMD".
nullPageCount :: ByteString -> Int
nullPageCount = bitmapPopCount
{-# INLINE nullPageCount #-}

-- | Indices of pages that are /not/ all-null - i.e. the pages a scan
-- needs to read. Walks 8 bits at a time and uses GHC's @countLeadingZeros@
-- intrinsic for cheap bit-scan, the same pattern the Roaring BITSET
-- decoder uses.
nonNullPages :: Int -> ByteString -> V.Vector Int
nonNullPages totalPages bs = V.fromList (go 0)
  where
    go !i
      | i >= totalPages = []
      | otherwise =
          let !byteIdx = i `shiftR` 3
              !bit = i .&. 7
              !b = if byteIdx < BS.length bs then BS.index bs byteIdx else 0
           in if (b `shiftR` bit) .&. 1 == 1
                then go (i + 1)
                else i : go (i + 1)
