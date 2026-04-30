{-# LANGUAGE BangPatterns #-}
-- | WKB (Well-Known Binary) encoding for the V3 @geometry@ and
-- @geography@ Iceberg types.
--
-- The Iceberg V3 spec stores @lower_bound@ / @upper_bound@ for
-- geometry and geography columns as a 2D WKB @POINT@:
--
-- @
--   1 byte   byte order (1 = little-endian, 0 = big-endian)
--   4 bytes  geometry type (1 = POINT)
--   8 bytes  X (longitude, double, IEEE 754 LE)
--   8 bytes  Y (latitude,  double, IEEE 754 LE)
--   --- 21 bytes total ---
-- @
--
-- For the bounding-box lower bound the X / Y are the minimums of all
-- points in the column; for the upper bound, the maximums. Iceberg
-- relies on this representation for partition pruning of geospatial
-- columns.
--
-- Geography uses the same byte layout, with X = longitude and
-- Y = latitude on the WGS-84 ellipsoid.
module Iceberg.Geometry
  ( -- * Points
    Point (..)
    -- * WKB codec
  , wkbEncodePoint
  , wkbDecodePoint
    -- * Convenience
  , wkbPointBytesLittleEndian
  ) where

import Data.Bits (shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64, castWord64ToDouble)

-- | A 2D point. For Iceberg geography columns, @pointX@ is longitude
-- and @pointY@ is latitude.
data Point = Point
  { pointX :: !Double
  , pointY :: !Double
  } deriving (Show, Eq)

-- | Encode a 'Point' as a 21-byte little-endian WKB POINT.
wkbEncodePoint :: Point -> ByteString
wkbEncodePoint p =
  BL.toStrict $ B.toLazyByteString $
       B.word8 1                                   -- byte order: little-endian
    <> B.word32LE 1                                -- WKB type: POINT
    <> B.word64LE (castDoubleToWord64 (pointX p))
    <> B.word64LE (castDoubleToWord64 (pointY p))

-- | Decode a 21-byte WKB POINT (either endianness, type code 1).
wkbDecodePoint :: ByteString -> Either String Point
wkbDecodePoint bs
  | BS.length bs < 21 =
      Left "Iceberg.Geometry.wkbDecodePoint: input shorter than 21 bytes"
  | otherwise =
      let !bo = BS.index bs 0
       in case bo of
            1 -> do
              ty <- readU32 1 LE
              if ty /= 1
                then Left ("wkbDecodePoint: expected POINT (type 1), got type " ++ show ty)
                else do
                  let !x = castWord64ToDouble (readU64 5  LE)
                      !y = castWord64ToDouble (readU64 13 LE)
                  Right (Point x y)
            0 -> do
              ty <- readU32 1 BE
              if ty /= 1
                then Left ("wkbDecodePoint: expected POINT (type 1), got type " ++ show ty)
                else do
                  let !x = castWord64ToDouble (readU64 5  BE)
                      !y = castWord64ToDouble (readU64 13 BE)
                  Right (Point x y)
            _ ->
              Left ("wkbDecodePoint: unknown byte-order flag " ++ show bo)
  where
    readU32 :: Int -> Endian -> Either String Word32
    readU32 off LE = Right $!
      let b0 = fromIntegral (BS.index bs off)        :: Word32
          b1 = fromIntegral (BS.index bs (off + 1)) :: Word32
          b2 = fromIntegral (BS.index bs (off + 2)) :: Word32
          b3 = fromIntegral (BS.index bs (off + 3)) :: Word32
       in     b0
          .&. 0xFF
          + (b1 `shiftL` 8)
          + (b2 `shiftL` 16)
          + (b3 `shiftL` 24)
    readU32 off BE = Right $!
      let b0 = fromIntegral (BS.index bs off)        :: Word32
          b1 = fromIntegral (BS.index bs (off + 1)) :: Word32
          b2 = fromIntegral (BS.index bs (off + 2)) :: Word32
          b3 = fromIntegral (BS.index bs (off + 3)) :: Word32
       in (b0 `shiftL` 24) + (b1 `shiftL` 16) + (b2 `shiftL` 8) + b3

    readU64 :: Int -> Endian -> Word64
    readU64 off LE =
      let bn i = fromIntegral (BS.index bs (off + i)) :: Word64
       in   bn 0
          + (bn 1 `shiftL` 8)
          + (bn 2 `shiftL` 16)
          + (bn 3 `shiftL` 24)
          + (bn 4 `shiftL` 32)
          + (bn 5 `shiftL` 40)
          + (bn 6 `shiftL` 48)
          + (bn 7 `shiftL` 56)
    readU64 off BE =
      let bn i = fromIntegral (BS.index bs (off + i)) :: Word64
       in   (bn 0 `shiftL` 56)
          + (bn 1 `shiftL` 48)
          + (bn 2 `shiftL` 40)
          + (bn 3 `shiftL` 32)
          + (bn 4 `shiftL` 24)
          + (bn 5 `shiftL` 16)
          + (bn 6 `shiftL` 8)
          + bn 7

data Endian = LE | BE

-- | Total length of a little-endian WKB 2D POINT: byte-order (1) +
-- type (4) + x (8) + y (8) = 21.
wkbPointBytesLittleEndian :: Int
wkbPointBytesLittleEndian = 21

-- shiftR is unused but kept available for callers; suppress warning.
_unusedShiftR :: Word64 -> Int -> Word64
_unusedShiftR = shiftR
