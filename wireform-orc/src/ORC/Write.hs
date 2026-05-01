{-# LANGUAGE BangPatterns #-}

-- | ORC column and file encoding.
--
-- Provides encoders for primitive column types (integer, boolean, float,
-- double, string) and a minimal ORC file builder that concatenates streams
-- into stripes with a proper footer + postscript.
module ORC.Write
  ( -- * Stream encoders
    encodeRLEv2Direct
  , encodeBooleanRLE
  , encodeIntColumn
  , encodeStringDirectColumn
  , encodeFloatColumn
  , encodeDoubleColumn
    -- * Date / timestamp / decimal column encoders
  , encodeDateColumn
  , encodeTimestampColumn
  , encodeDecimalColumn
  , encodeDecimalRawColumn
  , encodeORCNano
    -- * File assembly
  , buildStripe
  , buildORCFile
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Word (Word64, Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32)

import ORC.Footer (orcMagic, writeORCFooter)
import ORC.RLE
  ( bitWidth
  , closestWidth
  , encodeByteRLE
  , encodeWidth
  , packBitsMSB
  , putVulong
  , zigzagEncode
  )
import ORC.Stripe (encodeStripeFooter, encodeStream, Stream (..), StripeFooter (..))
import ORC.Types

------------------------------------------------------------------------
-- RLE v2 Direct encoding
------------------------------------------------------------------------

-- | Encode values as RLE v2 Direct.
--
-- Handles up to 512 values per run. For longer inputs, emits multiple
-- Direct runs.
encodeRLEv2Direct :: VP.Vector Int64 -> Bool -> ByteString
encodeRLEv2Direct vals signed =
  BL.toStrict $ B.toLazyByteString $ goChunks 0
  where
    !n = VP.length vals
    goChunks :: Int -> B.Builder
    goChunks !off
      | off >= n = mempty
      | otherwise =
          let !remaining = n - off
              !chunkLen = min 512 remaining
              !chunk = VP.slice off chunkLen vals
          in encodeDirectChunk chunk signed <> goChunks (off + chunkLen)

encodeDirectChunk :: VP.Vector Int64 -> Bool -> B.Builder
encodeDirectChunk vals signed =
  let !n = VP.length vals
      !transformed = VP.generate n $ \i ->
        let !v = VP.unsafeIndex vals i
        in if signed then zigzagEncode v else fromIntegral v :: Word64
      !maxVal = VP.foldl' max 0 transformed
      !rawW = bitWidth maxVal
      !w = closestWidth rawW
      !encodedW = encodeWidth w
      -- Header: [01][encodedW 5 bits][lenHigh 1 bit]
      !len1 = n - 1
      !lenHigh = (len1 `shiftR` 8) .&. 1
      !lenLow  = len1 .&. 0xFF
      !byte0 = (1 `shiftL` 6) .|. (encodedW `shiftL` 1) .|. lenHigh
      !byte1 = lenLow
      !packed = packBitsMSB transformed w
  in B.word8 (fromIntegral byte0)
     <> B.word8 (fromIntegral byte1)
     <> B.byteString packed

------------------------------------------------------------------------
-- Boolean stream encoding
------------------------------------------------------------------------

-- | Encode a boolean vector as ORC boolean stream (byte-RLE of bit-packed bytes).
encodeBooleanRLE :: V.Vector Bool -> ByteString
encodeBooleanRLE vals =
  let !n = V.length vals
      !numBytes = (n + 7) `quot` 8
      !bytes = VP.generate numBytes $ \bi ->
        let buildByte !bit !acc
              | bit >= 8 = acc
              | otherwise =
                  let !idx = bi * 8 + bit
                      !bitVal = if idx < n && V.unsafeIndex vals idx then 1 else 0 :: Word8
                      !acc' = acc .|. (bitVal `shiftL` (7 - bit))
                  in buildByte (bit + 1) acc'
        in buildByte 0 0
  in encodeByteRLE bytes

------------------------------------------------------------------------
-- Column encoders
------------------------------------------------------------------------

-- | Encode an integer column's DATA stream using RLE v2 Direct.
encodeIntColumn :: VP.Vector Int64 -> Bool -> ByteString
encodeIntColumn = encodeRLEv2Direct

-- | Encode a string column with DIRECT encoding.
-- Returns (DATA stream, LENGTH stream).
encodeStringDirectColumn :: V.Vector T.Text -> (ByteString, ByteString)
encodeStringDirectColumn texts =
  let !encodedTexts = V.map TE.encodeUtf8 texts
      !dataBs = BS.concat (V.toList encodedTexts)
      !lengths = VP.generate (V.length texts) $ \i ->
        fromIntegral (BS.length (V.unsafeIndex encodedTexts i)) :: Int64
      !lengthBs = encodeRLEv2Direct lengths False
  in (dataBs, lengthBs)

-- | Encode a float column (IEEE 754 single, little-endian).
encodeFloatColumn :: VP.Vector Float -> ByteString
encodeFloatColumn vals =
  BL.toStrict $ B.toLazyByteString $ VP.foldl' (\acc v -> acc <> writeFloatLE v) mempty vals

-- | Encode a double column (IEEE 754 double, little-endian).
encodeDoubleColumn :: VP.Vector Double -> ByteString
encodeDoubleColumn vals =
  BL.toStrict $ B.toLazyByteString $ VP.foldl' (\acc v -> acc <> writeDoubleLE v) mempty vals

{-# INLINE writeFloatLE #-}
writeFloatLE :: Float -> B.Builder
writeFloatLE !f =
  let !w = castFloatToWord32 f
  in B.word8 (fromIntegral (w .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 24) .&. 0xFF))

{-# INLINE writeDoubleLE #-}
writeDoubleLE :: Double -> B.Builder
writeDoubleLE !d =
  let !w = castDoubleToWord64 d
  in B.word8 (fromIntegral (w .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 24) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 32) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 40) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 48) .&. 0xFF))
     <> B.word8 (fromIntegral ((w `shiftR` 56) .&. 0xFF))

------------------------------------------------------------------------
-- Date / timestamp / decimal column encoders
------------------------------------------------------------------------

-- | Encode an ORC @DATE@ column (signed days since 1970-01-01) as a single
-- DATA stream using RLE v2. The reader's 'decodeDateColumn' takes the
-- output of this function unchanged.
encodeDateColumn :: VP.Vector Int32 -> ByteString
encodeDateColumn vals =
  encodeRLEv2Direct (VP.map fromIntegral vals) True

-- | Encode an ORC @TIMESTAMP@ column. Returns two streams: the @DATA@
-- stream (signed seconds since 1970-01-01) and the @SECONDARY@ stream
-- (unsigned encoded nanoseconds where the bottom 3 bits hold the
-- trailing-zero scale). Mirrors @decodeTimestampColumn@.
--
-- Each input is @(seconds, nanoValue)@ - the nanos part is the literal
-- nanosecond value (e.g. @123_000_000@ for half a second + 123 ms);
-- 'encodeORCNano' compresses trailing-zero runs into the spec's 3-bit
-- scale field.
encodeTimestampColumn :: VP.Vector Int64 -> VP.Vector Int64 -> (ByteString, ByteString)
encodeTimestampColumn secs nanos =
  let !secStream  = encodeRLEv2Direct secs True
      !nanoVals   = VP.map encodeORCNano nanos
      !nanoStream = encodeRLEv2Direct nanoVals False
   in (secStream, nanoStream)

-- | Compress a nanosecond value into ORC's @nanos@ wire encoding: the
-- bottom 3 bits store the trailing-zero scale (0..7) and the upper bits
-- store the un-scaled nanosecond value. So @123_000_000@ becomes
-- @123 \`shiftL\` 3 .|. 6@ (six trailing zeros).
encodeORCNano :: Int64 -> Int64
encodeORCNano !n
  | n == 0    = 0
  | otherwise =
      let !zeros = countTrailingZeros10 n 0
          !base  = n `quot` (pow10w zeros)
       in (base `shiftL` 3) .|. fromIntegral zeros
  where
    countTrailingZeros10 !v !acc
      | acc >= 7        = acc
      | v `rem` 10 == 0 = countTrailingZeros10 (v `quot` 10) (acc + 1)
      | otherwise       = acc

    pow10w :: Int -> Int64
    pow10w k = case k of
      0 -> 1; 1 -> 10; 2 -> 100; 3 -> 1000; 4 -> 10000
      5 -> 100000; 6 -> 1000000; 7 -> 10000000
      _ -> 100000000

-- | Encode a DECIMAL64 column (precision &le; 18) as a DATA stream of
-- signed unscaled values via RLE v2. The scale is fixed per-column and
-- is recorded on the schema's 'ORCType', not in the data stream. Mirrors
-- 'decodeDecimalColumn'. For DECIMAL128 columns use
-- 'encodeDecimalRawColumn' and the @SECONDARY@ scale stream.
encodeDecimalColumn :: VP.Vector Int64 -> ByteString
encodeDecimalColumn vals = encodeRLEv2Direct vals True

-- | Encode a variable-byte (LEB128-style) DECIMAL128 stream and a
-- corresponding RLE-v2 @SECONDARY@ scale stream. Useful when the writer
-- needs the full DECIMAL spec; the simpler 'encodeDecimalColumn' covers
-- the DECIMAL64 fast path the reader uses today.
encodeDecimalRawColumn
  :: V.Vector Integer  -- ^ Unscaled values.
  -> Int               -- ^ Column scale (constant per column).
  -> (ByteString, ByteString)
encodeDecimalRawColumn vals scale =
  let !dataBs = BL.toStrict $ B.toLazyByteString $
                  V.foldl' (\acc v -> acc <> writeVarSigned v) mempty vals
      !scaleStream =
        encodeRLEv2Direct (VP.replicate (V.length vals) (fromIntegral scale)) False
   in (dataBs, scaleStream)

-- | LEB128-style signed varint used by ORC's @decimal128@ DATA stream.
-- Zig-zag encode then write 7 bits per byte with the MSB as a
-- continuation flag.
writeVarSigned :: Integer -> B.Builder
writeVarSigned !x =
  let !u = if x >= 0 then x `shiftL` 1 else (negate x `shiftL` 1) - 1
   in goVar u
  where
    goVar 0 = B.word8 0
    goVar n = goLoop n
    goLoop !n
      | n < 0x80  = B.word8 (fromIntegral n)
      | otherwise =
          let !low = fromIntegral (n .&. 0x7F) .|. 0x80 :: Word8
           in B.word8 low <> goLoop (n `shiftR` 7)

------------------------------------------------------------------------
-- File assembly
------------------------------------------------------------------------

-- | Build a stripe from stream payloads and stream metadata.
--
-- Concatenates the DATA streams and appends a protobuf stripe footer.
buildStripe :: V.Vector (Word64, Word64, ByteString) -> ByteString
buildStripe streamInfos =
  let !streams = V.map (\(kind, col, bs) ->
        Stream { stKind = kind, stColumn = col, stLength = fromIntegral (BS.length bs) }) streamInfos
      !footer = StripeFooter streams
      !footerBs = encodeStripeFooter footer
      !dataParts = V.toList (V.map (\(_, _, bs) -> bs) streamInfos)
  in BS.concat (dataParts ++ [footerBs])

-- | Build a complete ORC file from type info and stripe data.
--
-- @types@: column types for the schema
-- @stripeData@: for each stripe, a vector of (streamKind, columnId, payload)
buildORCFile :: V.Vector ORCType -> V.Vector (V.Vector (Word64, Word64, ByteString)) -> ByteString
buildORCFile types stripeData =
  let !headerMagic = orcMagic
      !headerLen   = fromIntegral (BS.length headerMagic) :: Word64

      buildStripes :: Int -> Word64 -> V.Vector StripeInformation -> [ByteString]
                   -> (V.Vector StripeInformation, [ByteString])
      buildStripes !i !off !siAcc !bsAcc
        | i >= V.length stripeData = (siAcc, reverse bsAcc)
        | otherwise =
            let !sdata = V.unsafeIndex stripeData i
                !streams = V.map (\(kind, col, bs) ->
                  Stream { stKind = kind, stColumn = col, stLength = fromIntegral (BS.length bs) }) sdata
                !footer = StripeFooter streams
                !footerBs = encodeStripeFooter footer
                !dataLen = V.foldl' (\a (_, _, bs) -> a + fromIntegral (BS.length bs)) 0 sdata :: Word64
                !ftrLen = fromIntegral (BS.length footerBs) :: Word64
                !nRows = 0 -- caller should set proper row counts
                !si = StripeInformation
                  { siOffset = off
                  , siIndexLength = 0
                  , siDataLength = dataLen
                  , siFooterLength = ftrLen
                  , siNumberOfRows = nRows
                  }
                !stripeBs = BS.concat (V.toList (V.map (\(_, _, bs) -> bs) sdata) ++ [footerBs])
                !stripeLen = fromIntegral (BS.length stripeBs) :: Word64
            in buildStripes (i + 1) (off + stripeLen) (V.snoc siAcc si) (stripeBs : bsAcc)

      (!stripeInfos, !stripeBss) = buildStripes 0 headerLen V.empty []
      !contentLen = V.foldl' (\a si -> a + siIndexLength si + siDataLength si + siFooterLength si) 0 stripeInfos

      !footer = ORCFooter
        { orcHeaderLength  = headerLen
        , orcContentLength = contentLen
        , orcStripes       = stripeInfos
        , orcTypes         = types
        , orcMetadata      = V.empty
        , orcNumberOfRows  = V.foldl' (\a si -> a + siNumberOfRows si) 0 stripeInfos
        , orcStatistics    = V.empty
        }
      !footerBytes = writeORCFooter footer
  in BS.concat ([headerMagic] ++ stripeBss ++ [footerBytes])
