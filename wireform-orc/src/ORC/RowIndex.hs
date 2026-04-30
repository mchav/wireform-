{-# LANGUAGE BangPatterns #-}
-- | ORC per-stripe row index (@ROW_INDEX = 0@ stream).
--
-- Wire format (per @orc_proto.proto@):
--
-- @
-- message RowIndexEntry {
--   repeated uint64 positions  = 1 [packed=true];
--   optional ColumnStatistics statistics = 2;
-- }
--
-- message RowIndex {
--   repeated RowIndexEntry entry = 1;
-- }
-- @
--
-- A row index is emitted on the @ROW_INDEX@ stream once per column once
-- per stripe. Each entry corresponds to one /row group/ (default
-- 10 000 rows). The @positions@ list records the byte offsets within
-- this column's data + present streams that the row group starts at,
-- so a reader can skip directly to the row group that satisfies a
-- predicate.
--
-- This module is a small encoder; reading is the inverse and falls out
-- of the protobuf encoding so we don't need a separate decoder for the
-- common case of writing fresh ORC files.
module ORC.RowIndex
  ( RowIndexEntry (..)
  , encodeRowIndex
  , encodeRowIndexEntry
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word64)

-- | One entry in a row index. @rieStatistics@ carries this row group's
-- min / max / null-count, matching ORC's @ColumnStatistics@ protobuf
-- message; we leave that as an opaque pre-encoded byte-string so this
-- module doesn't have to depend on the full statistics encoding (which
-- varies per type).
data RowIndexEntry = RowIndexEntry
  { riePositions  :: ![Word64]
    -- ^ Byte offsets into the column streams. The number of entries is
    --   determined by the column's encoding (e.g. INT columns have
    --   one position per stream, all of which the reader uses to seek).
  , rieStatistics :: !ByteString
    -- ^ Pre-encoded @ColumnStatistics@ protobuf bytes; pass 'BS.empty'
    --   to omit the statistics field entirely.
  } deriving (Show, Eq)

-- | Encode the @ROW_INDEX@ stream payload from one entry per row group.
encodeRowIndex :: [RowIndexEntry] -> ByteString
encodeRowIndex entries =
  BL.toStrict $ B.toLazyByteString $
    foldMap (\e -> protoLengthDelimited 1 (encodeRowIndexEntry e)) entries

-- | Encode a single 'RowIndexEntry' as the protobuf @RowIndexEntry@
-- message.
encodeRowIndexEntry :: RowIndexEntry -> ByteString
encodeRowIndexEntry rie =
  BL.toStrict $ B.toLazyByteString $
       packedVarintField 1 (riePositions rie)
    <> if BS.null (rieStatistics rie)
         then mempty
         else protoLengthDelimited 2 (rieStatistics rie)

-- ============================================================
-- Protobuf helpers (subset of those in ORC.BloomFilter)
-- ============================================================

protoLengthDelimited :: Int -> ByteString -> B.Builder
protoLengthDelimited fieldNum payload =
     protoTag fieldNum 2
  <> protoVarint (fromIntegral (BS.length payload))
  <> B.byteString payload

packedVarintField :: Int -> [Word64] -> B.Builder
packedVarintField _ [] = mempty
packedVarintField fieldNum xs =
  let !payloadBytes = BL.toStrict $ B.toLazyByteString $
        foldMap protoVarint xs
   in protoTag fieldNum 2
      <> protoVarint (fromIntegral (BS.length payloadBytes))
      <> B.byteString payloadBytes

protoTag :: Int -> Int -> B.Builder
protoTag fieldNum wireType =
  protoVarint (fromIntegral ((fieldNum `shiftL` 3) .|. wireType))

protoVarint :: Word64 -> B.Builder
protoVarint = go
  where
    go !n
      | n < 0x80  = B.word8 (fromIntegral n)
      | otherwise =
          B.word8 (fromIntegral (n .&. 0x7F) .|. 0x80)
            <> go (n `shiftR` 7)

-- shiftR / shiftL referenced via varint helpers; suppress -Widentities.
_unusedShifts :: Word64 -> Word64
_unusedShifts w = (w `shiftR` 0) .|. (w `shiftL` 0)
