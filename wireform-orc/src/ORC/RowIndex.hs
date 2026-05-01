{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
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

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word64)

import ORC.Proto.Schema

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
    foldMap (\e -> encodeLengthDelimBytes RowIndex_Entry
                     (encodeRowIndexEntry e)) entries

-- | Encode a single 'RowIndexEntry' as the protobuf @RowIndexEntry@
-- message.
encodeRowIndexEntry :: RowIndexEntry -> ByteString
encodeRowIndexEntry rie =
  BL.toStrict $ B.toLazyByteString $
       encodePackedVarintField RowIndexEntry_Positions (riePositions rie)
    <> if BS.null (rieStatistics rie)
         then mempty
         else encodeLengthDelimBytes RowIndexEntry_Statistics
                (rieStatistics rie)
