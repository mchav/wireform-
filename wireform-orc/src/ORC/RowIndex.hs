{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

{- | ORC per-stripe row index (@ROW_INDEX = 0@ stream).

Wire format (per @orc_proto.proto@):

@
message RowIndexEntry {
  repeated uint64 positions  = 1 [packed=true];
  optional ColumnStatistics statistics = 2;
}

message RowIndex {
  repeated RowIndexEntry entry = 1;
}
@

A row index is emitted on the @ROW_INDEX@ stream once per column once
per stripe. Each entry corresponds to one /row group/ (default
10 000 rows). The @positions@ list records the byte offsets within
this column's data + present streams that the row group starts at,
so a reader can skip directly to the row group that satisfies a
predicate.

This module is a small encoder; reading is the inverse and falls out
of the protobuf encoding so we don't need a separate decoder for the
common case of writing fresh ORC files.
-}
module ORC.RowIndex (
  RowIndexEntry (..),
  encodeRowIndex,
  encodeRowIndexEntry,

  -- * Decoding
  decodeRowIndex,
  decodeRowIndexEntry,
) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word64)
import ORC.Proto.Schema
import Wireform.Builder qualified as B


{- | One entry in a row index. @rieStatistics@ carries this row group's
min / max / null-count, matching ORC's @ColumnStatistics@ protobuf
message; we leave that as an opaque pre-encoded byte-string so this
module doesn't have to depend on the full statistics encoding (which
varies per type).
-}
data RowIndexEntry = RowIndexEntry
  { riePositions :: ![Word64]
  -- ^ Byte offsets into the column streams. The number of entries is
  --   determined by the column's encoding (e.g. INT columns have
  --   one position per stream, all of which the reader uses to seek).
  , rieStatistics :: !ByteString
  -- ^ Pre-encoded @ColumnStatistics@ protobuf bytes; pass 'BS.empty'
  --   to omit the statistics field entirely.
  }
  deriving (Show, Eq)


-- | Encode the @ROW_INDEX@ stream payload from one entry per row group.
encodeRowIndex :: [RowIndexEntry] -> ByteString
encodeRowIndex entries =
  BL.toStrict $
    B.toLazyByteString $
      foldMap
        ( \e ->
            encodeLengthDelimBytes
              RowIndex_Entry
              (encodeRowIndexEntry e)
        )
        entries


{- | Encode a single 'RowIndexEntry' as the protobuf @RowIndexEntry@
message.
-}
encodeRowIndexEntry :: RowIndexEntry -> ByteString
encodeRowIndexEntry rie =
  BL.toStrict $
    B.toLazyByteString $
      encodePackedVarintField RowIndexEntry_Positions (riePositions rie)
        <> if BS.null (rieStatistics rie)
          then mempty
          else
            encodeLengthDelimBytes
              RowIndexEntry_Statistics
              (rieStatistics rie)


{- | Inverse of 'encodeRowIndex': parse the @ROW_INDEX@ stream
payload back into one entry per row group. Useful for
predicate pushdown — the per-row-group min/max statistics
(in 'rieStatistics') tell the reader which row groups can be
skipped, and the 'riePositions' offsets let it seek directly
to the surviving ones.

Returns the entries in encounter order (which matches the
writer's emission order, i.e. row-group order).
-}
decodeRowIndex :: ByteString -> Either String [RowIndexEntry]
decodeRowIndex bs = do
  acc <- decodeMsg bs [] $ \xs (fn, wt) ->
    case (fn, wt) of
      RowIndex_Entry ->
        ReadNested decodeRowIndexEntry (\rie -> rie : xs)
      _ -> SkipUnknown
  Right (reverse acc)


{- | Parse a single @RowIndexEntry@ submessage (the inner
protobuf inside 'decodeRowIndex's outer loop). Exposed for
callers that already hold the per-entry bytes.
-}
decodeRowIndexEntry :: ByteString -> Either String RowIndexEntry
decodeRowIndexEntry bs =
  decodeMsg bs (RowIndexEntry [] BS.empty) $ \rie (fn, wt) ->
    case (fn, wt) of
      RowIndexEntry_Positions ->
        ReadBytes
          ( \payload ->
              rie {riePositions = riePositions rie ++ unpackVarints payload}
          )
      RowIndexEntry_Statistics ->
        ReadBytes (\payload -> rie {rieStatistics = payload})
      _ -> SkipUnknown


{- | Unpack a packed-varint payload into a list of unsigned
64-bit values. ORC encodes @riePositions@ as @packed=true@
so the bytes here are a length-delimited sequence of
back-to-back varints rather than one repeated field.
-}
unpackVarints :: ByteString -> [Word64]
unpackVarints bs = go 0
  where
    !len = BS.length bs
    go !off
      | off >= len = []
      | otherwise =
          let (v, off') = readVarint bs off len
          in v : go off'


readVarint :: ByteString -> Int -> Int -> (Word64, Int)
readVarint bs !off !len = go off 0 0
  where
    go !pos !val !shift
      | pos >= len = (val, pos)
      | otherwise =
          let !b = fromIntegral (BS.index bs pos) :: Word64
              !val' = val .|. ((b .&. 0x7F) `shiftL` shift)
          in if b .&. 0x80 == 0
              then (val', pos + 1)
              else go (pos + 1) val' (shift + 7)
