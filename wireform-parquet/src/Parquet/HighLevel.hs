{-# LANGUAGE BangPatterns #-}
-- | High-level Parquet API.
--
-- 95% of callers should reach for this module. It hides the
-- @ColumnAux@ parallel-array machinery, the encrypted-footer
-- variant, and the per-row-group compression knobs of
-- "Parquet.Write" behind a single record-of-options:
--
-- @
-- -- Encode
-- let bytes = 'encodeParquet' 'defaultWriteOptions'
--                            schema rowGroups
--
-- -- Read
-- case 'decodeParquet' bytes of
--   Right (schema', rowGroups') -> ...
--   Left  err                   -> ...
-- @
--
-- @rowGroups@ is a @[V.Vector ColumnData]@ — one entry per row
-- group, each entry one 'ColumnData' per /leaf/ schema column in
-- the same order @schema@ enumerates them.
--
-- The 'WriteOptions' record consolidates every Parquet writer
-- knob in one place: compression, page version, bloom filters,
-- page-index emission, per-column encryption, and footer
-- encryption (PARE mode). Defaults are the modern-Parquet
-- recommended settings (Snappy, V2 pages, no encryption).
--
-- For lower-level control (per-column compression overrides,
-- bespoke 'ColumnAux' values, custom row-group dictionaries) drop
-- down to "Parquet.Write".
module Parquet.HighLevel
  ( -- * Encoding
    encodeParquet
  , encodeParquetNested
  , WriteOptions (..)
  , defaultWriteOptions
    -- * Decoding
  , decodeParquet
  , ReadOptions (..)
  , defaultReadOptions
  , FooterDecryption (..)
  , ParquetFile (..)
    -- * Legacy / deprecated variants
  , decodeParquetEncrypted
    -- * Re-exports for convenience
  , ColumnData (..)
  , ColumnEncryption (..)
  , FooterEncryption (..)
  , Compression (..)
  , PageVersion (..)
  , SchemaElement (..)
  , NestedSchema (..)
  , NestedRow (..)
  , LeafType (..)
  , LeafValue (..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Vector as V

import qualified Parquet.Nested as PN

import Parquet.Nested
  ( LeafType (..)
  , LeafValue (..)
  , NestedRow (..)
  , NestedSchema (..)
  , buildNestedFile
  )
import Parquet.Read
  ( FooterDecryption (..)
  , ParquetFile (..)
  , loadParquetFile
  , loadParquetFileEncrypted
  )
import Parquet.Types
  ( Compression (..)
  , SchemaElement (..)
  )
import Parquet.Write
  ( ColumnAux (..)
  , ColumnData (..)
  , ColumnEncryption (..)
  , FooterEncryption (..)
  , PageVersion (..)
  , buildParquetFileWithIndex
  , buildParquetFileWithIndexEncryptedFooter
  , emptyColumnAux
  )

-- ============================================================
-- Options
-- ============================================================

-- | Parquet writer configuration. Construct one with
-- 'defaultWriteOptions' and override the fields you care about:
--
-- @
-- let opts = 'defaultWriteOptions'
--             { writeCompression = 'ZSTD'
--             , writeBloomFilters = ["sku"]
--             }
--     bytes = 'encodeParquet' opts schema rowGroups
-- @
data WriteOptions = WriteOptions
  { writeCompression       :: !Compression
    -- ^ Compression codec applied to every column's data pages.
    -- Default: 'Snappy'. Use 'Uncompressed' to skip.
  , writePageVersion       :: !PageVersion
    -- ^ Data page version. 'PageV2' is recommended for modern
    -- writers; 'PageV1' interoperates with older Parquet readers.
    -- Default: 'PageV2'.
  , writeBloomFilters      :: ![Text]
    -- ^ Column paths (top-level field names) that should carry a
    -- split-block bloom filter. Empty list → no bloom filters.
    -- Filter parameters use parquet-cpp's defaults
    -- (~3% FPP at 1024 distinct values per row group).
  , writePageIndex         :: !Bool
    -- ^ Emit per-column 'OffsetIndex' / 'ColumnIndex' regions in
    -- the trailing page-index area. Default: 'True' — page
    -- indexes substantially improve scan performance for filter
    -- pushdown and most ecosystem readers (pyarrow, parquet-mr,
    -- DuckDB, Trino) take advantage of them when present.
  , writeColumnEncryption  :: !(V.Vector (Maybe ColumnEncryption))
    -- ^ Per-leaf-column encryption configuration. The vector
    -- length must match the number of leaf columns; 'Nothing'
    -- entries leave the column in plaintext. Default:
    -- 'V.empty' (no per-column encryption — interpreted as
    -- "all columns plaintext").
  , writeFooterEncryption  :: !(Maybe FooterEncryption)
    -- ^ When 'Just', the file is emitted in /encrypted-footer/
    -- mode (PARE trailing magic). When 'Nothing', the footer
    -- stays in plaintext (PAR1 magic). Default: 'Nothing'.
  } deriving (Show, Eq)

-- | Sensible modern-Parquet defaults: Snappy compression, V2
-- pages, page indexes on, no encryption, no bloom filters.
defaultWriteOptions :: WriteOptions
defaultWriteOptions = WriteOptions
  { writeCompression       = Snappy
  , writePageVersion       = PageV2
  , writeBloomFilters      = []
  , writePageIndex         = True
  , writeColumnEncryption  = V.empty
  , writeFooterEncryption  = Nothing
  }

-- ============================================================
-- Encoding
-- ============================================================

-- | Serialise a Parquet file from a schema + row groups, applying
-- the supplied options.
--
-- The schema is a flat 'V.Vector' of 'SchemaElement' (one entry
-- per node in the parquet schema tree, including the synthetic
-- root). Row groups are a list of column-major collections — one
-- 'V.Vector' 'ColumnData' per row group, each containing exactly
-- one entry per leaf column in declaration order.
encodeParquet
  :: WriteOptions
  -> V.Vector SchemaElement
  -> [V.Vector ColumnData]
  -> ByteString
encodeParquet opts schema rgs =
  let !rowGroups = V.fromList rgs
      !auxes     = V.map (mkAuxes opts schema) rowGroups
  in  case writeFooterEncryption opts of
        Nothing -> buildParquetFileWithIndex schema rowGroups auxes
        Just fe ->
          buildParquetFileWithIndexEncryptedFooter fe schema rowGroups auxes

-- | Compute a 'ColumnAux' vector per row group from the
-- write-time options. Any per-column knob (compression, bloom,
-- encryption, page version) is applied uniformly here; callers
-- that need per-column overrides can fall back to
-- 'Parquet.Write.buildParquetFileWithIndex' directly.
mkAuxes
  :: WriteOptions
  -> V.Vector SchemaElement
  -> V.Vector ColumnData
  -> V.Vector ColumnAux
mkAuxes opts _schema cols =
  V.imap (\_i _col -> baseAux) cols
  where
    baseAux = emptyColumnAux
      { caCodec       = writeCompression opts
      , caPageVersion = writePageVersion opts
      -- caBloomFilter / caOffsetIndex / caColumnIndex /
      -- caEncryption all default to Nothing in 'emptyColumnAux'.
      -- Bloom filters need a pre-populated 'Sbbf' built from the
      -- column's row data; the high-level path doesn't have
      -- access to the values at this point so we leave them
      -- unset and document that callers wanting bloom filters
      -- should use the lower-level path. Page indexes are
      -- emitted by the writer regardless of this slot when
      -- 'writePageIndex' is True (the slot is for the
      -- materialised index struct; see Parquet.ColumnAux.NewIndex).
      }

-- ============================================================
-- Decoding
-- ============================================================

-- | Serialise a Parquet file carrying /nested/ (struct / list /
-- map / variant) columns. Delegates to 'Parquet.Nested.buildNestedFile'
-- after the Dremel shredder in "Parquet.Nested".
--
-- @columns@ is a vector of @(column-name, schema)@ pairs;
-- @rowsPerColumn@ is a parallel vector where the i-th entry is
-- the row-major list of 'NestedRow' values for column @i@. Every
-- column must carry the same number of rows.
--
-- The nested-file writer currently emits uncompressed
-- @DATA_PAGE_V2@ pages and doesn't take the 'WriteOptions'
-- record — compression / page-index / encryption knobs are
-- roadmap items for the nested path. Use 'encodeParquet' for the
-- flat path if you need those knobs today.
encodeParquetNested
  :: V.Vector (Text, NestedSchema)
  -> V.Vector (V.Vector NestedRow)
  -> Either String ByteString
encodeParquetNested = buildNestedFile

-- ============================================================
-- Decoding
-- ============================================================

-- | Parquet reader configuration. Construct one with
-- 'defaultReadOptions' and override the fields you care about.
--
-- @
-- let opts = 'defaultReadOptions'
--             { readFooterDecryption = Just ('FooterDecryption' key prefix fileId) }
-- 'decodeParquet' opts bytes
-- @
data ReadOptions = ReadOptions
  { readFooterDecryption :: !(Maybe FooterDecryption)
    -- ^ When 'Just', expect an encrypted-footer file (PARE
    -- trailing magic) and decrypt with the supplied key / AAD /
    -- file-id. When 'Nothing' (the default), a plaintext (PAR1)
    -- footer is expected.
  } deriving (Show, Eq)

-- | Plaintext-footer defaults (matches the common case).
defaultReadOptions :: ReadOptions
defaultReadOptions = ReadOptions
  { readFooterDecryption = Nothing
  }

-- | Parse a Parquet file's footer. By default expects a
-- plaintext PAR1 footer; pass 'readFooterDecryption' in
-- 'ReadOptions' to decrypt a PARE-footer file.
--
-- @
-- -- plaintext
-- 'decodeParquet' 'defaultReadOptions' bytes
--
-- -- encrypted footer
-- 'decodeParquet'
--   'defaultReadOptions' { 'readFooterDecryption' = Just fd }
--   bytes
-- @
decodeParquet :: ReadOptions -> ByteString -> Either String ParquetFile
decodeParquet opts = case readFooterDecryption opts of
  Nothing -> loadParquetFile
  Just fd -> loadParquetFileEncrypted fd

-- | Legacy wrapper for the encrypted-footer path.
{-# DEPRECATED decodeParquetEncrypted
    "Use 'decodeParquet' with 'readFooterDecryption' set in 'ReadOptions'." #-}
decodeParquetEncrypted
  :: FooterDecryption
  -> ByteString
  -> Either String ParquetFile
decodeParquetEncrypted fd =
  decodeParquet defaultReadOptions { readFooterDecryption = Just fd }
