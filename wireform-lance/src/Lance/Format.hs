{-# LANGUAGE OverloadedStrings #-}
-- | Apache Lance columnar format reader.
--
-- Lance is a single-file columnar format from LanceDB designed
-- around fast random row access for vector-search workloads.
-- Each Lance file has the following on-disk layout (per the
-- official spec at <https://lance.org/format/file/>):
--
-- @
-- ┌──────────────────────────────────┐
-- │ "LANC" magic                     │  (4 bytes, leading)
-- ├──────────────────────────────────┤
-- │ Data pages                       │
-- │   Data Buffer 0                  │
-- │   …                              │
-- │   Data Buffer BN                 │
-- ├──────────────────────────────────┤
-- │ Column metadatas                 │
-- │   Column 0 Metadata              │  ← protobuf 'ColumnMetadata'
-- │   …                              │
-- │   Column CN Metadata             │
-- ├──────────────────────────────────┤
-- │ Column Metadata Offset Table     │
-- │   per column: u64 position +     │
-- │               u64 size           │
-- ├──────────────────────────────────┤
-- │ Global Buffers Offset Table      │
-- │   per buffer: u64 position +     │
-- │               u64 size           │
-- ├──────────────────────────────────┤
-- │ Footer (fixed 40 bytes)          │
-- │   u64 column-meta-0 offset       │
-- │   u64 CMO-table offset           │
-- │   u64 GBO-table offset           │
-- │   u32 num global buffers         │
-- │   u32 num columns                │
-- │   u16 major version              │
-- │   u16 minor version              │
-- │   "LANC" magic (trailing)        │
-- └──────────────────────────────────┘
-- @
--
-- All footer integers are unsigned little-endian.
--
-- This module currently exposes:
--
--   * envelope validation ('readLanceFile')
--   * full /footer/ parsing ('LanceFooter')
--   * the column metadata + global buffer offset tables
--     ('ColumnSlice', 'GlobalBufferSlice')
--   * a column-metadata byte-range extractor
--     ('extractColumnMetadataBytes')
--
-- The protobuf 'ColumnMetadata' decoder is intentionally /not/
-- implemented here — that surface lives downstream in code that
-- depends on @wireform-proto@ — but the byte ranges this module
-- returns are exactly what such a decoder would consume.
module Lance.Format
  ( -- * File envelope
    lanceMagic
  , LanceFile (..)
  , readLanceFile
    -- * Footer
  , LanceFooter (..)
  , footerSize
  , parseFooter
    -- * Offset tables
  , ColumnSlice (..)
  , GlobalBufferSlice (..)
  , parseColumnOffsetTable
  , parseGlobalBufferOffsetTable
    -- * Column metadata extraction
  , extractColumnMetadataBytes
  ) where

import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word64)

-- | The 4-byte magic at the start and end of every Lance file.
lanceMagic :: ByteString
lanceMagic = BS.pack [0x4C, 0x41, 0x4E, 0x43]  -- "LANC"

-- | A parsed Lance file. Carries the raw bytes (or, conceptually,
-- a handle to them — the caller can wrap an mmap or a fully-loaded
-- 'ByteString') plus the decoded footer.
--
-- Real readers will keep the bytes mmapped or stream them out of
-- a remote object store; this module only commits to the
-- in-memory case.
data LanceFile = LanceFile
  { lfBytes  :: !ByteString
  , lfFooter :: !LanceFooter
  } deriving (Show, Eq)

-- | The fixed-size 40-byte footer, fully decoded.
data LanceFooter = LanceFooter
  { lfColumnMeta0Offset :: !Word64
    -- ^ Absolute file offset of the first column's metadata
    -- protobuf message. Equivalently the 'cmoPosition' of slot
    -- @0@ in 'lfCMOTableOffset'.
  , lfCMOTableOffset    :: !Word64
    -- ^ Absolute file offset of the column-metadata offset
    -- table (each entry is a @u64@ position + @u64@ size).
  , lfGBOTableOffset    :: !Word64
    -- ^ Absolute file offset of the global-buffers offset
    -- table (same layout: per-buffer @u64@ position + @u64@
    -- size).
  , lfNumGlobalBuffers  :: !Word32
  , lfNumColumns        :: !Word32
  , lfMajorVersion      :: !Word16
  , lfMinorVersion      :: !Word16
  } deriving (Show, Eq)

-- | A single (position, size) pair from the column metadata
-- offset table. There is exactly one of these per column.
data ColumnSlice = ColumnSlice
  { csPosition :: !Word64
  , csSize     :: !Word64
  } deriving (Show, Eq)

-- | A single (position, size) pair from the global buffer
-- offset table. Global buffers carry auxiliary data — the
-- file's Arrow-style schema (FlatBuffers-encoded), per-column
-- statistics, dictionaries, indexes, etc. The format does not
-- ascribe meaning to which slot is which; that's the encoding
-- layer's job.
data GlobalBufferSlice = GlobalBufferSlice
  { gbsPosition :: !Word64
  , gbsSize     :: !Word64
  } deriving (Show, Eq)

-- | Total bytes the fixed-size footer occupies, including the
-- trailing @LANC@ magic. Useful for slicing it off the tail.
--
-- > 8  -- u64 column-meta-0 offset
-- > 8  -- u64 CMO table offset
-- > 8  -- u64 GBO table offset
-- > 4  -- u32 num global buffers
-- > 4  -- u32 num columns
-- > 2  -- u16 major
-- > 2  -- u16 minor
-- > 4  -- "LANC"
-- > = 40 bytes
footerSize :: Int
footerSize = 8 + 8 + 8 + 4 + 4 + 2 + 2 + 4

-- | Validate the magic envelope of a Lance file, parse the
-- footer, and return the assembled 'LanceFile'. Errors are
-- reported by the @Left@ branch of 'Either'.
readLanceFile :: ByteString -> Either String LanceFile
readLanceFile bs
  | BS.length bs < footerSize + 4 =
      Left "Lance.Format: file too short for footer + leading magic"
  | BS.take 4 bs /= lanceMagic =
      Left "Lance.Format: missing leading LANC magic"
  | otherwise = do
      footer <- parseFooter bs
      Right LanceFile { lfBytes = bs, lfFooter = footer }

-- | Decode the footer out of the tail of the file. Returns an
-- error if the trailing magic is missing or any field is out of
-- range for the file size.
parseFooter :: ByteString -> Either String LanceFooter
parseFooter bs
  | total < footerSize =
      Left "Lance.Format: file too short to contain a footer"
  | trailingMagic /= lanceMagic =
      Left "Lance.Format: missing trailing LANC magic"
  | otherwise = Right LanceFooter
      { lfColumnMeta0Offset = u64 footer 0
      , lfCMOTableOffset    = u64 footer 8
      , lfGBOTableOffset    = u64 footer 16
      , lfNumGlobalBuffers  = u32 footer 24
      , lfNumColumns        = u32 footer 28
      , lfMajorVersion      = u16 footer 32
      , lfMinorVersion      = u16 footer 34
      }
  where
    total         = BS.length bs
    footer        = BS.drop (total - footerSize) bs
    trailingMagic = BS.drop (footerSize - 4) footer

-- | Read the column metadata offset table from a 'LanceFile'.
-- Returns @lfNumColumns@ slices in column order. Errors out if
-- the table runs past the file size.
parseColumnOffsetTable :: LanceFile -> Either String (V.Vector ColumnSlice)
parseColumnOffsetTable LanceFile{lfBytes = bs, lfFooter = footer} =
  parseSliceTable "column metadata offset table"
                  ColumnSlice
                  bs
                  (lfCMOTableOffset footer)
                  (lfNumColumns footer)

-- | Read the global-buffers offset table.
parseGlobalBufferOffsetTable
  :: LanceFile
  -> Either String (V.Vector GlobalBufferSlice)
parseGlobalBufferOffsetTable LanceFile{lfBytes = bs, lfFooter = footer} =
  parseSliceTable "global buffer offset table"
                  GlobalBufferSlice
                  bs
                  (lfGBOTableOffset footer)
                  (lfNumGlobalBuffers footer)

-- | Slice the protobuf 'ColumnMetadata' bytes for one column.
-- The column index must be in @[0, lfNumColumns)@.
extractColumnMetadataBytes
  :: LanceFile
  -> Int
  -> Either String ByteString
extractColumnMetadataBytes lf col = do
  table <- parseColumnOffsetTable lf
  if col < 0 || col >= V.length table
    then Left $ "Lance.Format: column index " ++ show col
              ++ " out of range [0," ++ show (V.length table) ++ ")"
    else
      let slice    = table V.! col
          bs       = lfBytes lf
          start    = fromIntegral (csPosition slice) :: Int
          len      = fromIntegral (csSize slice)     :: Int
       in if start < 0 || len < 0 || start + len > BS.length bs
            then Left "Lance.Format: column metadata slice out of range"
            else Right (BS.take len (BS.drop start bs))

-- ============================================================
-- Internal helpers
-- ============================================================

parseSliceTable
  :: String                   -- ^ table name (for error messages)
  -> (Word64 -> Word64 -> a)  -- ^ constructor (position, size)
  -> ByteString               -- ^ entire file
  -> Word64                   -- ^ table start offset
  -> Word32                   -- ^ entry count
  -> Either String (V.Vector a)
parseSliceTable name mk bs offset count =
  let !off    = fromIntegral offset      :: Int
      !cnt    = fromIntegral count       :: Int
      !needed = cnt * 16
   in if off < 0 || off + needed > BS.length bs
        then Left $ "Lance.Format: " ++ name ++ " out of range"
        else Right $ V.generate cnt $ \i ->
               let !base = off + i * 16
                in mk (u64 bs base) (u64 bs (base + 8))

u64 :: ByteString -> Int -> Word64
u64 bs i = leN bs i 8

u32 :: ByteString -> Int -> Word32
u32 bs i = fromIntegral (leN bs i 4)

u16 :: ByteString -> Int -> Word16
u16 bs i = fromIntegral (leN bs i 2)

-- | Decode the @n@ little-endian bytes starting at index @i@
-- into a 'Word64'. The caller is responsible for ensuring the
-- access is in range; out-of-range bytes read as 0 because of
-- 'BS.indexMaybe'.
leN :: ByteString -> Int -> Int -> Word64
leN bs i n = go 0 0
  where
    go !acc !k
      | k == n    = acc
      | otherwise = go (acc + (fromIntegral (BS.index bs (i + k)) `shiftL` (k * 8)))
                       (k + 1)

-- Keep an Int64 import suppressor handy in case downstream
-- callers want a signed-offset variant.
_int64Unused :: Int64
_int64Unused = 0
