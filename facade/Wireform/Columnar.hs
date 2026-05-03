{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Unified entry point for wireform's three columnar formats.
--
-- The per-format modules ("Arrow.Stream", "Parquet.HighLevel",
-- "ORC.HighLevel") each expose their own native surface. This
-- module layers a single Arrow-shaped API on top of all three:
-- callers pass an Arrow 'AT.Schema' + a sequence of
-- @'V.Vector' 'AC.ColumnArray'@ batches, pick a 'Format', and
-- get bytes out or bytes in.
--
-- @
-- import qualified Wireform.Columnar as Col
--
-- let bytes = 'encode' Col.Arrow   Col.'defaultWriteOptions' sch batches
--     -- or: 'encode' Col.Parquet Col.'defaultWriteOptions' sch batches
--     -- or: 'encode' Col.ORC     Col.'defaultWriteOptions' sch batches
--
-- case 'decode' Col.Arrow Col.'defaultReadOptions' bytes of
--   Right (sch', batches') -> ...
--   Left  err              -> ...
-- @
--
-- Every format accepts the same inputs (Arrow schema + column
-- batches) and every format returns the same outputs (schema +
-- column batches). Format-specific knobs live on the options
-- records — fields irrelevant to the chosen format are ignored.
-- The per-format modules remain the canonical home for each
-- format's deep feature set (Parquet's bloom filters + page
-- indexes, ORC's stripe encryption, Arrow's streaming reader);
-- this facade handles the 80% case where you want to pick a
-- wire format and move on.
module Wireform.Columnar
  ( -- * Format selection
    Format (..)
    -- * Encoding
  , encode
  , WriteOptions (..)
  , defaultWriteOptions
    -- * Decoding
  , decode
  , ReadOptions (..)
  , defaultReadOptions
    -- * Records (via 'Arrow.Record.Table')
    -- | One-call helpers that lift a 'ArR.Table'-described record
    -- type all the way to / from a columnar wire format. Equivalent
    -- to @'encode' fmt opts schema [cols]@ / the inverse, but lets
    -- callers work in Haskell value space end-to-end.
  , encodeRecords
  , decodeRecords
    -- * Per-format passthroughs
    -- | When callers need format-specific knobs beyond what the
    -- unified options record exposes, drop down to the per-format
    -- modules. 'encode' / 'decode' are deliberately lossy in
    -- exchange for a uniform surface.
  , module Arrow.Stream
  , module Parquet.HighLevel
  , module ORC.HighLevel
  ) where

import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Data.Word (Word64)

import qualified Arrow.Column as AC
import qualified Arrow.Record as ArR
import qualified Arrow.Stream as Arrow
import Arrow.Stream hiding
  ( WriteOptions
  , defaultWriteOptions
  , encodeArrowStream
  , encodeArrowFile
  , decodeArrowStream
  , decodeArrowFile
  )
import qualified Arrow.Types as AT

import qualified ORC.Arrow as OArrow
import qualified ORC.HighLevel as ORC
import ORC.HighLevel hiding
  ( WriteOptions
  , defaultWriteOptions
  , encodeORC
  , decodeORC
  )
import qualified ORC.Read as ORead
import qualified ORC.Stripe as OStripe
import qualified ORC.Types as OT

import qualified Parquet.Arrow as PArrow
import qualified Parquet.HighLevel as Parquet
import Parquet.HighLevel hiding
  ( WriteOptions
  , ReadOptions
  , defaultWriteOptions
  , defaultReadOptions
  , encodeParquet
  , encodeParquetNested
  , decodeParquet
  )

-- ============================================================
-- Format selection
-- ============================================================

-- | Which columnar format 'encode' / 'decode' should use.
data Format
  = Arrow
    -- ^ Apache Arrow IPC /stream/ format (pyarrow's @ipc.new_stream@
    -- shape). Produces a contiguous stream frame; read with
    -- 'Arrow.Stream.decodeArrowStream'.
  | ArrowFile
    -- ^ Apache Arrow IPC /file/ format (@ARROW1@ sentinel +
    -- Footer block indexes). Seek-friendly; read with
    -- 'Arrow.Stream.decodeArrowFile'.
  | Parquet
    -- ^ Apache Parquet. Uses 'Parquet.Arrow.arrowToParquet' to
    -- lower the input batches to flat Parquet columns. Nested
    -- types (struct / list / map) fall through to an error —
    -- for those use 'Parquet.HighLevel.encodeParquetNested'
    -- directly.
  | ORC
    -- ^ Apache ORC. Uses 'ORC.Arrow.arrowToORC' to lower the
    -- input batches; each batch becomes one stripe.
  deriving (Show, Eq, Ord, Enum, Bounded)

-- ============================================================
-- Write options
-- ============================================================

-- | Unified writer configuration. Every field is format-specific;
-- the writer silently ignores fields that don't apply to the
-- chosen 'Format'.
--
-- @
-- let opts = 'defaultWriteOptions'
--             { 'arrowWrite'   = 'Arrow.Stream.defaultWriteOptions'
--                                   { writeBodyCompression = Just BodyZstd }
--             , 'parquetWrite' = 'Parquet.HighLevel.defaultWriteOptions'
--                                   { writeCompression = ZSTD }
--             }
-- 'encode' Arrow opts sch batches
-- @
data WriteOptions = WriteOptions
  { arrowWrite   :: !Arrow.WriteOptions
    -- ^ Used when 'Format' is 'Arrow' or 'ArrowFile'. Body
    -- compression, dictionary-handling strategy.
  , parquetWrite :: !Parquet.WriteOptions
    -- ^ Used when 'Format' is 'Parquet'. Compression codec,
    -- page version, page index, per-column encryption, footer
    -- encryption, bloom filters.
  , orcWrite     :: !ORC.WriteOptions
    -- ^ Used when 'Format' is 'ORC'. Stripe encryption plan.
  }

-- | Format-appropriate sensible defaults.
defaultWriteOptions :: WriteOptions
defaultWriteOptions = WriteOptions
  { arrowWrite   = Arrow.defaultWriteOptions
  , parquetWrite = Parquet.defaultWriteOptions
  , orcWrite     = ORC.defaultWriteOptions
  }

-- ============================================================
-- Read options
-- ============================================================

-- | Unified reader configuration. Currently only Parquet has
-- caller-visible read-time knobs (footer decryption); Arrow and
-- ORC readers self-configure from the file header.
data ReadOptions = ReadOptions
  { parquetRead :: !Parquet.ReadOptions
  }

-- | Empty/default reader options (expects plaintext Parquet
-- footers).
defaultReadOptions :: ReadOptions
defaultReadOptions = ReadOptions
  { parquetRead = Parquet.defaultReadOptions
  }

-- ============================================================
-- Encode
-- ============================================================

-- | Encode an Arrow schema + a list of column batches into a
-- bytestring in the chosen columnar format. The Parquet / ORC
-- paths delegate to 'Parquet.Arrow.arrowToParquet' /
-- 'ORC.Arrow.arrowToORC' for the Arrow-to-format lowering, so the
-- bridges' shape restrictions apply (see the per-format module
-- docs for what's currently supported).
--
-- Returns 'Left' if the format-specific bridge can't represent
-- the input (e.g. nested types in the Parquet flat path,
-- unsupported Arrow types in ORC).
encode
  :: Format
  -> WriteOptions
  -> AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either String ByteString
encode fmt opts sch batches = case fmt of
  Arrow ->
    Right (Arrow.encodeArrowStream (arrowWrite opts) sch batches)
  ArrowFile ->
    Right (Arrow.encodeArrowFile (arrowWrite opts) sch batches)
  Parquet
    -- Any nullable column in the schema? Route through the
    -- mixed writer so nulls round-trip via definition-level
    -- streams. Otherwise use the fast all-required path which
    -- supports compression + page indexes + encryption.
    | anyNullable sch -> do
        (pSchema, pRgs) <- PArrow.arrowToParquetMixed sch batches
        Right (Parquet.encodeParquetMixed (parquetWrite opts) pSchema pRgs)
    | otherwise -> do
        (pSchema, pRgs) <- PArrow.arrowToParquet sch batches
        Right (Parquet.encodeParquet (parquetWrite opts) pSchema pRgs)
  ORC -> do
    (types, stripesWithRows) <- OArrow.arrowToORC sch batches
    ORC.encodeORC (orcWrite opts) types stripesWithRows

-- ============================================================
-- Decode
-- ============================================================

-- | Decode bytes in the given format back into an Arrow schema +
-- column batches. The Parquet / ORC paths reconstruct the Arrow
-- schema from the file's logical/converted-type annotations via
-- 'Parquet.Arrow.parquetFileArrowSchema' / the ORC type table;
-- all row groups / stripes are materialised eagerly.
--
-- For streaming / incremental reads of Arrow files, use
-- 'Arrow.Stream.openStreamReader' directly; for lazy Parquet row
-- group iteration use 'Parquet.Arrow.streamRowGroups'.
decode
  :: Format
  -> ReadOptions
  -> ByteString
  -> Either String (AT.Schema, [V.Vector AC.ColumnArray])
decode fmt opts bs = case fmt of
  Arrow     -> Arrow.decodeArrowStream bs
  ArrowFile -> Arrow.decodeArrowFile   bs
  Parquet -> do
    pf <- Parquet.decodeParquet (parquetRead opts) bs
    let !sch = PArrow.parquetFileArrowSchema pf
    batches <- mapM
      (\i -> case PArrow.parquetRowGroupToArrow sch pf i of
               Right cols -> Right cols
               Left  err  -> Left (show err))
      [0 .. PArrow.numRowGroups pf - 1]
    Right (sch, batches)
  ORC -> do
    footer <- ORC.decodeORC bs
    orcFile <- ORead.loadORCFile bs
    sch <- orcFooterToArrowSchemaWithNullability orcFile footer
    let !numStripes = V.length (orcStripes footer)
    batches <- mapM (\i -> OArrow.orcStripeToArrow sch bs footer i)
                    [0 .. numStripes - 1]
    Right (sch, batches)

-- | Does the schema have at least one nullable leaf? Used to
-- pick between the required-only and mixed Parquet writer
-- paths.
anyNullable :: AT.Schema -> Bool
anyNullable = any AT.fieldNullable . V.toList . AT.arrowFields

-- | Reconstruct an Arrow schema from an ORC footer using the
-- file's stripe footers to derive nullability per leaf. A
-- column is marked nullable iff its first stripe emitted a
-- @PRESENT@ stream for that column id — which is the exact
-- signal the bridge writer leaves behind for nullable Arrow
-- inputs.
--
-- ORC's file-level footer doesn't itself carry a per-column
-- nullability flag (every ORC column is implicitly nullable),
-- so the PRESENT-stream heuristic is the faithful inverse of
-- the bridge's encoder.
orcFooterToArrowSchemaWithNullability
  :: ORead.ORCFile -> ORCFooter -> Either String AT.Schema
orcFooterToArrowSchemaWithNullability orcFile footer = do
  let !types = orcTypes footer
      !root  = V.head types
      !subs  = otSubtypes root
      !names = otFieldNames root
  -- Probe the first stripe for PRESENT streams; ORC writers
  -- emit PRESENT consistently across stripes, so stripe 0 is a
  -- reliable oracle.
  presentCols <- case V.length (orcStripes footer) of
    0 -> Right mempty
    _ -> do
      sf <- ORead.loadStripeFooter orcFile 0
      let pres = [ OStripe.stColumn s
                 | s <- V.toList (OStripe.sfStreams sf)
                 , OStripe.stKind s == orcPresentKind
                 ]
      Right pres
  let !children = V.zipWith
        (\name subIdx ->
            let !leafType = types V.! fromIntegral subIdx
                !nullable = fromIntegral subIdx `elem` presentCols
            in  AT.Field name nullable (orcKindToArrowType leafType) V.empty Nothing)
        names subs
  Right AT.Schema
    { AT.arrowFields = children
    , AT.arrowEndianness = AT.Little
    }
  where
    -- Stream.proto defines PRESENT as kind 0; the OT.Stream ADT
    -- keeps the raw numeric tag. Match against that directly to
    -- avoid a separate dispatch.
    orcPresentKind :: Word64
    orcPresentKind = 0

-- | Map an ORC 'ORCType' back to its Arrow flavour. Mirrors the
-- inverse of 'ORC.Arrow.arrowFieldToORCType'; falls back to
-- 'AT.ABinary' for kinds the bridge doesn't handle yet
-- (callers that care drop down to ORC.HighLevel directly).
orcKindToArrowType :: ORCType -> AT.ArrowType
orcKindToArrowType ot = case otKind ot of
  TKBoolean   -> AT.ABool
  TKByte      -> AT.AInt 8  True
  TKShort     -> AT.AInt 16 True
  TKInt       -> AT.AInt 32 True
  TKLong      -> AT.AInt 64 True
  TKFloat     -> AT.AFloatingPoint AT.Single
  TKDouble    -> AT.AFloatingPoint AT.DoublePrecision
  TKString    -> AT.AUtf8
  TKBinary    -> AT.ABinary
  TKDate      -> AT.ADate AT.DateDay
  TKTimestamp -> AT.ATimestamp AT.Microsecond Nothing
  TKDecimal   -> AT.ADecimal 38 18
  _           -> AT.ABinary

-- ============================================================
-- Record-level helpers
-- ============================================================

-- | Encode a vector of Haskell records as a one-batch file in
-- the chosen columnar format. Schema comes from the 'ArR.Table'
-- instance.
--
-- @
-- bytes \<- either fail pure
--       $ 'encodeRecords' 'Arrow' 'defaultWriteOptions' tradeTable trades
-- @
encodeRecords
  :: Format
  -> WriteOptions
  -> ArR.Table r
  -> V.Vector r
  -> Either String ByteString
encodeRecords fmt opts tbl rs =
  let (sch, cols) = ArR.encodeTable tbl rs
  in  encode fmt opts sch [cols]

-- | Inverse of 'encodeRecords'. Decodes the wire bytes, then
-- dispatches each row group / batch through the 'ArR.Table'
-- instance to reconstruct a flat record vector. Multi-batch
-- files are concatenated.
decodeRecords
  :: Format
  -> ReadOptions
  -> ArR.Table r
  -> ByteString
  -> Either String (V.Vector r)
decodeRecords fmt opts tbl bs = do
  (sch, batches) <- decode fmt opts bs
  perBatch <- traverse (ArR.decodeTable tbl sch) batches
  Right (V.concat perBatch)
