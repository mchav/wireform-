{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Iceberg V3 Variant shredding (parquet-format VariantShredding spec).
--
-- Shredding extracts known fields of a Variant column into typed
-- Parquet sub-columns so query engines can use columnar projection
-- and column statistics for predicate pushdown. A Variant value
-- that matches the shredded type goes to @typed_value@; anything
-- else falls through to @value@ as a regular Variant binary.
--
-- Spec: <https://parquet.apache.org/docs/file-format/types/variantshredding/>.
--
-- Scope of /this/ module: the primitive-shredding case (the most
-- common one in practice). A user picks a 'ShreddedType' for the
-- column; rows are routed to either the typed sub-column or the
-- fallback Variant @value@ column. Object / array shredding
-- (which is a recursive generalisation of this same routing
-- decision) layers on top of primitive shredding once the writer
-- needs it.
module Iceberg.Variant.Shredding
  ( -- * Shredding decision
    ShreddedType (..)
  , ShreddedRow (..)
  , TypedValue (..)
  , routeRow
    -- * Convenience: shredded Parquet column builder
  , buildShreddedVariantParquetFile
    -- * Reading shredded Variants back
  , ShreddedColumn (..)
  , reconstructVariant
  , typedValueToVariant
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Vector as V

import qualified Iceberg.Variant as IV
import qualified Parquet.Nested as PN

-- ============================================================
-- Shredded type
-- ============================================================

-- | The Parquet primitive type a Variant column is being shredded
-- as. Each constructor maps to one of the rows in the spec's
-- "Shredded Value Types" table.
data ShreddedType
  = ShredInt32
  | ShredInt64
  | ShredFloat
  | ShredDouble
  | ShredBool
  | ShredString
  deriving (Show, Eq)

-- | One row's shredding decision. Per the spec table
-- ((value, typed_value) interpretation):
--
-- * 'ShredAsTyped'   - the Variant matched the shredded type;
--   @typed_value@ is set, @value@ is null.
-- * 'ShredAsValue'   - the Variant didn't match; @value@ holds the
--   re-encoded Variant bytes, @typed_value@ is null.
-- * 'ShredMissing'   - the row is missing entirely (both null);
--   only valid for shredded /object fields/, not for top-level
--   Variant columns.
-- * 'ShredVariantNull' - the Variant value is null; @value@ is set
--   to the canonical Variant null byte (basic_type=0,
--   primitive_header=0), @typed_value@ is null.
data ShreddedRow
  = ShredAsTyped !TypedValue
  | ShredAsValue !ByteString          -- ^ re-encoded Variant value bytes
  | ShredMissing
  | ShredVariantNull
  deriving (Show, Eq)

-- | Carrier for the typed sub-column's value. We use a plain ADT
-- rather than a wrapped 'IV.LeafValue' because shredded primitives
-- always go to a fixed Parquet physical type — no need for the
-- decimal/temporal/uuid ceremony at this layer.
data TypedValue
  = TVInt32  !Int32
  | TVInt64  !Int64
  | TVFloat  !Float
  | TVDouble !Double
  | TVBool   !Bool
  | TVString !Text
  deriving (Show, Eq)

-- ============================================================
-- Routing
-- ============================================================

-- | Decide whether a Variant value goes to the typed sub-column or
-- falls through to the @value@ column. The decision matches the
-- spec's "Shredded Value Types" table for primitive shredding.
--
-- Variant integers smaller than the shredded type widen on the way
-- in (e.g. a 'VInt8' shreds to 'TVInt32' when the column is
-- 'ShredInt32'). Variant integers bigger than the shredded type
-- fall through to @value@ — the spec leaves it to the writer to
-- decide whether to lose precision; we always preserve precision
-- and route to the fallback column.
routeRow :: ShreddedType -> Maybe IV.Variant -> ShreddedRow
routeRow _   Nothing         = ShredVariantNull
routeRow _   (Just IV.VNull) = ShredVariantNull
routeRow st  (Just v)        = case (st, v) of
  (ShredInt32, IV.VInt8  i)            -> ShredAsTyped (TVInt32 (fromIntegral i))
  (ShredInt32, IV.VInt16 i)            -> ShredAsTyped (TVInt32 (fromIntegral i))
  (ShredInt32, IV.VInt32 i)            -> ShredAsTyped (TVInt32 i)
  (ShredInt64, IV.VInt8  i)            -> ShredAsTyped (TVInt64 (fromIntegral i))
  (ShredInt64, IV.VInt16 i)            -> ShredAsTyped (TVInt64 (fromIntegral i))
  (ShredInt64, IV.VInt32 i)            -> ShredAsTyped (TVInt64 (fromIntegral i))
  (ShredInt64, IV.VInt64 i)            -> ShredAsTyped (TVInt64 i)
  (ShredFloat,  IV.VFloat f)           -> ShredAsTyped (TVFloat f)
  (ShredDouble, IV.VFloat f)           -> ShredAsTyped (TVDouble (realToFrac f))
  (ShredDouble, IV.VDouble d)          -> ShredAsTyped (TVDouble d)
  (ShredBool,   IV.VBool b)            -> ShredAsTyped (TVBool b)
  (ShredString, IV.VString s)          -> ShredAsTyped (TVString s)
  -- Anything else falls through to the unshredded value column.
  -- We re-encode the Variant value bytes (the metadata stays the
  -- same per the column-wide invariant).
  (_, _) ->
    let (_, valBytes) = IV.encodeVariant v
     in ShredAsValue valBytes

-- ============================================================
-- Parquet writer integration
-- ============================================================

-- | Build a Parquet file with one shredded Variant column. Schema:
--
-- @
-- optional group <name> (VARIANT(1)) {
--   required binary metadata;
--   optional binary value;
--   optional <T>    typed_value;       -- shredded sub-column
-- }
-- @
--
-- The shared metadata used by every row is supplied by the caller;
-- per-row Variants are routed via 'routeRow' to either the
-- @typed_value@ slot or the @value@ slot.
--
-- The 'NestedLeaf' approach we use for the unshredded Variant case
-- doesn't extend cleanly here because the typed sub-column has a
-- different physical type. We compose the file from three parallel
-- 'NestedRow' columns ('metadata', 'value', 'typed_value') with the
-- spec-required co-occurrence guarantees baked into 'routeRow'.
buildShreddedVariantParquetFile
  :: Text                           -- ^ column name
  -> ByteString                     -- ^ shared Variant metadata (used for every row)
  -> ShreddedType
  -> V.Vector (Maybe IV.Variant)    -- ^ row-major Variant data
  -> Either String ByteString
buildShreddedVariantParquetFile colName sharedMeta st rows =
  let !routed = V.map (routeRow st) rows
      !metaCol = V.map (const (PN.NRLeaf (PN.LvBinary sharedMeta))) routed
      !valueCol = V.map valueFor routed
      !typedCol = V.map (typedFor st) routed
      -- Use three parallel /flat/ optional columns rather than a
      -- single nested group: pyarrow / Spark accept both encodings,
      -- and this keeps the writer aligned with the existing nested
      -- file builder. We pick column names that mirror what a
      -- group-shredded reader would see (`<name>.metadata` etc.).
      !schemas = V.fromList
        [ (colName <> ".metadata",
            PN.NSRequired (PN.NSPrimitive PN.LtBinary))
        , (colName <> ".value",
            PN.NSOptional (PN.NSPrimitive PN.LtBinary))
        , (colName <> ".typed_value",
            PN.NSOptional (PN.NSPrimitive (typedLeafType st)))
        ]
      !data_ = V.fromList [metaCol, valueCol, typedCol]
   in PN.buildNestedFile schemas data_

valueFor :: ShreddedRow -> PN.NestedRow
valueFor = \case
  ShredAsValue bs   -> PN.NRLeaf (PN.LvBinary bs)
  ShredVariantNull  -> PN.NRLeaf (PN.LvBinary (BS.pack [0x00]))
                       -- canonical Variant null encoding
  ShredAsTyped _    -> PN.NRNull
  ShredMissing      -> PN.NRNull

typedFor :: ShreddedType -> ShreddedRow -> PN.NestedRow
typedFor st row = case row of
  ShredAsTyped (TVInt32 i)
    | st == ShredInt32 -> PN.NRLeaf (PN.LvInt32 i)
  ShredAsTyped (TVInt64 i)
    | st == ShredInt64 -> PN.NRLeaf (PN.LvInt64 i)
  ShredAsTyped (TVFloat f)
    | st == ShredFloat -> PN.NRLeaf (PN.LvFloat f)
  ShredAsTyped (TVDouble d)
    | st == ShredDouble -> PN.NRLeaf (PN.LvDouble d)
  ShredAsTyped (TVBool b)
    | st == ShredBool -> PN.NRLeaf (PN.LvBool b)
  ShredAsTyped (TVString s)
    | st == ShredString -> PN.NRLeaf (PN.LvString s)
  _ -> PN.NRNull

-- ============================================================
-- Reader: reconstruct a Variant from its shredded representation
-- ============================================================

-- | One row of a shredded Variant column as the spec defines it. The
-- @value@ and @typed_value@ slots are independent 'Maybe's because
-- the spec's value/typed_value matrix gives different semantics for
-- each combination (see 'ShreddedRow' for the encoder side).
--
-- Mirrors the Python @construct_variant@ algorithm's input.
data ShreddedColumn = ShreddedColumn
  { sc_value      :: !(Maybe ByteString)
    -- ^ The unshredded fallback. When non-null, holds the
    --   re-encoded Variant value bytes for the row.
  , sc_typedValue :: !(Maybe TypedValue)
    -- ^ The typed sub-column. When non-null, the row matched the
    --   shredded type.
  } deriving (Show, Eq)

-- | Reconstruct one Variant row from its shredded
-- @(value, typed_value)@ pair, per the spec's @construct_variant@
-- algorithm.
--
-- Per the spec table:
--
-- @
-- value     typed_value   Meaning
-- null      null          Missing (returns Nothing; only valid for
--                         shredded /object fields/, not top-level
--                         Variant columns - top-level callers
--                         should treat Nothing as Variant null).
-- non-null  null          Present, any type (returned via
--                         decodeVariant on the value bytes).
-- null      non-null      Present, the shredded type (returned by
--                         lifting the typed sub-column to a
--                         Variant via 'typedValueToVariant').
-- non-null  non-null      Partially shredded object (not yet
--                         supported in this primitive-shredding
--                         implementation; we return @Left@).
-- @
--
-- For top-level Variant columns, where the spec says missing rows
-- should be returned as Variant null, this function returns
-- @Right Nothing@ for the missing case so the caller can decide
-- between 'IV.VNull' and 'Nothing' depending on context.
reconstructVariant
  :: ByteString
    -- ^ Shared metadata bytes (the 'metadata' column of the
    --   shredded group, the same for every row).
  -> ShreddedColumn
  -> Either String (Maybe IV.Variant)
reconstructVariant meta sc = case (sc_value sc, sc_typedValue sc) of
  (Nothing, Nothing) ->
    Right Nothing
  (Just bs, Nothing) ->
    -- Unshredded fallback: decode the Variant value bytes against
    -- the shared metadata dictionary.
    case IV.decodeVariant meta bs of
      Right v -> Right (Just v)
      Left  e -> Left ("reconstructVariant: " ++ e)
  (Nothing, Just tv) ->
    Right (Just (typedValueToVariant tv))
  (Just _, Just _) ->
    -- Partially-shredded object case (spec §Objects). The typed
    -- sub-column would be a Parquet group of further shredded
    -- fields; this primitive-shredding module doesn't model that
    -- shape, so we surface the situation explicitly.
    Left "reconstructVariant: partially-shredded object \
         \(both value and typed_value present); object \
         \shredding is implemented in a separate code path"

-- | Lift a 'TypedValue' (the typed sub-column's payload) to a
-- 'Variant'. The shredded type uniquely determines which Variant
-- constructor to use.
typedValueToVariant :: TypedValue -> IV.Variant
typedValueToVariant = \case
  TVInt32  i -> IV.VInt32  i
  TVInt64  i -> IV.VInt64  i
  TVFloat  f -> IV.VFloat  f
  TVDouble d -> IV.VDouble d
  TVBool   b -> IV.VBool   b
  TVString s -> IV.VString s

-- ============================================================
-- Internal helpers (writer side)
-- ============================================================

typedLeafType :: ShreddedType -> PN.LeafType
typedLeafType = \case
  ShredInt32  -> PN.LtInt32
  ShredInt64  -> PN.LtInt64
  ShredFloat  -> PN.LtFloat
  ShredDouble -> PN.LtDouble
  ShredBool   -> PN.LtBool
  ShredString -> PN.LtString
