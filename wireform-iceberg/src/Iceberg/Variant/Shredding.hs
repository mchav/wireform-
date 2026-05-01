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
    -- * Reading primitive-shredded Variants back
  , ShreddedColumn (..)
  , reconstructVariant
  , typedValueToVariant
    -- * Object shredding (recursive case)
  , ObjectShreddingSchema (..)
  , ShreddedField (..)
  , ObjectShreddedRow (..)
  , routeObjectRow
  , reconstructObjectVariant
    -- * Array shredding (recursive case)
  , ArrayShreddedRow (..)
  , routeArrayRow
  , reconstructArrayVariant
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
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

-- ============================================================
-- Object shredding (recursive case)
-- ============================================================
--
-- The spec lets an entire object be shredded into a Parquet group
-- of per-field shredded columns. Each named field is itself a
-- (value, typed_value) pair; typed_value can be a primitive (the
-- common case) or recursively a group of further shredded fields.
--
-- Per the spec table (§Objects):
--
--   value        typed_value       Meaning
--   null         non-null          Fully shredded object
--   non-null     non-null          Partially-shredded object
--                                    (value has non-shredded fields only)
--   null         null              Object value is null (Variant null)
--   non-null     null              Not an object (any other Variant)
--
-- Per-field rules:
--
--   value        typed_value       Meaning
--   null         null              Field is missing from the object
--   non-null     null              Field present, may be any type
--   null         non-null          Field present, the shredded type
--   both         null              -- INVALID, see spec
--

-- | Schema for one shredded field: its name plus the
-- 'ShreddedType' (currently only primitive types; recursive group
-- shredding is left as a follow-up).
data ShreddedField = ShreddedField
  { sfName :: !Text
  , sfType :: !ShreddedType
  } deriving (Show, Eq)

-- | Schema for an object-shredded Variant column: an ordered list
-- of fields the writer is going to extract into typed sub-columns.
-- Fields not listed here remain in the unshredded @value@ column.
newtype ObjectShreddingSchema = ObjectShreddingSchema
  { ossFields :: [ShreddedField]
  } deriving (Show, Eq)

-- | One row's object-shredding decision. Mirrors the writer-side
-- shape per the spec:
--
-- * 'ObjFullyShredded': the variant /is/ an object whose fields are
--   /exactly/ the shredded fields (no extras). 'value' is null.
-- * 'ObjPartiallyShredded': the variant is an object with extra
--   non-shredded fields. 'value' carries those extras as a
--   re-encoded Variant object (only the non-shredded fields);
--   'typed_value' carries the shredded ones.
-- * 'ObjNotAnObject': the variant isn't an object; falls through
--   to 'value' wholesale and 'typed_value' is null.
-- * 'ObjVariantNull': the variant value is null (canonical Variant
--   null encoded in 'value', 'typed_value' null).
-- * 'ObjMissing': missing row (both null).
data ObjectShreddedRow = ObjectShreddedRow
  { osrValue       :: !(Maybe ByteString)
    -- ^ Bytes of the unshredded fallback (re-encoded Variant); may
    --   be the canonical Variant-null encoding @0x00@.
  , osrTypedFields :: !(Maybe [(Text, ShreddedRow)])
    -- ^ When the variant was an object (fully or partially
    --   shredded), one entry per shredded field in
    --   'ossFields' order. When the variant wasn't an object,
    --   'Nothing'.
  } deriving (Show, Eq)

-- | Route a Variant value into an 'ObjectShreddedRow' per the spec
-- semantics. The variant's metadata is needed when re-encoding the
-- non-shredded subset for partial shredding.
routeObjectRow
  :: ObjectShreddingSchema
  -> Maybe IV.Variant
  -> ObjectShreddedRow
routeObjectRow _   Nothing         = ObjectShreddedRow Nothing Nothing
routeObjectRow _   (Just IV.VNull) =
  ObjectShreddedRow (Just (BS.pack [0x00])) Nothing
routeObjectRow oss (Just (IV.VObject m)) =
  let !shreddedNames = map sfName (ossFields oss)
      !shreddedSet   = foldr (\(ShreddedField n _) acc ->
                                Map.insert n () acc)
                              Map.empty (ossFields oss)
      !typedFields = map
        (\(ShreddedField n ty) ->
            ( n
            , routeRow ty (Map.lookup n m)
            ))
        (ossFields oss)
      !nonShredded = Map.filterWithKey
        (\k _ -> not (Map.member k shreddedSet)) m
      !valueBytes
        | Map.null nonShredded = Nothing
        | otherwise =
            -- Re-encode the non-shredded subset as a Variant object.
            -- The metadata is a fresh empty-dictionary value plus
            -- the keys; encodeVariant constructs that for us.
            let (_meta, vBytes) = IV.encodeVariant (IV.VObject nonShredded)
             in Just vBytes
      _shredded = shreddedNames -- silence unused
   in ObjectShreddedRow valueBytes (Just typedFields)
routeObjectRow _ (Just other) =
  -- Not an object: typed_value must be null per the spec, and
  -- value carries the entire variant.
  let (_, vBytes) = IV.encodeVariant other
   in ObjectShreddedRow (Just vBytes) Nothing

-- | Reconstruct a Variant from its object-shredded representation.
-- Per the spec's @construct_variant@ algorithm:
--
-- * If 'osrTypedFields' is 'Just', the result is an object whose
--   fields are the union of the shredded typed fields and the
--   non-shredded fields decoded from 'osrValue' (when present).
-- * If 'osrTypedFields' is 'Nothing' and 'osrValue' is 'Just',
--   we decode the value bytes against the metadata.
-- * Otherwise the value is missing/null.
reconstructObjectVariant
  :: ByteString                -- ^ shared metadata bytes
  -> ObjectShreddedRow
  -> Either String (Maybe IV.Variant)
reconstructObjectVariant meta osr = case (osrValue osr, osrTypedFields osr) of
  (Nothing, Nothing) ->
    Right Nothing
  (Just vBytes, Nothing) ->
    -- Whole value lives in the unshredded column; could be Variant
    -- null (the canonical 0x00) or any other type.
    case IV.decodeVariant meta vBytes of
      Right v -> Right (Just v)
      Left  e -> Left ("reconstructObjectVariant: " ++ e)
  (Nothing, Just typed) -> do
    -- Fully shredded object.
    fields <- reconstructTypedFields meta typed
    Right (Just (IV.VObject (Map.fromList fields)))
  (Just vBytes, Just typed) -> do
    -- Partially-shredded object: union the shredded fields with the
    -- decoded non-shredded subset.
    shreddedFields <- reconstructTypedFields meta typed
    decoded <- case IV.decodeVariant meta vBytes of
      Right (IV.VObject m) -> Right m
      Right other ->
        Left ("reconstructObjectVariant: partially-shredded value "
                ++ "must be an object, got " ++ show other)
      Left e -> Left ("reconstructObjectVariant: " ++ e)
    -- Spec requires shredded keys be disjoint from non-shredded;
    -- enforce.
    let !shreddedNames = foldr (\(n, _) acc -> Map.insert n () acc)
                                Map.empty shreddedFields
        !overlap = Map.intersectionWith (\_ _ -> ()) shreddedNames decoded
    if not (Map.null overlap)
      then Left ("reconstructObjectVariant: shredded and non-shredded "
                  ++ "keys must be disjoint; overlap on "
                  ++ show (Map.keys overlap))
      else
        let !merged = Map.union (Map.fromList shreddedFields) decoded
         in Right (Just (IV.VObject merged))

-- | Walk the shredded-field list, reconstructing each field's
-- Variant via 'reconstructVariant', and skip fields that are
-- missing per the spec ('ShreddedColumn Nothing Nothing' on the
-- field).
reconstructTypedFields
  :: ByteString
  -> [(Text, ShreddedRow)]
  -> Either String [(Text, IV.Variant)]
reconstructTypedFields meta = go
  where
    go [] = Right []
    go ((name, row) : rest) = do
      mv <- case row of
        ShredAsTyped tv ->
          reconstructVariant meta
            (ShreddedColumn Nothing (Just tv))
        ShredAsValue bs ->
          reconstructVariant meta
            (ShreddedColumn (Just bs) Nothing)
        ShredVariantNull ->
          reconstructVariant meta
            (ShreddedColumn (Just (BS.pack [0x00])) Nothing)
        ShredMissing ->
          reconstructVariant meta
            (ShreddedColumn Nothing Nothing)
      rest' <- go rest
      case mv of
        Just v  -> Right ((name, v) : rest')
        Nothing -> Right rest'  -- field missing; drop from result

-- ============================================================
-- Array shredding
-- ============================================================
--
-- An array column is shredded as a Parquet 3-level list where
-- each 'element' is a (value, typed_value) pair. Per the spec
-- table:
--
--   value    typed_value      Meaning
--   null     null             Variant null
--   null     non-null (list)  Array; elements live in typed_value
--   non-null null             Not an array; falls through to value
--   non-null non-null         INVALID (writers must not emit)
--
-- Array elements have their own (value, typed_value) per-element
-- pair with the spec's per-element rule: exactly one of value /
-- typed_value must be non-null (no missing elements; null elements
-- use the 0x00 variant-null encoding in 'value').

-- | One row of an array-shredded column.
data ArrayShreddedRow = ArrayShreddedRow
  { asrValue          :: !(Maybe ByteString)
    -- ^ Unshredded fallback: the canonical Variant null bytes
    --   when the row is null, the re-encoded non-array variant
    --   when the row isn't an array, or 'Nothing' when the row
    --   is an array (in which case 'asrTypedElements' is set).
  , asrTypedElements  :: !(Maybe [ShreddedRow])
    -- ^ One ShreddedRow per array element when the row is an
    --   array; 'Nothing' otherwise.
  } deriving (Show, Eq)

-- | Route a Variant value into an 'ArrayShreddedRow'. The element
-- shred type tells us how to route each element (e.g. 'ShredString'
-- means string elements go to typed_value, others fall through).
routeArrayRow :: ShreddedType -> Maybe IV.Variant -> ArrayShreddedRow
routeArrayRow _ Nothing                = ArrayShreddedRow Nothing Nothing
routeArrayRow _ (Just IV.VNull) =
  ArrayShreddedRow (Just (BS.pack [0x00])) Nothing
routeArrayRow elemTy (Just (IV.VArray xs)) =
  let !elements = map (\e -> routeRow elemTy (Just e)) (V.toList xs)
   in ArrayShreddedRow Nothing (Just elements)
routeArrayRow _ (Just other) =
  let (_, vBytes) = IV.encodeVariant other
   in ArrayShreddedRow (Just vBytes) Nothing

-- | Reconstruct a Variant array from its shredded representation.
reconstructArrayVariant
  :: ByteString
  -> ArrayShreddedRow
  -> Either String (Maybe IV.Variant)
reconstructArrayVariant meta asr =
  case (asrValue asr, asrTypedElements asr) of
    (Nothing, Nothing) ->
      Right Nothing
    (Just bs, Nothing) -> case IV.decodeVariant meta bs of
      Right v -> Right (Just v)
      Left  e -> Left ("reconstructArrayVariant: " ++ e)
    (Nothing, Just elems) -> do
      vs <- mapM (reconstructElement meta) elems
      Right (Just (IV.VArray (V.fromList vs)))
    (Just _, Just _) ->
      Left "reconstructArrayVariant: invalid encoding (both value \
           \and typed_value non-null)"
  where
    reconstructElement m row = case row of
      ShredAsTyped tv     -> Right (typedValueToVariant tv)
      ShredAsValue bs     -> case IV.decodeVariant m bs of
        Right v -> Right v
        Left  e -> Left ("reconstructArrayVariant: element decode: " ++ e)
      ShredVariantNull    -> Right IV.VNull
      ShredMissing        ->
        -- Per the spec, array elements must be present; this is
        -- a malformed input. The closest valid interpretation is
        -- a Variant null element, but we surface the error so
        -- callers can detect it.
        Left "reconstructArrayVariant: missing element \
             \(arrays must not contain missing entries)"

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
