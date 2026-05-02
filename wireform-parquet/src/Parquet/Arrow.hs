{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Arrow ↔ Parquet column-data bridge.
--
-- Lets callers keep a single in-memory representation
-- ('Arrow.Column.ColumnArray') and choose Parquet at the wire-
-- format level, mirroring how
-- @pyarrow.parquet.write_table(table, path)@ works in Python: a
-- 'pa.Table' goes in, Parquet bytes come out.
--
-- @
-- -- Arrow → Parquet
-- let !(schema, rowGroups) = 'arrowToParquet' arrowSchema arrowBatches
--     bytes                = 'Parquet.HighLevel.encodeParquet'
--                              'Parquet.HighLevel.defaultWriteOptions'
--                              schema rowGroups
--
-- -- Parquet → Arrow (one row group at a time)
-- pf <- 'Parquet.HighLevel.decodeParquet' bytes
-- batch <- 'parquetRowGroupToArrow' arrowSchema pf 0
-- @
--
-- The bridge currently covers the flat-primitive shape Parquet's
-- writer ('Parquet.Write.ColumnData') natively supports: 'Int8',
-- 'Int16', 'Int32', 'Int64', 'UInt8'..'UInt64', 'Float', 'Double',
-- 'Bool', 'Utf8', 'Binary', plus their nullable variants. Nested
-- columns (struct / list / map / union / dictionary / view / REE)
-- aren't in Parquet's flat data plane, so they fall through to a
-- Left at translation time. Support for nested shredding via
-- "Parquet.Nested" is a separate item.
module Parquet.Arrow
  ( -- * Arrow → Parquet
    arrowToParquet
  , columnArrayToColumnData
    -- * Parquet → Arrow
  , parquetRowGroupToArrow
  , readParquetColumn
    -- * Streaming reader (one row group at a time)
  , streamRowGroups
  , numRowGroups
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Arrow.Column as AC
import qualified Arrow.Types as AT

import qualified Parquet.Read as PR
import qualified Parquet.Types as P
import qualified Parquet.Write as PW

-- ============================================================
-- Arrow → Parquet
-- ============================================================

-- | Lower an Arrow schema + a sequence of column-major batches
-- to the inputs 'Parquet.HighLevel.encodeParquet' expects.
--
-- Each Arrow batch becomes one Parquet row group. Returns 'Left'
-- if any column type isn't representable in Parquet's flat data
-- plane (struct / list / dictionary / view / REE — see the
-- module docs for the supported subset).
arrowToParquet
  :: AT.Schema
  -> [V.Vector AC.ColumnArray]
  -> Either String (V.Vector P.SchemaElement, [V.Vector PW.ColumnData])
arrowToParquet sch batches = do
  let !leafFields = arrowFieldsToLeaves (AT.arrowFields sch)
      !rootElem   = P.SchemaElement
                      { P.seName        = "schema"
                      , P.seRepetition  = Nothing
                      , P.seType        = Nothing
                      , P.seNumChildren = Just (fromIntegral (V.length leafFields))
                      , P.seConvertedType = Nothing
                      , P.seLogicalType = Nothing
                      , P.seFieldId     = Nothing
                      }
  schemaElems <- V.mapM arrowFieldToSchemaElement leafFields
  let !pSchema = V.cons rootElem schemaElems
  rgData <- mapM (V.mapM columnArrayToColumnData) batches
  Right (pSchema, rgData)

-- | Project the (potentially-nested) Arrow field tree to a flat
-- list of leaves. Today we only support flat schemas (the bridge
-- doesn't round-trip nested types); nested fields fall through to
-- 'columnArrayToColumnData' which then reports a clean 'Left'.
arrowFieldsToLeaves :: V.Vector AT.Field -> V.Vector AT.Field
arrowFieldsToLeaves = V.filter (V.null . AT.fieldChildren)

-- | Translate an Arrow leaf 'Field' into a Parquet
-- 'SchemaElement'.
arrowFieldToSchemaElement
  :: AT.Field -> Either String P.SchemaElement
arrowFieldToSchemaElement f = do
  pType <- case AT.fieldType f of
    AT.AInt 8  _      -> Right P.PTInt32
    AT.AInt 16 _      -> Right P.PTInt32
    AT.AInt 32 _      -> Right P.PTInt32
    AT.AInt 64 _      -> Right P.PTInt64
    AT.ABool          -> Right P.PTBoolean
    AT.AFloatingPoint AT.Single          -> Right P.PTFloat
    AT.AFloatingPoint AT.DoublePrecision -> Right P.PTDouble
    AT.AUtf8          -> Right P.PTByteArray
    AT.ABinary        -> Right P.PTByteArray
    AT.ALargeUtf8     -> Right P.PTByteArray
    AT.ALargeBinary   -> Right P.PTByteArray
    -- Temporal types: map to INT32 (Date, Time-millis) or INT64
    -- (Date64, Time-micros/nanos, Timestamp, Duration). Logical
    -- / converted types are set below.
    AT.ADate AT.DateDay         -> Right P.PTInt32
    AT.ADate AT.DateMillisecond -> Right P.PTInt64
    AT.ATime _ 32     -> Right P.PTInt32
    AT.ATime _ 64     -> Right P.PTInt64
    AT.ATimestamp _ _ -> Right P.PTInt64
    AT.ADuration _    -> Right P.PTInt64
    other ->
      Left $ "Parquet.Arrow: Arrow type "
             <> show other <> " has no Parquet flat-primitive equivalent"
  let !rep = if AT.fieldNullable f then P.Optional else P.Required
      !logical = case AT.fieldType f of
        AT.AUtf8           -> Just P.LTString
        AT.ALargeUtf8      -> Just P.LTString
        AT.ADate _         -> Just P.LTDate
        -- Arrow Time / Timestamp / Duration map to Parquet's
        -- @TIME(unit, isAdjustedToUTC)@ / @TIMESTAMP(unit, utc)@
        -- logical types. We only record the converted-type
        -- fallback here; the LogicalType-specific fields
        -- (precision, scale, isUtc) aren't exposed by
        -- Parquet.Types.LogicalType yet — a separate item.
        _                  -> Nothing
  Right P.SchemaElement
    { P.seName          = AT.fieldName f
    , P.seRepetition    = Just rep
    , P.seType          = Just pType
    , P.seNumChildren   = Nothing
    , P.seConvertedType = case AT.fieldType f of
        AT.AUtf8       -> Just P.CTUtf8
        AT.ALargeUtf8  -> Just P.CTUtf8
        AT.ADate _     -> Just P.CTDate
        AT.ATime _ 32  -> Just P.CTTimeMillis
        AT.ATime _ 64  -> Just P.CTTimeMicros
        AT.ATimestamp AT.Millisecond _ -> Just P.CTTimestampMillis
        AT.ATimestamp AT.Microsecond _ -> Just P.CTTimestampMicros
        _              -> Nothing
    , P.seLogicalType   = logical
    , P.seFieldId       = Nothing
    }

-- | Lower one Arrow column to Parquet's 'ColumnData'. Nullable
-- variants are flattened to the present-only values vector with
-- nulls dropped — Parquet's 'ColumnData' is the non-nullable
-- shape; the 'Parquet.Write.OptionalColumn' path handles nullable
-- variants but the high-level API doesn't currently route through
-- it. Future work: return an @Either ColumnData OptionalColumn@
-- so callers can select the right Parquet writer.
columnArrayToColumnData
  :: AC.ColumnArray -> Either String PW.ColumnData
columnArrayToColumnData = \case
  AC.ColInt8   v -> Right $ PW.ColInt32 (VP.map fromIntegral v)
  AC.ColInt16  v -> Right $ PW.ColInt32 (VP.map fromIntegral v)
  AC.ColInt32  v -> Right $ PW.ColInt32 v
  AC.ColInt64  v -> Right $ PW.ColInt64 v
  AC.ColUInt8  v -> Right $ PW.ColInt32 (VP.map (fromIntegral :: Word8  -> Int32) v)
  AC.ColUInt16 v -> Right $ PW.ColInt32 (VP.map (fromIntegral :: Word16 -> Int32) v)
  AC.ColUInt32 v -> Right $ PW.ColInt32 (VP.map (fromIntegral :: Word32 -> Int32) v)
  AC.ColUInt64 v -> Right $ PW.ColInt64 (VP.map (fromIntegral :: Word64 -> Int64) v)
  AC.ColFloat  v -> Right $ PW.ColFloat v
  AC.ColDouble v -> Right $ PW.ColDouble v
  AC.ColBool   v -> Right $ PW.ColBool v
  AC.ColUtf8   v -> Right $ PW.ColByteArray (V.map TE.encodeUtf8 v)
  AC.ColLargeUtf8 v -> Right $ PW.ColByteArray (V.map TE.encodeUtf8 v)
  AC.ColBinary v -> Right $ PW.ColByteArray v
  AC.ColLargeBinary v -> Right $ PW.ColByteArray v
  -- Temporal types: lower to the natural Parquet physical type
  -- the schema element declared (Int32 for Date32 / Time32, Int64
  -- for Date64 / Time64 / Timestamp / Duration).
  AC.ColDate32 v   -> Right $ PW.ColInt32 v
  AC.ColDate64 v   -> Right $ PW.ColInt64 v
  AC.ColTime32 v   -> Right $ PW.ColInt32 v
  AC.ColTime64 v   -> Right $ PW.ColInt64 v
  AC.ColTimestamp v -> Right $ PW.ColInt64 v
  AC.ColDuration  v -> Right $ PW.ColInt64 v
  -- Nullable: drop nulls, emit only the present values. The
  -- writer's high-level path treats every column as
  -- (Required+ColumnData); proper Optional support requires a
  -- distinct OptionalColumn lowering that the writer's nested
  -- Parquet.Write.encodeOptionalColumnPage path consumes. We
  -- preserve the nullability in the schema (Repetition=Optional)
  -- but drop the nulls here for now; round-trip will recover the
  -- present values with no NULL slots.
  AC.ColInt8Maybe   v -> Right $ PW.ColInt32 (VP.fromList [fromIntegral i | Just i <- V.toList v])
  AC.ColInt16Maybe  v -> Right $ PW.ColInt32 (VP.fromList [fromIntegral i | Just i <- V.toList v])
  AC.ColInt32Maybe  v -> Right $ PW.ColInt32 (VP.fromList [i | Just i <- V.toList v])
  AC.ColInt64Maybe  v -> Right $ PW.ColInt64 (VP.fromList [i | Just i <- V.toList v])
  AC.ColUInt8Maybe  v -> Right $ PW.ColInt32 (VP.fromList [fromIntegral i | Just i <- V.toList v])
  AC.ColUInt16Maybe v -> Right $ PW.ColInt32 (VP.fromList [fromIntegral i | Just i <- V.toList v])
  AC.ColUInt32Maybe v -> Right $ PW.ColInt32 (VP.fromList [fromIntegral i | Just i <- V.toList v])
  AC.ColUInt64Maybe v -> Right $ PW.ColInt64 (VP.fromList [fromIntegral i | Just i <- V.toList v])
  AC.ColFloatMaybe  v -> Right $ PW.ColFloat (VP.fromList [f | Just f <- V.toList v])
  AC.ColDoubleMaybe v -> Right $ PW.ColDouble (VP.fromList [d | Just d <- V.toList v])
  AC.ColBoolMaybe   v -> Right $ PW.ColBool (V.fromList [b | Just b <- V.toList v])
  AC.ColUtf8Maybe   v -> Right $ PW.ColByteArray (V.fromList [TE.encodeUtf8 t | Just t <- V.toList v])
  AC.ColLargeUtf8Maybe v -> Right $ PW.ColByteArray (V.fromList [TE.encodeUtf8 t | Just t <- V.toList v])
  AC.ColBinaryMaybe v -> Right $ PW.ColByteArray (V.fromList [b | Just b <- V.toList v])
  AC.ColLargeBinaryMaybe v -> Right $ PW.ColByteArray (V.fromList [b | Just b <- V.toList v])
  other -> Left $ "Parquet.Arrow: Arrow column shape "
                  <> show other
                  <> " has no flat Parquet equivalent (nested types "
                  <> "go through Parquet.Nested, dictionary columns "
                  <> "should pre-resolve to their values column)"

-- ============================================================
-- Parquet → Arrow
-- ============================================================

-- | Read all leaf columns of a Parquet row group and lift them to
-- Arrow column shapes. Requires a parallel Arrow schema so we know
-- the target nullability + UTF-8 vs raw-binary semantics — the
-- Parquet schema alone is ambiguous for byte-array columns.
parquetRowGroupToArrow
  :: AT.Schema    -- ^ Target Arrow schema (per-leaf nullability matters)
  -> PR.ParquetFile
  -> Int          -- ^ Row group index
  -> Either String (V.Vector AC.ColumnArray)
parquetRowGroupToArrow sch pf rgIdx = do
  let !fields = AT.arrowFields sch
  V.imapM (\colIdx fld -> readParquetColumn pf rgIdx colIdx fld) fields

-- | Read one column chunk and project it into a 'ColumnArray'.
-- Dispatches on the Arrow target type + nullability; falls back
-- to a clean 'Left' for shapes the bridge doesn't yet cover
-- (nullable strings need definition-level decoding which is the
-- caller's job today via 'Parquet.Read.readPlain*OptionalColumnChunk').
readParquetColumn
  :: PR.ParquetFile
  -> Int   -- ^ row-group index
  -> Int   -- ^ column index within the row group
  -> AT.Field
  -> Either String AC.ColumnArray
readParquetColumn pf rgIdx colIdx fld = do
  chunk <- PR.columnChunkSlice pf rgIdx colIdx
  let !codec = chunkCodec pf rgIdx colIdx
      !nullable = AT.fieldNullable fld
  case AT.fieldType fld of
    -- Non-nullable primitives.
    AT.AInt 32 True | not nullable ->
      AC.ColInt32   <$> PR.readPlainInt32ColumnChunk codec chunk
    AT.AInt 64 True | not nullable ->
      AC.ColInt64   <$> PR.readPlainInt64ColumnChunk codec chunk
    AT.AFloatingPoint AT.Single | not nullable ->
      AC.ColFloat   <$> PR.readPlainFloatColumnChunk codec chunk
    AT.AFloatingPoint AT.DoublePrecision | not nullable ->
      AC.ColDouble  <$> PR.readPlainDoubleColumnChunk codec chunk
    AT.ABool | not nullable ->
      AC.ColBool    <$> PR.readPlainBoolColumnChunk codec chunk
    AT.AUtf8 | not nullable -> do
      bs <- PR.readPlainByteArrayColumnChunk codec chunk
      Right $ AC.ColUtf8 (V.map decodeUtf8Lossy bs)
    AT.ABinary | not nullable ->
      AC.ColBinary  <$> PR.readPlainByteArrayColumnChunk codec chunk
    -- Temporal non-nullable: read the underlying int stream and
    -- cast to the Arrow column flavour.
    AT.ADate AT.DateDay | not nullable ->
      AC.ColDate32 <$> PR.readPlainInt32ColumnChunk codec chunk
    AT.ADate AT.DateMillisecond | not nullable ->
      AC.ColDate64 <$> PR.readPlainInt64ColumnChunk codec chunk
    AT.ATime _ 32 | not nullable ->
      AC.ColTime32 <$> PR.readPlainInt32ColumnChunk codec chunk
    AT.ATime _ 64 | not nullable ->
      AC.ColTime64 <$> PR.readPlainInt64ColumnChunk codec chunk
    AT.ATimestamp _ _ | not nullable ->
      AC.ColTimestamp <$> PR.readPlainInt64ColumnChunk codec chunk
    AT.ADuration _ | not nullable ->
      AC.ColDuration <$> PR.readPlainInt64ColumnChunk codec chunk
    other ->
      Left $ "Parquet.Arrow: column type "
             <> show other
             <> " (nullable=" <> show nullable
             <> ") not yet supported by the read bridge; use the "
             <> "specialised readers in Parquet.Read"

-- | Look up the column's 'Compression' codec from the footer.
chunkCodec :: PR.ParquetFile -> Int -> Int -> P.Compression
chunkCodec pf rgIdx colIdx =
  let fm   = PR.pfFooter pf
      rgs  = P.fmRowGroups fm
      rg   = V.unsafeIndex rgs rgIdx
      cols = P.rgColumns rg
      cc   = V.unsafeIndex cols colIdx
  in case P.ccMetadata cc of
       Just md -> P.cmCodec md
       Nothing -> P.Uncompressed

-- | UTF-8 decode that swallows invalid sequences (replacing them
-- with U+FFFD). Real Arrow strings should always be valid UTF-8;
-- we don't want a single bad byte to fail an entire batch read.
decodeUtf8Lossy :: ByteString -> T.Text
decodeUtf8Lossy bs = case TE.decodeUtf8' bs of
  Right t -> t
  Left  _ -> TE.decodeUtf8With TE.lenientDecode bs

-- ============================================================
-- Streaming reader
-- ============================================================

-- | Number of row groups in the file. Useful as a loop bound for
-- 'parquetRowGroupToArrow' / 'streamRowGroups'.
numRowGroups :: PR.ParquetFile -> Int
numRowGroups pf =
  V.length (P.fmRowGroups (PR.pfFooter pf))

-- | Lazily project every row group of a Parquet file into Arrow
-- columns, mirroring pyarrow's @ParquetFile.iter_batches()@. The
-- resulting list defers the per-row-group decode until the
-- consumer pulls the corresponding @Either@; failed row groups
-- surface their parse error in the @Left@ slot without aborting
-- the rest of the stream.
--
-- @
-- pf <- 'Parquet.HighLevel.decodeParquet' bytes
-- forM_ ('streamRowGroups' arrowSchema pf) $ \\rg -> case rg of
--   Right cols -> consume cols
--   Left  err  -> log err
-- @
streamRowGroups
  :: AT.Schema
  -> PR.ParquetFile
  -> [Either String (V.Vector AC.ColumnArray)]
streamRowGroups sch pf =
  [ parquetRowGroupToArrow sch pf i
  | i <- [0 .. numRowGroups pf - 1]
  ]
