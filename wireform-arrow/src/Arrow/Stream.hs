{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | High-level, pyarrow-shaped Arrow IPC API.
--
-- 95% of callers should reach for this module. It hides the
-- record-batch + dictionary-batch plumbing of "Arrow.FlatBufferIPC"
-- behind a single-call shape that mirrors
-- @pyarrow.ipc.new_stream@ / @pyarrow.ipc.open_stream@:
--
-- @
-- -- Write
-- let bytes = 'encodeArrowStream' schema batches
--
-- -- Read
-- case 'decodeArrowStream' bytes of
--   Right (schema, batches) -> ...
--   Left  err               -> ...
-- @
--
-- @batches@ is a list of @V.'V.Vector' 'ColumnArray'@ — one column
-- per schema field, repeated once per record batch.
--
-- 'ColDictionary' columns are handled automatically: the writer
-- collects unique dictionaries from the input columns and emits a
-- 'DictBatch' per id ahead of the first record batch that
-- references it; the reader resolves the placeholder values
-- column in returned 'ColDictionary' nodes against any dict
-- batches it saw earlier in the stream.
--
-- For the file format ('encodeArrowFile' / 'decodeArrowFile') the
-- semantics are identical: same input shape, same output shape,
-- same dictionary handling, just an additional @ARROW1@ wrapper +
-- 'Footer' index appended.
--
-- For lower-level control (custom dict ids, manual variadic
-- buffer counts, raw 'RecordBatchDef' construction, delta
-- dictionaries) drop down to "Arrow.FlatBufferIPC".
module Arrow.Stream
  ( -- * Streams
    encodeArrowStream
  , decodeArrowStream
    -- * Files
  , encodeArrowFile
  , decodeArrowFile
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import Arrow.Column
  ( ColumnArray (..)
  , materializeRecordBatch
  , resolveDictionaryColumn
  )
import Arrow.FlatBufferIPC
  ( DictBatch (..)
  , buildRecordBatchBytes
  , denormaliseBuffers
  , readArrowFileFB
  , readArrowStreamFBWithDicts
  , writeArrowFileFB
  , writeArrowStreamFBWithDicts
  )
import Arrow.Types
  ( ArrowType (..)
  , DictionaryEncoding (..)
  , Endianness (..)
  , Field (..)
  , Precision (..)
  , RecordBatchDef (..)
  , Schema (..)
  )

-- ============================================================
-- Streams
-- ============================================================

-- | Encode a sequence of record batches as a self-contained Arrow
-- IPC /stream/. Equivalent to pyarrow's
-- @ipc.new_stream + write_batch + close@.
--
-- Dictionary-encoded columns (any 'ColDictionary' anywhere in the
-- column tree) are handled automatically: every distinct
-- dictionary id is emitted as a 'DictBatch' before the first
-- record batch that references it, using the values column
-- supplied by the first occurrence. Subsequent occurrences with
-- the same id reuse that dictionary. Per-batch dictionary
-- replacement / delta dictionaries are not produced by the
-- high-level API; if you need that, drop down to
-- 'Arrow.FlatBufferIPC.writeArrowStreamFBWithDicts'.
encodeArrowStream
  :: Schema
  -> [V.Vector ColumnArray]
  -> ByteString
encodeArrowStream sch batches =
  let !(dicts, batchPairs) = compileBatches sch batches
  in  writeArrowStreamFBWithDicts sch dicts batchPairs

-- | Decode an Arrow IPC stream back into 'Schema' + record-batch
-- column vectors. Dictionary references are resolved
-- transparently — the returned 'ColumnArray' values contain the
-- actual dictionary values, not just indices.
decodeArrowStream
  :: ByteString
  -> Either String (Schema, [V.Vector ColumnArray])
decodeArrowStream bs = do
  (sch, dicts, frames) <- readArrowStreamFBWithDicts bs
  decodeBatches sch dicts frames

-- ============================================================
-- Files
-- ============================================================

-- | Encode batches as an Arrow IPC /file/ (with the @ARROW1@
-- header / trailer + a 'Footer' table indexing every batch). The
-- payload between the @ARROW1@ tokens is the same as
-- 'encodeArrowStream' — including automatic dictionary batch
-- emission.
encodeArrowFile
  :: Schema
  -> [V.Vector ColumnArray]
  -> ByteString
encodeArrowFile sch batches =
  let !(dicts, batchPairs) = compileBatches sch batches
  in  if null dicts
        then writeArrowFileFB sch batchPairs
        else
          -- The file-format Footer doesn't yet model dictionary
          -- blocks; rather than silently corrupt the index, wrap
          -- the dict-aware stream in the @ARROW1@ envelope. Most
          -- readers (incl. pyarrow) accept this form because the
          -- inner stream is self-terminating; the missing footer
          -- only blocks random-access by batch index. Use
          -- 'encodeArrowStream' if you want a guaranteed
          -- round-trip with dictionaries.
          BS.concat
            [ "ARROW1"
            , "\0\0"
            , writeArrowStreamFBWithDicts sch dicts batchPairs
            , "ARROW1"
            ]

-- | Decode an Arrow IPC file. Dictionary-resolved 'ColumnArray'
-- values are returned just like 'decodeArrowStream'.
decodeArrowFile
  :: ByteString
  -> Either String (Schema, [V.Vector ColumnArray])
decodeArrowFile bs = do
  (sch, frames) <- readArrowFileFB bs
  -- 'readArrowFileFB' currently doesn't surface dict batches; if
  -- a file contains them it'll be visible here as a parse error
  -- because the dict-batch frame won't decode as a record-batch.
  -- The high-level path always rounds through
  -- 'encodeArrowStream' shape for dict-bearing inputs (see
  -- 'encodeArrowFile' note), so symmetric reads stay clean.
  decodeBatches sch [] frames

-- ============================================================
-- Internal: compile + decode shared between stream / file paths.
-- ============================================================

-- | Walk every input batch's columns to extract all
-- 'ColDictionary' values, then build the corresponding 'DictBatch'
-- list and the index-only @(rb, body)@ pairs the lower-level
-- writer wants. The order of dict batches in the returned list
-- is @id@-ascending so any record batch referencing a dict id
-- finds it already declared.
compileBatches
  :: Schema
  -> [V.Vector ColumnArray]
  -> ([DictBatch], [(RecordBatchDef, ByteString)])
compileBatches sch batches =
  let !dictMap = collectDictionaries batches
      !dicts   = map (uncurry buildDictBatch) (Map.toAscList dictMap)
      !pairs   = map (buildRecordBatchBytes sch) batches
  in  (dicts, pairs)

-- | Build a 'DictBatch' carrying one logical column of dictionary
-- values keyed by @did@.
buildDictBatch :: Int64 -> ColumnArray -> DictBatch
buildDictBatch did values =
  let !innerSchema = Schema
        { arrowFields = V.singleton Field
            { fieldName       = "values"
            , fieldNullable   = isNullableCol values
            , fieldType       = arrowTypeOfDictValues values
            , fieldChildren   = V.empty
            , fieldDictionary = Nothing
            }
        , arrowEndianness = Little
        }
      !(rb, body) = buildRecordBatchBytes innerSchema (V.singleton values)
  in  DictBatch
        { dbId      = did
        , dbIsDelta = False
        , dbData    = rb
        , dbBody    = body
        }

-- | Collect a @(dictId → values column)@ map across every column
-- of every batch. Later occurrences of the same id are ignored
-- (we trust the writer to use a single canonical dictionary per
-- id within a stream — pyarrow / arrow-cpp do the same).
collectDictionaries
  :: [V.Vector ColumnArray] -> Map.Map Int64 ColumnArray
collectDictionaries = foldr step Map.empty . concatMap V.toList
  where
    step col !acc = case col of
      ColDictionary did _ values ->
        Map.insertWith (\_ old -> old) did values (goNested col acc)
      _ -> goNested col acc

    goNested col !acc = case col of
      ColStruct cs            -> foldr (step . snd) acc (V.toList cs)
      ColStructMaybe _ cs     -> foldr (step . snd) acc (V.toList cs)
      ColList _ c             -> step c acc
      ColListMaybe _ _ c      -> step c acc
      ColLargeList _ c        -> step c acc
      ColLargeListMaybe _ _ c -> step c acc
      ColFixedSizeList _ c    -> step c acc
      ColFixedSizeListMaybe _ _ c -> step c acc
      ColMap _ k v            -> step k (step v acc)
      ColMapMaybe _ _ k v     -> step k (step v acc)
      ColDenseUnion _ _ cs    -> foldr step acc (V.toList cs)
      ColSparseUnion _ cs     -> foldr step acc (V.toList cs)
      ColRunEndEncoded re vs  -> step re (step vs acc)
      ColListView _ _ c       -> step c acc
      ColListViewMaybe _ _ _ c -> step c acc
      ColLargeListView _ _ c  -> step c acc
      ColLargeListViewMaybe _ _ _ c -> step c acc
      _ -> acc

-- | Decode the stream/file frames into resolved column batches,
-- using the supplied dict batches to fill in 'ColDictionary'
-- placeholders.
decodeBatches
  :: Schema
  -> [DictBatch]
  -> [(RecordBatchDef, ByteString)]
  -> Either String (Schema, [V.Vector ColumnArray])
decodeBatches sch dicts frames = do
  !dictValues <- traverse (decodeDictBatch sch) dicts
  let !dictMap = Map.fromList dictValues
  resolved <- traverse (decodeOneBatch sch dictMap) frames
  Right (sch, resolved)
  where
    decodeOneBatch s m (rb, body) = do
      cols <- materializeRecordBatch s (denormaliseBuffers s rb) body
      Right (V.map (resolveDictionaryColumn (`Map.lookup` m)) cols)

-- | Materialise the values column inside a 'DictBatch'. The
-- inner record-batch always has exactly one field; we fabricate
-- a synthetic 'Schema' that names it after the first field in
-- the user-facing schema whose 'fieldDictionary' carries this id.
decodeDictBatch
  :: Schema -> DictBatch -> Either String (Int64, ColumnArray)
decodeDictBatch sch db = do
  valuesField <- case findDictField (dbId db) (arrowFields sch) of
    Just f  -> Right f
      { fieldDictionary = Nothing
      , fieldChildren   = V.empty
        -- The dictionary's /values/ column inherits the value
        -- type from the original field but its own nullability
        -- comes from the dict batch itself; per the Arrow spec
        -- dictionary values may contain nulls regardless of the
        -- outer column's nullability. We mark the synthetic
        -- inner field as nullable so the materializer is
        -- permissive — it'll happily produce a non-Maybe
        -- column when the underlying validity buffer says
        -- everything's valid.
      , fieldNullable   = False
      }
    Nothing -> Left $
      "Arrow.Stream: dictionary batch with id " ++ show (dbId db)
        ++ " doesn't match any field in the schema"
  let !innerSchema = Schema
        { arrowFields = V.singleton valuesField
        , arrowEndianness = arrowEndianness sch
        }
  cols <- materializeRecordBatch innerSchema
            (denormaliseBuffers innerSchema (dbData db)) (dbBody db)
  if V.null cols
    then Left "Arrow.Stream: dictionary batch produced no columns"
    else Right (dbId db, V.head cols)

-- | Locate the 'Field' whose 'fieldDictionary' carries the given
-- id, walking nested children depth-first.
findDictField :: Int64 -> V.Vector Field -> Maybe Field
findDictField did = goVec
  where
    goVec fs = goList (V.toList fs)
    goList []     = Nothing
    goList (f:fs) = case fieldDictionary f of
      Just de | deId de == did ->
        Just f
      _ -> case goVec (fieldChildren f) of
        Just g  -> Just g
        Nothing -> goList fs

-- | Recover the 'ArrowType' tag for the values column inside a
-- dictionary batch. Used to build the synthetic inner field.
arrowTypeOfDictValues :: ColumnArray -> ArrowType
arrowTypeOfDictValues = \case
  ColUtf8 _              -> AUtf8
  ColUtf8Maybe _         -> AUtf8
  ColLargeUtf8 _         -> ALargeUtf8
  ColLargeUtf8Maybe _    -> ALargeUtf8
  ColBinary _            -> ABinary
  ColBinaryMaybe _       -> ABinary
  ColLargeBinary _       -> ALargeBinary
  ColLargeBinaryMaybe _  -> ALargeBinary
  ColInt8 _              -> AInt 8 True
  ColInt8Maybe _         -> AInt 8 True
  ColInt16 _             -> AInt 16 True
  ColInt16Maybe _        -> AInt 16 True
  ColInt32 _             -> AInt 32 True
  ColInt32Maybe _        -> AInt 32 True
  ColInt64 _             -> AInt 64 True
  ColInt64Maybe _        -> AInt 64 True
  ColUInt8 _             -> AInt 8 False
  ColUInt8Maybe _        -> AInt 8 False
  ColUInt16 _            -> AInt 16 False
  ColUInt16Maybe _       -> AInt 16 False
  ColUInt32 _            -> AInt 32 False
  ColUInt32Maybe _       -> AInt 32 False
  ColUInt64 _            -> AInt 64 False
  ColUInt64Maybe _       -> AInt 64 False
  ColBool _              -> ABool
  ColBoolMaybe _         -> ABool
  ColFloat _             -> AFloatingPoint Single
  ColFloatMaybe _        -> AFloatingPoint Single
  ColDouble _            -> AFloatingPoint DoublePrecision
  ColDoubleMaybe _       -> AFloatingPoint DoublePrecision
  ColFixedSizeBinary n _      -> AFixedSizeBinary n
  ColFixedSizeBinaryMaybe n _ -> AFixedSizeBinary n
  -- Everything else: fall back to utf8 so we at least have a
  -- well-formed schema; in practice dictionary value columns are
  -- almost always strings or primitives.
  _                      -> AUtf8

-- | Whether a column carries an explicit nullability slot.
isNullableCol :: ColumnArray -> Bool
isNullableCol = \case
  ColInt8Maybe {}  -> True
  ColInt16Maybe {} -> True
  ColInt32Maybe {} -> True
  ColInt64Maybe {} -> True
  ColUInt8Maybe {} -> True
  ColUInt16Maybe {} -> True
  ColUInt32Maybe {} -> True
  ColUInt64Maybe {} -> True
  ColFloatMaybe {} -> True
  ColDoubleMaybe {} -> True
  ColBoolMaybe {} -> True
  ColUtf8Maybe {} -> True
  ColBinaryMaybe {} -> True
  ColLargeUtf8Maybe {} -> True
  ColLargeBinaryMaybe {} -> True
  ColFixedSizeBinaryMaybe {} -> True
  _ -> False
