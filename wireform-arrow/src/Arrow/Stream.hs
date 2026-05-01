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
  ( -- * Streams (eager)
    encodeArrowStream
  , encodeArrowStreamWith
  , decodeArrowStream
    -- * Files (eager)
  , encodeArrowFile
  , encodeArrowFileWith
  , decodeArrowFile
    -- * Streams (incremental / iterator)
  , StreamReader
  , openStreamReader
  , streamReaderSchema
  , streamReaderNext
  , streamReaderToList
    -- * Write options
  , WriteOptions (..)
  , defaultWriteOptions
  , BodyCompressionCodec (..)
  ) where

import Data.ByteString (ByteString)
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
  , buildRecordBatchBytesWith
  , decompressBody
  , denormaliseBuffers
  , readArrowFileFBWithDicts
  , readArrowStreamFBWithDicts
  , writeArrowFileFBWithDicts
  , writeArrowStreamFBWithDicts
  )
import Arrow.Types
  ( ArrowType (..)
  , BodyCompressionCodec (..)
  , DictionaryEncoding (..)
  , Endianness (..)
  , Field (..)
  , Precision (..)
  , RecordBatchDef (..)
  , Schema (..)
  )

-- ============================================================
-- Write options
-- ============================================================

-- | Arrow IPC writer configuration. Construct one with
-- 'defaultWriteOptions' and override the fields you care about.
data WriteOptions = WriteOptions
  { writeBodyCompression :: !(Maybe BodyCompressionCodec)
    -- ^ When 'Just', body buffers are compressed per Arrow's
    -- 'BodyCompression' table (typically 'BodyZstd'). 'Nothing'
    -- (the default) leaves buffers uncompressed.
  } deriving (Show, Eq)

-- | Defaults: no body compression. PyArrow leaves IPC streams
-- uncompressed by default; we follow suit.
defaultWriteOptions :: WriteOptions
defaultWriteOptions = WriteOptions
  { writeBodyCompression = Nothing
  }

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
encodeArrowStream = encodeArrowStreamWith defaultWriteOptions

-- | 'encodeArrowStream' with explicit 'WriteOptions'.
encodeArrowStreamWith
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> ByteString
encodeArrowStreamWith opts sch batches =
  let !(dicts, batchPairs) = compileBatchesWith opts sch batches
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
encodeArrowFile = encodeArrowFileWith defaultWriteOptions

-- | 'encodeArrowFile' with explicit 'WriteOptions'.
encodeArrowFileWith
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> ByteString
encodeArrowFileWith opts sch batches =
  let !(dicts, batchPairs) = compileBatchesWith opts sch batches
  in  writeArrowFileFBWithDicts sch dicts batchPairs

-- | Decode an Arrow IPC file. Dictionary-resolved 'ColumnArray'
-- values are returned just like 'decodeArrowStream'.
decodeArrowFile
  :: ByteString
  -> Either String (Schema, [V.Vector ColumnArray])
decodeArrowFile bs = do
  (sch, dicts, frames) <- readArrowFileFBWithDicts bs
  decodeBatches sch dicts frames

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
compileBatches = compileBatchesWith defaultWriteOptions

compileBatchesWith
  :: WriteOptions
  -> Schema
  -> [V.Vector ColumnArray]
  -> ([DictBatch], [(RecordBatchDef, ByteString)])
compileBatchesWith opts sch batches =
  let !dictMap = collectDictionaries batches
      !dicts   = map (uncurry buildDictBatch) (Map.toAscList dictMap)
      !mCodec  = writeBodyCompression opts
      !pairs   = map (buildRecordBatchBytesWith mCodec sch) batches
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
      (rb', body') <- maybeDecompressBatch rb body
      cols <- materializeRecordBatch s (denormaliseBuffers s rb') body'
      Right (V.map (resolveDictionaryColumn (`Map.lookup` m)) cols)

-- | If the record batch advertises body compression, run the
-- per-buffer decompressor and rewrite the buffer offsets to
-- point at the uncompressed layout (suitable for
-- 'denormaliseBuffers' / 'materializeRecordBatch').
maybeDecompressBatch
  :: RecordBatchDef -> ByteString -> Either String (RecordBatchDef, ByteString)
maybeDecompressBatch rb body = case rbBodyCompression rb of
  Nothing    -> Right (rb, body)
  Just codec -> do
    (newBufs, newBody) <- decompressBody codec (rbBuffers rb) body
    Right ( rb { rbBuffers         = newBufs
               , rbBodyCompression = Nothing
               }
          , newBody
          )

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

-- ============================================================
-- Streaming reader
-- ============================================================

-- | An iterator-style handle for reading an Arrow IPC stream
-- batch-by-batch, mirroring pyarrow's @ipc.RecordBatchStreamReader@:
--
-- @
-- case 'openStreamReader' bytes of
--   Left  e  -> handleError e
--   Right rd -> do
--     let !sch = 'streamReaderSchema' rd
--     loop rd
--   where
--     loop rd = case 'streamReaderNext' rd of
--       Right (Just (cols, rd')) -> consume cols >> loop rd'
--       Right Nothing            -> finish ()
--       Left  e                  -> handleError e
-- @
--
-- All dictionary batches present in the stream are consumed and
-- materialised at 'openStreamReader' time, so subsequent
-- 'streamReaderNext' calls only allocate per-batch column data.
-- Use 'streamReaderToList' to drain the iterator into a list
-- (equivalent to 'decodeArrowStream' but keeps the streaming
-- shape for callers that want incremental processing later).
data StreamReader = StreamReader
  { srSchema  :: !Schema
  , srDictMap :: !(Map.Map Int64 ColumnArray)
  , srFrames  :: ![(RecordBatchDef, ByteString)]
  }

-- | Initialise an iterator from raw stream bytes. Parses the
-- schema + every dictionary batch eagerly (since record batches
-- can reference any dict declared earlier in the stream) and
-- leaves the record-batch frames un-materialised until the
-- caller pulls them.
openStreamReader :: ByteString -> Either String StreamReader
openStreamReader bs = do
  (sch, dicts, frames) <- readArrowStreamFBWithDicts bs
  dictPairs <- traverse (decodeDictBatch sch) dicts
  let !dictMap = Map.fromList dictPairs
  Right StreamReader
    { srSchema  = sch
    , srDictMap = dictMap
    , srFrames  = frames
    }

-- | The schema decoded at 'openStreamReader' time.
streamReaderSchema :: StreamReader -> Schema
streamReaderSchema = srSchema

-- | Pull the next record batch from the iterator. Returns:
--
--   * @Right (Just (cols, rd'))@ — a materialised batch with
--     dictionary references resolved, plus a continuation
--     reader for the remaining frames.
--   * @Right Nothing@ — the stream's EOS marker has been
--     consumed; no more batches.
--   * @Left e@ — a parse / materialisation error.
streamReaderNext
  :: StreamReader
  -> Either String (Maybe (V.Vector ColumnArray, StreamReader))
streamReaderNext rd = case srFrames rd of
  [] -> Right Nothing
  ((rb, body) : rest) -> do
    cols <- materializeRecordBatch (srSchema rd)
              (denormaliseBuffers (srSchema rd) rb)
              body
    let !resolved = V.map
          (resolveDictionaryColumn (`Map.lookup` srDictMap rd)) cols
        !rd' = rd { srFrames = rest }
    Right (Just (resolved, rd'))

-- | Drain a 'StreamReader' into a list of batches. Equivalent
-- (modulo the schema bundling) to 'decodeArrowStream' but keeps
-- the streaming shape for callers that prefer to iterate.
streamReaderToList
  :: StreamReader
  -> Either String [V.Vector ColumnArray]
streamReaderToList rd0 = go rd0 []
  where
    go rd acc = case streamReaderNext rd of
      Left e                      -> Left e
      Right Nothing               -> Right (reverse acc)
      Right (Just (cols, rd'))    -> go rd' (cols : acc)
