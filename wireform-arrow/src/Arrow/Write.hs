{-# LANGUAGE BangPatterns #-}
-- | Apache Arrow IPC column encoders and stream\/file writers.
module Arrow.Write
  ( encodePlainInt32Column
  , encodePlainInt64Column
  , encodePlainFloat
  , encodePlainDouble
  , encodePlainBool
  , encodePlainUtf8
  , encodeNullBitmap
  , buildRecordBatch
  , writeArrowStream
  , writeArrowFile
  ) where

import Data.Bits ((.&.), (.|.), shiftL, complement)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP

import Arrow.Types
import Arrow.IPC (encodeIPCMessage)
import Arrow.Column (ColumnArray (..), columnLength)

-- * Plain column encoders

encodePlainInt32Column :: VP.Vector Int32 -> ByteString
encodePlainInt32Column vec = BL.toStrict $ B.toLazyByteString $
  VP.foldl' (\acc v -> acc <> B.int32LE v) mempty vec

encodePlainInt64Column :: VP.Vector Int64 -> ByteString
encodePlainInt64Column vec = BL.toStrict $ B.toLazyByteString $
  VP.foldl' (\acc v -> acc <> B.int64LE v) mempty vec

encodePlainFloat :: VP.Vector Float -> ByteString
encodePlainFloat vec = BL.toStrict $ B.toLazyByteString $
  VP.foldl' (\acc v -> acc <> B.floatLE v) mempty vec

encodePlainDouble :: VP.Vector Double -> ByteString
encodePlainDouble vec = BL.toStrict $ B.toLazyByteString $
  VP.foldl' (\acc v -> acc <> B.doubleLE v) mempty vec

encodePlainBool :: V.Vector Bool -> ByteString
encodePlainBool vec =
  let !n = V.length vec
      !nBytes = (n + 7) `quot` 8
      packByte !byteIdx =
        let !base = byteIdx * 8
            goBit !acc !bit
              | bit >= 8 = acc
              | base + bit >= n = acc
              | V.unsafeIndex vec (base + bit) = goBit (acc .|. (1 `shiftL` bit)) (bit + 1)
              | otherwise = goBit acc (bit + 1)
        in goBit (0 :: Word8) 0
      go !i
        | i >= nBytes = mempty
        | otherwise = B.word8 (packByte i) <> go (i + 1)
  in BL.toStrict (B.toLazyByteString (go 0))

encodePlainUtf8 :: V.Vector Text -> (ByteString, ByteString)
encodePlainUtf8 vec =
  let !n = V.length vec
      go !i !off !offB !datB
        | i >= n =
            ( BL.toStrict (B.toLazyByteString (offB <> B.int32LE off))
            , BL.toStrict (B.toLazyByteString datB)
            )
        | otherwise =
            let !bs = TE.encodeUtf8 (V.unsafeIndex vec i)
                !len = fromIntegral (BS.length bs) :: Int32
            in go (i + 1) (off + len) (offB <> B.int32LE off) (datB <> B.byteString bs)
  in go 0 0 mempty mempty

encodeNullBitmap :: V.Vector Bool -> ByteString
encodeNullBitmap = encodePlainBool

-- * Internal column encoders

encodePlainBinary :: V.Vector ByteString -> (ByteString, ByteString)
encodePlainBinary vec =
  let !n = V.length vec
      go !i !off !offB !datB
        | i >= n =
            ( BL.toStrict (B.toLazyByteString (offB <> B.int32LE off))
            , BL.toStrict (B.toLazyByteString datB)
            )
        | otherwise =
            let !bs = V.unsafeIndex vec i
                !len = fromIntegral (BS.length bs) :: Int32
            in go (i + 1) (off + len) (offB <> B.int32LE off) (datB <> B.byteString bs)
  in go 0 0 mempty mempty

encodeInt8s :: VP.Vector Int8 -> ByteString
encodeInt8s vec = BL.toStrict $ B.toLazyByteString $
  VP.foldl' (\acc v -> acc <> B.int8 v) mempty vec

encodeInt16s :: VP.Vector Int16 -> ByteString
encodeInt16s vec = BL.toStrict $ B.toLazyByteString $
  VP.foldl' (\acc v -> acc <> B.int16LE v) mempty vec

-- Unsigned integer encoders.
encodeUInt8s :: VP.Vector Word8 -> ByteString
encodeUInt8s vec = BL.toStrict $ B.toLazyByteString $
  VP.foldl' (\acc v -> acc <> B.word8 v) mempty vec

encodeUInt16s :: VP.Vector Word16 -> ByteString
encodeUInt16s vec = BL.toStrict $ B.toLazyByteString $
  VP.foldl' (\acc v -> acc <> B.word16LE v) mempty vec

encodeUInt32s :: VP.Vector Word32 -> ByteString
encodeUInt32s vec = BL.toStrict $ B.toLazyByteString $
  VP.foldl' (\acc v -> acc <> B.word32LE v) mempty vec

encodeUInt64s :: VP.Vector Word64 -> ByteString
encodeUInt64s vec = BL.toStrict $ B.toLazyByteString $
  VP.foldl' (\acc v -> acc <> B.word64LE v) mempty vec

-- Half-precision floats are just raw 16-bit words.
encodeFloat16s :: VP.Vector Word16 -> ByteString
encodeFloat16s = encodeUInt16s

-- Int64 encoder for LargeList / LargeBinary / LargeUtf8 offsets.
encodePlainInt64Offsets :: VP.Vector Int64 -> ByteString
encodePlainInt64Offsets vec = BL.toStrict $ B.toLazyByteString $
  VP.foldl' (\acc v -> acc <> B.int64LE v) mempty vec

-- Int32 array encoder that accepts the same shape as 'encodeInt16s'.
-- (Left as an alias for discoverability from the encodeCol site.)
encodeInt32s :: VP.Vector Int32 -> ByteString
encodeInt32s = encodePlainInt32Column

encodeInt64s :: VP.Vector Int64 -> ByteString
encodeInt64s = encodePlainInt64Column

-- Large variable-length (Int64 offsets) encoders.
encodePlainLargeUtf8 :: V.Vector Text -> (ByteString, ByteString)
encodePlainLargeUtf8 vec =
  let !n = V.length vec
      go !i !off !offB !datB
        | i >= n =
            ( BL.toStrict (B.toLazyByteString (offB <> B.int64LE off))
            , BL.toStrict (B.toLazyByteString datB)
            )
        | otherwise =
            let !bs = TE.encodeUtf8 (V.unsafeIndex vec i)
                !len = fromIntegral (BS.length bs) :: Int64
            in go (i + 1) (off + len) (offB <> B.int64LE off) (datB <> B.byteString bs)
  in go 0 0 mempty mempty

encodePlainLargeBinary :: V.Vector ByteString -> (ByteString, ByteString)
encodePlainLargeBinary vec =
  let !n = V.length vec
      go !i !off !offB !datB
        | i >= n =
            ( BL.toStrict (B.toLazyByteString (offB <> B.int64LE off))
            , BL.toStrict (B.toLazyByteString datB)
            )
        | otherwise =
            let !bs = V.unsafeIndex vec i
                !len = fromIntegral (BS.length bs) :: Int64
            in go (i + 1) (off + len) (offB <> B.int64LE off) (datB <> B.byteString bs)
  in go 0 0 mempty mempty

-- Fixed-size binary: just concatenate the fixed-width payloads. The
-- caller is expected to have enforced the width (we do a best-effort
-- pad / truncate here so ragged inputs don't corrupt the downstream
-- offsets).
encodePlainFixedSizeBinary :: Int -> V.Vector ByteString -> ByteString
encodePlainFixedSizeBinary !w vec = BL.toStrict $ B.toLazyByteString $
  V.foldl' (\acc bs ->
              let !raw = BS.length bs
              in if raw == w
                   then acc <> B.byteString bs
                   else if raw > w
                          then acc <> B.byteString (BS.take w bs)
                          else acc <> B.byteString bs
                                   <> B.byteString (BS.replicate (w - raw) 0)
           ) mempty vec

-- Interval encoders (YearMonth / DayTime / MonthDayNano).
encodeIntervalYearMonth :: VP.Vector Int32 -> ByteString
encodeIntervalYearMonth = encodePlainInt32Column

encodeIntervalDayTime :: VP.Vector Int32 -> VP.Vector Int32 -> ByteString
encodeIntervalDayTime days millis = BL.toStrict $ B.toLazyByteString $
  let !n = min (VP.length days) (VP.length millis)
      go !i
        | i >= n = mempty
        | otherwise =
            B.int32LE (VP.unsafeIndex days i)
            <> B.int32LE (VP.unsafeIndex millis i)
            <> go (i + 1)
  in go 0

encodeIntervalMonthDayNano
  :: VP.Vector Int32 -> VP.Vector Int32 -> VP.Vector Int64 -> ByteString
encodeIntervalMonthDayNano months days nanos = BL.toStrict $ B.toLazyByteString $
  let !n = min (VP.length months) (min (VP.length days) (VP.length nanos))
      go !i
        | i >= n = mempty
        | otherwise =
            B.int32LE (VP.unsafeIndex months i)
            <> B.int32LE (VP.unsafeIndex days i)
            <> B.int64LE (VP.unsafeIndex nanos i)
            <> go (i + 1)
  in go 0

alignUp8 :: Int -> Int
alignUp8 n = (n + 7) .&. complement 7

-- * Record batch builder accumulator

data BuildAcc = BuildAcc
  { baOffset :: !Int64
  , baNodes  :: ![FieldNode]
  , baBufs   :: ![Buffer]
  , baBody   :: !B.Builder
  }

emptyBuildAcc :: BuildAcc
emptyBuildAcc = BuildAcc 0 [] [] mempty

addBufData :: ByteString -> BuildAcc -> BuildAcc
addBufData bs (BuildAcc off ns bufs body) =
  let !rawLen = BS.length bs
      !padded = alignUp8 rawLen
      !pad = padded - rawLen
  in BuildAcc
      (off + fromIntegral padded)
      ns
      (Buffer off (fromIntegral rawLen) : bufs)
      (body <> B.byteString bs <> B.byteString (BS.replicate pad 0))

addFieldNode :: Int64 -> Int64 -> BuildAcc -> BuildAcc
addFieldNode len nc (BuildAcc off ns bufs body) =
  BuildAcc off (FieldNode len nc : ns) bufs body

countNulls :: V.Vector (Maybe a) -> Int
countNulls = V.foldl' (\c x -> case x of Nothing -> c + 1; Just _ -> c) 0

-- * Top-level record batch encoder

-- | Encode a record batch as a complete IPC message (continuation + metadata + body).
buildRecordBatch :: Schema -> V.Vector ColumnArray -> ByteString
buildRecordBatch schema cols =
  let !acc = encodeColumns (arrowFields schema) cols emptyBuildAcc
      !nodes = V.fromList (reverse (baNodes acc))
      !bufs = V.fromList (reverse (baBufs acc))
      !numRows = if V.null cols then 0 else columnLength (V.head cols)
      !rb = RecordBatchDef
        { rbLength = fromIntegral numRows
        , rbNodes = nodes
        , rbBuffers = bufs
        }
      !bodyLen = baOffset acc
      !metaBs = encodeRecordBatchMeta rb bodyLen
      !metaLen = BS.length metaBs
      !paddedMetaLen = alignUp8 metaLen
      !metaPad = paddedMetaLen - metaLen
      !bodyBs = BL.toStrict (B.toLazyByteString (baBody acc))
  in BL.toStrict $ B.toLazyByteString $
      B.word32LE 0xFFFFFFFF
      <> B.int32LE (fromIntegral paddedMetaLen)
      <> B.byteString metaBs
      <> B.byteString (BS.replicate metaPad 0)
      <> B.byteString bodyBs

-- Metadata in the same simplified format as Arrow.IPC
encodeRecordBatchMeta :: RecordBatchDef -> Int64 -> ByteString
encodeRecordBatchMeta rb bodyLen = BL.toStrict $ B.toLazyByteString $
  B.int16LE 4
  <> B.word8 3
  <> B.int32LE (fromIntegral (BS.length headerBs))
  <> B.byteString headerBs
  <> B.int64LE bodyLen
  where
    headerBs = BL.toStrict $ B.toLazyByteString $
      B.int64LE (rbLength rb)
      <> B.int32LE (fromIntegral (V.length (rbNodes rb)))
      <> V.foldl' (\acc n -> acc <> B.int64LE (fnLength n) <> B.int64LE (fnNullCount n)) mempty (rbNodes rb)
      <> B.int32LE (fromIntegral (V.length (rbBuffers rb)))
      <> V.foldl' (\acc b -> acc <> B.int64LE (bufOffset b) <> B.int64LE (bufLength b)) mempty (rbBuffers rb)

-- * Column encoding (DFS preorder, matching Arrow spec)

encodeColumns :: V.Vector Field -> V.Vector ColumnArray -> BuildAcc -> BuildAcc
encodeColumns fields cols acc =
  V.ifoldl' (\a i f -> encodeCol f (V.unsafeIndex cols i) a) acc fields

-- | Encode one column (depth-first preorder per the Arrow IPC spec).
-- Every 'ColumnArray' constructor has a handler; non-null primitive
-- columns emit a single data buffer, nullable primitives prepend a
-- validity bitmap, variable-length columns emit (offsets, data),
-- large variants use 64-bit offsets, and nested columns recurse into
-- their children.
encodeCol :: Field -> ColumnArray -> BuildAcc -> BuildAcc
encodeCol f col acc = case col of
  -- ============================================================
  -- Non-null primitives
  -- ============================================================
  ColInt8  v -> primFlat (encodeInt8s  v) (VP.length v) acc
  ColInt16 v -> primFlat (encodeInt16s v) (VP.length v) acc
  ColInt32 v -> primFlat (encodeInt32s v) (VP.length v) acc
  ColInt64 v -> primFlat (encodeInt64s v) (VP.length v) acc
  ColUInt8  v -> primFlat (encodeUInt8s  v) (VP.length v) acc
  ColUInt16 v -> primFlat (encodeUInt16s v) (VP.length v) acc
  ColUInt32 v -> primFlat (encodeUInt32s v) (VP.length v) acc
  ColUInt64 v -> primFlat (encodeUInt64s v) (VP.length v) acc
  ColFloat16 v -> primFlat (encodeFloat16s v) (VP.length v) acc
  ColFloat   v -> primFlat (encodePlainFloat  v) (VP.length v) acc
  ColDouble  v -> primFlat (encodePlainDouble v) (VP.length v) acc
  ColBool    v -> primFlat (encodePlainBool   v) (V.length  v) acc

  -- Date / time / timestamp / duration are all fixed-width integer
  -- payloads under the hood.
  ColDate32    v -> primFlat (encodeInt32s v) (VP.length v) acc
  ColDate64    v -> primFlat (encodeInt64s v) (VP.length v) acc
  ColTime32    v -> primFlat (encodeInt32s v) (VP.length v) acc
  ColTime64    v -> primFlat (encodeInt64s v) (VP.length v) acc
  ColTimestamp v -> primFlat (encodeInt64s v) (VP.length v) acc
  ColDuration  v -> primFlat (encodeInt64s v) (VP.length v) acc

  -- Interval / decimal / fixed-size binary: fixed-width payloads
  -- with unit-specific strides.
  ColIntervalYearMonth v ->
    primFlat (encodeIntervalYearMonth v) (VP.length v) acc
  ColIntervalDayTime days millis ->
    primFlat (encodeIntervalDayTime days millis) (VP.length days) acc
  ColIntervalMonthDayNano months days nanos ->
    primFlat (encodeIntervalMonthDayNano months days nanos) (VP.length months) acc
  ColDecimal128 _ _ v ->
    primFlat (encodePlainFixedSizeBinary 16 v) (V.length v) acc
  ColDecimal256 _ _ v ->
    primFlat (encodePlainFixedSizeBinary 32 v) (V.length v) acc
  ColFixedSizeBinary w v ->
    primFlat (encodePlainFixedSizeBinary w v) (V.length v) acc

  -- ============================================================
  -- Non-null variable-length columns
  -- ============================================================
  ColUtf8 v ->
    let (offBs, datBs) = encodePlainUtf8 v
    in varFlat offBs datBs (V.length v) acc
  ColBinary v ->
    let (offBs, datBs) = encodePlainBinary v
    in varFlat offBs datBs (V.length v) acc
  ColLargeUtf8 v ->
    let (offBs, datBs) = encodePlainLargeUtf8 v
    in varFlat offBs datBs (V.length v) acc
  ColLargeBinary v ->
    let (offBs, datBs) = encodePlainLargeBinary v
    in varFlat offBs datBs (V.length v) acc

  -- ============================================================
  -- Nullable primitives (one validity bitmap + one data buffer)
  -- ============================================================
  ColInt8Maybe   v -> primNullable encodeInt8s  (0 :: Int8)  v acc
  ColInt16Maybe  v -> primNullable encodeInt16s (0 :: Int16) v acc
  ColInt32Maybe  v -> primNullable encodeInt32s (0 :: Int32) v acc
  ColInt64Maybe  v -> primNullable encodeInt64s (0 :: Int64) v acc
  ColUInt8Maybe  v -> primNullable encodeUInt8s  (0 :: Word8)  v acc
  ColUInt16Maybe v -> primNullable encodeUInt16s (0 :: Word16) v acc
  ColUInt32Maybe v -> primNullable encodeUInt32s (0 :: Word32) v acc
  ColUInt64Maybe v -> primNullable encodeUInt64s (0 :: Word64) v acc
  ColFloat16Maybe v -> primNullable encodeFloat16s (0 :: Word16) v acc
  ColFloatMaybe   v -> primNullable encodePlainFloat  (0 :: Float)  v acc
  ColDoubleMaybe  v -> primNullable encodePlainDouble (0 :: Double) v acc
  ColBoolMaybe v -> primNullableBoxed encodePlainBool False v acc
  ColDate32Maybe    v -> primNullable encodeInt32s (0 :: Int32) v acc
  ColDate64Maybe    v -> primNullable encodeInt64s (0 :: Int64) v acc
  ColTime32Maybe    v -> primNullable encodeInt32s (0 :: Int32) v acc
  ColTime64Maybe    v -> primNullable encodeInt64s (0 :: Int64) v acc
  ColTimestampMaybe v -> primNullable encodeInt64s (0 :: Int64) v acc
  ColDurationMaybe  v -> primNullable encodeInt64s (0 :: Int64) v acc

  -- Nullable variable-length + fixed-size binary columns.
  ColUtf8Maybe v ->
    varNullableBoxed encodePlainUtf8 T.empty v acc
  ColBinaryMaybe v ->
    varNullableBoxed encodePlainBinary BS.empty v acc
  ColLargeUtf8Maybe v ->
    varNullableBoxed encodePlainLargeUtf8 T.empty v acc
  ColLargeBinaryMaybe v ->
    varNullableBoxed encodePlainLargeBinary BS.empty v acc
  ColFixedSizeBinaryMaybe w v ->
    primNullableBoxed (encodePlainFixedSizeBinary w) (BS.replicate w 0) v acc

  -- ============================================================
  -- Nested columns
  -- ============================================================
  ColStruct children ->
    let !n = if V.null children
               then 0
               else fromIntegral (columnLength (snd (V.head children))) :: Int64
        acc1 = addFieldNode n 0 acc
        childFields = fieldChildren f
    in V.ifoldl' (\a i (_, cc) -> encodeCol (V.unsafeIndex childFields i) cc a) acc1 children

  ColStructMaybe validity children ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
        childFields = fieldChildren f
    in V.ifoldl' (\a i (_, cc) -> encodeCol (V.unsafeIndex childFields i) cc a) acc1 children

  ColList offsets child ->
    let !n = fromIntegral (max 0 (VP.length offsets - 1)) :: Int64
        acc1 = addBufData (encodePlainInt32Column offsets) $ addFieldNode n 0 acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1

  ColListMaybe validity offsets child ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodePlainInt32Column offsets)
             $ addBufData (encodeNullBitmap validity)
             $ addFieldNode n nc acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1

  ColLargeList offsets child ->
    let !n = fromIntegral (max 0 (VP.length offsets - 1)) :: Int64
        acc1 = addBufData (encodePlainInt64Offsets offsets) $ addFieldNode n 0 acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1

  ColLargeListMaybe validity offsets child ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodePlainInt64Offsets offsets)
             $ addBufData (encodeNullBitmap validity)
             $ addFieldNode n nc acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1

  -- FixedSizeList has no offsets buffer: the length is implicit in
  -- the schema's FixedSizeList type, and the child array is exactly
  -- @parentLen * size@ long. We emit a single FieldNode for the
  -- parent then recurse into the child.
  ColFixedSizeList w child ->
    let !listLen   = max 1 w
        !parentLen = fromIntegral (columnLength child `quot` listLen) :: Int64
        acc1 = addFieldNode parentLen 0 acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1

  ColFixedSizeListMaybe _ validity child ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
        childField = childFieldAt f 0
    in encodeCol childField child acc1

  ColMap offsets keyChild valChild ->
    let !n = fromIntegral (max 0 (VP.length offsets - 1)) :: Int64
        acc1 = addBufData (encodePlainInt32Column offsets) $ addFieldNode n 0 acc
        -- Map child is a single struct field; the struct itself has
        -- one FieldNode + (keys, values) sub-fields.
        structField = childFieldAt f 0
        keyField    = childFieldAt structField 0
        valField    = childFieldAt structField 1
        !structLen  = fromIntegral (columnLength keyChild) :: Int64
        acc2 = addFieldNode structLen 0 acc1
        acc3 = encodeCol keyField keyChild acc2
    in encodeCol valField valChild acc3

  ColMapMaybe validity offsets keyChild valChild ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = validityNullCount validity
        acc1 = addBufData (encodePlainInt32Column offsets)
             $ addBufData (encodeNullBitmap validity)
             $ addFieldNode n nc acc
        structField = childFieldAt f 0
        keyField    = childFieldAt structField 0
        valField    = childFieldAt structField 1
        !structLen  = fromIntegral (columnLength keyChild) :: Int64
        acc2 = addFieldNode structLen 0 acc1
        acc3 = encodeCol keyField keyChild acc2
    in encodeCol valField valChild acc3

  -- Union columns don't carry a top-level validity bitmap (nulls are
  -- represented via the child arrays + the type_ids buffer).
  ColDenseUnion typeIds offsets children ->
    let !n = fromIntegral (VP.length typeIds) :: Int64
        acc1 = addBufData (encodePlainInt32Column offsets)
             $ addBufData (encodeInt8s typeIds)
             $ addFieldNode n 0 acc
        childFields = fieldChildren f
    in V.ifoldl' (\a i cc -> encodeCol (V.unsafeIndex childFields i) cc a) acc1 children

  ColSparseUnion typeIds children ->
    let !n = fromIntegral (VP.length typeIds) :: Int64
        acc1 = addBufData (encodeInt8s typeIds) $ addFieldNode n 0 acc
        childFields = fieldChildren f
    in V.ifoldl' (\a i cc -> encodeCol (V.unsafeIndex childFields i) cc a) acc1 children

  -- Dictionary-encoded columns emit the indices like a regular Int32
  -- column; the dictionary itself lives in a separate DictionaryBatch
  -- IPC message (handled at the stream-writer level, out of scope for
  -- this per-column encoder).
  ColDictionary _dictId indices _dictValues ->
    primFlat (encodePlainInt32Column indices) (VP.length indices) acc

-- ============================================================
-- Helpers
-- ============================================================

-- | Pick the i-th child field, defaulting to the parent on an out-of-
-- range index (which should never happen for a well-formed schema,
-- but we'd rather produce a degenerate record batch than crash).
childFieldAt :: Field -> Int -> Field
childFieldAt f i =
  let !cs = fieldChildren f
  in if i < V.length cs then V.unsafeIndex cs i else f

primFlat :: ByteString -> Int -> BuildAcc -> BuildAcc
primFlat bs n acc =
  addBufData bs (addFieldNode (fromIntegral n) 0 acc)

varFlat :: ByteString -> ByteString -> Int -> BuildAcc -> BuildAcc
varFlat offBs datBs n acc =
  addBufData datBs $ addBufData offBs
    $ addFieldNode (fromIntegral n) 0 acc

-- | Encode a @V.Vector (Maybe a)@ for an @Unboxed/Primitive a@ value
-- payload. Builds a validity bitmap + a dense payload (nulls filled
-- with the caller-supplied zero).
primNullable
  :: VP.Prim a
  => (VP.Vector a -> ByteString) -> a
  -> V.Vector (Maybe a) -> BuildAcc -> BuildAcc
primNullable enc zero vec acc =
  let !n  = fromIntegral (V.length vec) :: Int64
      !nc = fromIntegral (countNulls vec) :: Int64
      validity = V.map isJust vec
      vals = VP.generate (V.length vec) $ \i ->
               fromMaybe zero (V.unsafeIndex vec i)
  in addBufData (enc vals)
     $ addBufData (encodeNullBitmap validity)
     $ addFieldNode n nc acc

-- | Encode a @V.Vector (Maybe a)@ backed by 'V.Vector' (i.e. not
-- primitive — 'Bool' / 'ByteString' / 'Text'). The 'V.Vector'-side
-- variant can't reuse 'primNullable' because the payload lives in a
-- boxed vector.
primNullableBoxed
  :: (V.Vector a -> ByteString) -> a
  -> V.Vector (Maybe a) -> BuildAcc -> BuildAcc
primNullableBoxed enc zero vec acc =
  let !n  = fromIntegral (V.length vec) :: Int64
      !nc = fromIntegral (countNulls vec) :: Int64
      validity = V.map isJust vec
      vals = V.map (fromMaybe zero) vec
  in addBufData (enc vals)
     $ addBufData (encodeNullBitmap validity)
     $ addFieldNode n nc acc

-- | Encode a @V.Vector (Maybe a)@ that serialises as an
-- (offsets, data) pair (Utf8 / Binary / LargeUtf8 / LargeBinary).
varNullableBoxed
  :: (V.Vector a -> (ByteString, ByteString)) -> a
  -> V.Vector (Maybe a) -> BuildAcc -> BuildAcc
varNullableBoxed enc zero vec acc =
  let !n  = fromIntegral (V.length vec) :: Int64
      !nc = fromIntegral (countNulls vec) :: Int64
      validity = V.map isJust vec
      vals = V.map (fromMaybe zero) vec
      (offBs, datBs) = enc vals
  in addBufData datBs
     $ addBufData offBs
     $ addBufData (encodeNullBitmap validity)
     $ addFieldNode n nc acc

-- | Count @False@ entries in a validity bitmap.
validityNullCount :: V.Vector Bool -> Int64
validityNullCount = V.foldl' (\c v -> if v then c else c + 1) 0

-- * Stream / File writers

arrowMagic :: ByteString
arrowMagic = "ARROW1"

-- | Write a complete Arrow IPC stream (schema + record batches + EOS).
writeArrowStream :: Schema -> V.Vector (V.Vector ColumnArray) -> ByteString
writeArrowStream schema batches =
  let !schemaBs = encodeIPCMessage (SchemaMessage schema)
      batchParts = V.toList $ V.map (buildRecordBatch schema) batches
      eos = BL.toStrict $ B.toLazyByteString $
        B.word32LE 0xFFFFFFFF <> B.int32LE 0
  in BS.concat (schemaBs : batchParts ++ [eos])

-- | Write a complete Arrow IPC file (magic + schema + batches + footer + magic).
writeArrowFile :: Schema -> V.Vector (V.Vector ColumnArray) -> ByteString
writeArrowFile schema batches =
  let !schemaBs = encodeIPCMessage (SchemaMessage schema)
      !paddedSchemaLen = alignUp8 (BS.length schemaBs)
      !schemaPad = paddedSchemaLen - BS.length schemaBs
      !headerSize = 8 + paddedSchemaLen

      batchBss = V.map (buildRecordBatch schema) batches

      (_, revOffsets) = V.foldl' (\(!off, !acc) bbs ->
        (off + fromIntegral (BS.length bbs), off : acc)
        ) (fromIntegral headerSize :: Int64, []) batchBss
      blockOffsets = reverse revOffsets

      footerBs = encodeFooter schema blockOffsets
      !footerLen = BS.length footerBs
  in BS.concat
      [ arrowMagic, BS.pack [0, 0]
      , schemaBs, BS.replicate schemaPad 0
      , BS.concat (V.toList batchBss)
      , footerBs
      , BL.toStrict (B.toLazyByteString (B.int32LE (fromIntegral footerLen)))
      , arrowMagic
      ]

encodeFooter :: Schema -> [Int64] -> ByteString
encodeFooter schema blockOffsets =
  let !schemaBs = encodeIPCMessage (SchemaMessage schema)
  in BL.toStrict $ B.toLazyByteString $
      B.int32LE (fromIntegral (BS.length schemaBs))
      <> B.byteString schemaBs
      <> B.int32LE (fromIntegral (length blockOffsets))
      <> foldl' (\acc off -> acc <> B.int64LE off) mempty blockOffsets
  where
    foldl' _ z [] = z
    foldl' g !z (x:xs) = foldl' g (g z x) xs
