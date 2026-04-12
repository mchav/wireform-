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
import Data.Word (Word8)
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

encodeCol :: Field -> ColumnArray -> BuildAcc -> BuildAcc
encodeCol f col acc = case col of
  ColInt8 vec ->
    addBufData (encodeInt8s vec) $ addFieldNode (fromIntegral (VP.length vec)) 0 acc
  ColInt16 vec ->
    addBufData (encodeInt16s vec) $ addFieldNode (fromIntegral (VP.length vec)) 0 acc
  ColInt32 vec ->
    addBufData (encodePlainInt32Column vec) $ addFieldNode (fromIntegral (VP.length vec)) 0 acc
  ColInt64 vec ->
    addBufData (encodePlainInt64Column vec) $ addFieldNode (fromIntegral (VP.length vec)) 0 acc
  ColFloat vec ->
    addBufData (encodePlainFloat vec) $ addFieldNode (fromIntegral (VP.length vec)) 0 acc
  ColDouble vec ->
    addBufData (encodePlainDouble vec) $ addFieldNode (fromIntegral (VP.length vec)) 0 acc
  ColBool vec ->
    addBufData (encodePlainBool vec) $ addFieldNode (fromIntegral (V.length vec)) 0 acc
  ColUtf8 vec ->
    let (offBs, datBs) = encodePlainUtf8 vec
    in addBufData datBs $ addBufData offBs $ addFieldNode (fromIntegral (V.length vec)) 0 acc
  ColBinary vec ->
    let (offBs, datBs) = encodePlainBinary vec
    in addBufData datBs $ addBufData offBs $ addFieldNode (fromIntegral (V.length vec)) 0 acc

  ColInt8Maybe vec ->
    let !n = fromIntegral (V.length vec) :: Int64
        !nc = fromIntegral (countNulls vec) :: Int64
        validity = V.map isJust vec
        vals = VP.generate (V.length vec) $ \i -> case V.unsafeIndex vec i of
          Just v -> v; Nothing -> 0
    in addBufData (encodeInt8s vals) $ addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
  ColInt16Maybe vec ->
    let !n = fromIntegral (V.length vec) :: Int64
        !nc = fromIntegral (countNulls vec) :: Int64
        validity = V.map isJust vec
        vals = VP.generate (V.length vec) $ \i -> case V.unsafeIndex vec i of
          Just v -> v; Nothing -> 0
    in addBufData (encodeInt16s vals) $ addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
  ColInt32Maybe vec ->
    let !n = fromIntegral (V.length vec) :: Int64
        !nc = fromIntegral (countNulls vec) :: Int64
        validity = V.map isJust vec
        vals = VP.generate (V.length vec) $ \i -> case V.unsafeIndex vec i of
          Just v -> v; Nothing -> 0
    in addBufData (encodePlainInt32Column vals) $ addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
  ColInt64Maybe vec ->
    let !n = fromIntegral (V.length vec) :: Int64
        !nc = fromIntegral (countNulls vec) :: Int64
        validity = V.map isJust vec
        vals = VP.generate (V.length vec) $ \i -> case V.unsafeIndex vec i of
          Just v -> v; Nothing -> 0
    in addBufData (encodePlainInt64Column vals) $ addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
  ColFloatMaybe vec ->
    let !n = fromIntegral (V.length vec) :: Int64
        !nc = fromIntegral (countNulls vec) :: Int64
        validity = V.map isJust vec
        vals = VP.generate (V.length vec) $ \i -> case V.unsafeIndex vec i of
          Just v -> v; Nothing -> 0
    in addBufData (encodePlainFloat vals) $ addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
  ColDoubleMaybe vec ->
    let !n = fromIntegral (V.length vec) :: Int64
        !nc = fromIntegral (countNulls vec) :: Int64
        validity = V.map isJust vec
        vals = VP.generate (V.length vec) $ \i -> case V.unsafeIndex vec i of
          Just v -> v; Nothing -> 0
    in addBufData (encodePlainDouble vals) $ addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
  ColBoolMaybe vec ->
    let !n = fromIntegral (V.length vec) :: Int64
        !nc = fromIntegral (countNulls vec) :: Int64
        validity = V.map isJust vec
        vals = V.map (fromMaybe False) vec
    in addBufData (encodePlainBool vals) $ addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
  ColUtf8Maybe vec ->
    let !n = fromIntegral (V.length vec) :: Int64
        !nc = fromIntegral (countNulls vec) :: Int64
        validity = V.map isJust vec
        texts = V.map (fromMaybe T.empty) vec
        (offBs, datBs) = encodePlainUtf8 texts
    in addBufData datBs $ addBufData offBs $ addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
  ColBinaryMaybe vec ->
    let !n = fromIntegral (V.length vec) :: Int64
        !nc = fromIntegral (countNulls vec) :: Int64
        validity = V.map isJust vec
        bins = V.map (fromMaybe BS.empty) vec
        (offBs, datBs) = encodePlainBinary bins
    in addBufData datBs $ addBufData offBs $ addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc

  ColStruct children ->
    let !n = if V.null children then 0 else fromIntegral (columnLength (snd (V.head children))) :: Int64
        acc1 = addFieldNode n 0 acc
        childFields = fieldChildren f
    in V.ifoldl' (\a i (_, cc) -> encodeCol (V.unsafeIndex childFields i) cc a) acc1 children
  ColStructMaybe validity children ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = fromIntegral (V.foldl' (\c v -> if not v then c + 1 else c) (0 :: Int) validity) :: Int64
        acc1 = addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
        childFields = fieldChildren f
    in V.ifoldl' (\a i (_, cc) -> encodeCol (V.unsafeIndex childFields i) cc a) acc1 children
  ColList offsets child ->
    let !n = fromIntegral (max 0 (VP.length offsets - 1)) :: Int64
        acc1 = addBufData (encodePlainInt32Column offsets) $ addFieldNode n 0 acc
        childField = V.head (fieldChildren f)
    in encodeCol childField child acc1
  ColListMaybe validity offsets child ->
    let !n = fromIntegral (V.length validity) :: Int64
        !nc = fromIntegral (V.foldl' (\c v -> if not v then c + 1 else c) (0 :: Int) validity) :: Int64
        acc1 = addBufData (encodePlainInt32Column offsets) $ addBufData (encodeNullBitmap validity) $ addFieldNode n nc acc
        childField = V.head (fieldChildren f)
    in encodeCol childField child acc1
  ColDictionary _dictId indices _dictValues ->
    let !n = fromIntegral (VP.length indices) :: Int64
    in addBufData (encodePlainInt32Column indices) $ addFieldNode n 0 acc

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
