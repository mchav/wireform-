{-# LANGUAGE OverloadedStrings #-}
-- | Internal round-trip tests for the Arrow IPC writer + reader.
--
-- Every 'ColumnArray' constructor the writer emits must be parsed
-- back correctly by 'readArrowStream' + 'materializeRecordBatch'.
--
-- The IPC framing wireform-arrow uses is a simplified encoding (see
-- @Arrow.IPC@) rather than a real flatbuffer, so pyarrow can't read
-- these bytes directly; the golden interop item (roadmap B.4) is
-- tracked separately. These tests prove the writer-reader pair is
-- self-consistent across every supported column shape.
module Main (main) where

import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Data.Word (Word8, Word16, Word32, Word64)
import System.Exit (exitFailure)

import Arrow.Column
  ( ColumnArray (..)
  , columnLength
  , materializeRecordBatch
  )
import Arrow.File (asBatches, asSchema, readArrowStream)
import Arrow.Stream
  ( BodyCompressionCodec (..)
  , DictHandling (..)
  , WriteOptions (..)
  , decodeArrowStream
  , defaultWriteOptions
  , encodeArrowStream
  , encodeArrowStreamWith
  , openStreamReader
  , streamReaderNext
  , streamReaderSchema
  , streamReaderToList
  )
import Arrow.FlatBufferIPC
  ( buildSchemaMessage
  , decodeSchemaMessage
  , Tensor (..)
  , TensorDim (..)
  , encodeTensorFrame
  , decodeTensorFrame
  , SparseTensor (..)
  , encodeSparseTensorFrame
  , decodeSparseTensorFrame
  )
import Arrow.Types
import Arrow.Write (writeArrowStream)

main :: IO ()
main = do
  putStrLn "wireform-arrow writer/reader round-trip suite"

  -- Every column: build a single-batch Arrow stream containing just
  -- that column, serialise it, parse it, materialise, and assert
  -- the recovered ColumnArray equals the one we put in.

  roundTripPrim "Int8"   (ColInt8   (VP.fromList [0, 1, -1, 100, -128, 127]))
  roundTripPrim "Int16"  (ColInt16  (VP.fromList [0, 1, -1, 32767, -32768]))
  roundTripPrim "Int32"  (ColInt32  (VP.fromList [0, 1, -1, maxBound, minBound]))
  roundTripPrim "Int64"  (ColInt64  (VP.fromList [0, 1, -1, maxBound, minBound]))
  roundTripPrim "UInt8"  (ColUInt8  (VP.fromList ([0, 255] :: [Word8])))
  roundTripPrim "UInt16" (ColUInt16 (VP.fromList ([0, 65535] :: [Word16])))
  roundTripPrim "UInt32" (ColUInt32 (VP.fromList ([0, maxBound] :: [Word32])))
  roundTripPrim "UInt64" (ColUInt64 (VP.fromList ([0, maxBound] :: [Word64])))
  roundTripPrim "Float16" (ColFloat16 (VP.fromList ([0, 0x3C00, 0xBC00] :: [Word16])))
  roundTripPrim "Float"  (ColFloat  (VP.fromList [0.0, 1.5, -2.25, 3.14 :: Float]))
  roundTripPrim "Double" (ColDouble (VP.fromList [0.0, 1.5, -2.25, 3.14159265 :: Double]))
  roundTripPrim "Bool"   (ColBool   (V.fromList [True, False, True, False, True]))

  roundTripPrim "Date32" (ColDate32 (VP.fromList [0 :: Int32, 18000, -1]))
  roundTripPrim "Date64" (ColDate64 (VP.fromList [0 :: Int64, 1700000000000]))
  roundTripPrim "Time32" (ColTime32 (VP.fromList [0 :: Int32, 12345]))
  roundTripPrim "Time64" (ColTime64 (VP.fromList [0 :: Int64, 12345000000]))
  roundTripPrim "Timestamp"
    (ColTimestamp (VP.fromList [0 :: Int64, 1700000000_000_000_000]))
  roundTripPrim "Duration" (ColDuration (VP.fromList [0 :: Int64, 60_000_000_000]))

  roundTripPrim "IntervalYearMonth"
    (ColIntervalYearMonth (VP.fromList [0 :: Int32, 12, -6, 100]))
  roundTripPrim "IntervalDayTime"
    (ColIntervalDayTime (VP.fromList [1 :: Int32, 2, 30]) (VP.fromList [500, -1, 0]))
  roundTripPrim "IntervalMonthDayNano"
    (ColIntervalMonthDayNano
        (VP.fromList [1 :: Int32, 2])
        (VP.fromList [3 :: Int32, 4])
        (VP.fromList [1000 :: Int64, -500]))

  roundTripPrim "Decimal128" (ColDecimal128 18 2 (V.fromList
    [ BS.replicate 16 0
    , BS.pack [0x64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    ]))
  roundTripPrim "Decimal256" (ColDecimal256 38 4 (V.fromList
    [ BS.replicate 32 0
    , BS.pack (0x2A : replicate 31 0)
    ]))

  roundTripPrim "FixedSizeBinary"
    (ColFixedSizeBinary 4 (V.fromList
      [ BS.pack [0, 1, 2, 3], BS.pack [0xFF, 0xFE, 0xFD, 0xFC] ]))

  roundTripPrim "Utf8"   (ColUtf8   (V.fromList ["alpha", "beta", "", "\xe2\x9a\xa1"]))
  roundTripPrim "Binary" (ColBinary (V.fromList [BS.pack [1,2,3], BS.empty, BS.pack [0xFF]]))
  roundTripPrim "LargeUtf8"   (ColLargeUtf8   (V.fromList ["alpha", "beta"]))
  roundTripPrim "LargeBinary" (ColLargeBinary (V.fromList [BS.pack [0,1,2], BS.pack [0xFF]]))

  -- Nullable variants.
  roundTripPrim "Int8Maybe"
    (ColInt8Maybe (V.fromList [Just 1, Nothing, Just (-1), Just 42]))
  roundTripPrim "Int16Maybe"
    (ColInt16Maybe (V.fromList [Just 100, Nothing, Just (-200)]))
  roundTripPrim "Int32Maybe"
    (ColInt32Maybe (V.fromList [Nothing, Just 0, Just maxBound]))
  roundTripPrim "Int64Maybe"
    (ColInt64Maybe (V.fromList [Just 0, Nothing, Just (-1)]))
  roundTripPrim "UInt8Maybe"
    (ColUInt8Maybe (V.fromList [Just (0 :: Word8), Just 255, Nothing]))
  roundTripPrim "UInt16Maybe"
    (ColUInt16Maybe (V.fromList [Just (0 :: Word16), Nothing]))
  roundTripPrim "UInt32Maybe"
    (ColUInt32Maybe (V.fromList [Just (0 :: Word32), Nothing, Just maxBound]))
  roundTripPrim "UInt64Maybe"
    (ColUInt64Maybe (V.fromList [Just (0 :: Word64), Nothing]))
  roundTripPrim "Float16Maybe"
    (ColFloat16Maybe (V.fromList [Just (0 :: Word16), Just 0x3C00, Nothing]))
  roundTripPrim "FloatMaybe"
    (ColFloatMaybe (V.fromList [Just 1.5, Nothing, Just (-2.25 :: Float)]))
  roundTripPrim "DoubleMaybe"
    (ColDoubleMaybe (V.fromList [Just 1.5, Nothing]))
  roundTripPrim "BoolMaybe"
    (ColBoolMaybe (V.fromList [Just True, Nothing, Just False, Just True]))

  roundTripPrim "Utf8Maybe"
    (ColUtf8Maybe (V.fromList [Just "alpha", Nothing, Just ""]))
  roundTripPrim "BinaryMaybe"
    (ColBinaryMaybe (V.fromList [Just (BS.pack [1, 2]), Nothing]))
  roundTripPrim "LargeUtf8Maybe"
    (ColLargeUtf8Maybe (V.fromList [Nothing, Just "beta"]))
  roundTripPrim "LargeBinaryMaybe"
    (ColLargeBinaryMaybe (V.fromList [Just (BS.pack [0xFF]), Nothing]))
  roundTripPrim "FixedSizeBinaryMaybe"
    (ColFixedSizeBinaryMaybe 3
      (V.fromList [Just (BS.pack [1, 2, 3]), Nothing, Just (BS.pack [4, 5, 6])]))

  roundTripPrim "Date32Maybe" (ColDate32Maybe (V.fromList [Just (0 :: Int32), Nothing]))
  roundTripPrim "Date64Maybe" (ColDate64Maybe (V.fromList [Just (0 :: Int64), Nothing]))
  roundTripPrim "Time32Maybe" (ColTime32Maybe (V.fromList [Just (0 :: Int32), Nothing]))
  roundTripPrim "Time64Maybe" (ColTime64Maybe (V.fromList [Just (0 :: Int64), Nothing]))
  roundTripPrim "TimestampMaybe" (ColTimestampMaybe (V.fromList [Just (0 :: Int64), Nothing]))
  roundTripPrim "DurationMaybe" (ColDurationMaybe (V.fromList [Just (0 :: Int64), Nothing]))

  -- ============================================================
  -- Nested columns
  -- ============================================================

  -- Struct with two primitive children.
  roundTripNested "Struct"
    (nestedField "s" False AStruct $ V.fromList
       [ plainField "id"   False (AInt 64 True)
       , plainField "name" False AUtf8
       ])
    (ColStruct $ V.fromList
       [ ("id",   ColInt64 (VP.fromList [1, 2, 3]))
       , ("name", ColUtf8  (V.fromList ["a", "b", "c"]))
       ])

  roundTripNested "StructMaybe"
    (nestedField "s" True AStruct $ V.fromList
       [ plainField "id"   False (AInt 32 True)
       , plainField "flag" False ABool
       ])
    (ColStructMaybe
       (V.fromList [True, False, True])
       (V.fromList
         [ ("id",   ColInt32 (VP.fromList [1, 2, 3]))
         , ("flag", ColBool  (V.fromList [True, False, True]))
         ]))

  roundTripNested "List<int32>"
    (nestedField "l" False AList $ V.fromList
      [ plainField "item" False (AInt 32 True) ])
    (ColList (VP.fromList [0, 2, 2, 5])
       (ColInt32 (VP.fromList [10, 20, 30, 40, 50])))

  roundTripNested "ListMaybe<int32>"
    (nestedField "l" True AList $ V.fromList
      [ plainField "item" False (AInt 32 True) ])
    (ColListMaybe
       (V.fromList [True, False, True])
       (VP.fromList [0, 2, 2, 5])
       (ColInt32 (VP.fromList [10, 20, 30, 40, 50])))

  roundTripNested "LargeList<int32>"
    (nestedField "l" False ALargeList $ V.fromList
      [ plainField "item" False (AInt 32 True) ])
    (ColLargeList (VP.fromList [0, 2, 2, 5])
       (ColInt32 (VP.fromList [1, 2, 3, 4, 5])))

  roundTripNested "LargeListMaybe<int32>"
    (nestedField "l" True ALargeList $ V.fromList
      [ plainField "item" False (AInt 32 True) ])
    (ColLargeListMaybe
       (V.fromList [True, False, True])
       (VP.fromList [0, 2, 2, 5])
       (ColInt32 (VP.fromList [1, 2, 3, 4, 5])))

  roundTripNested "FixedSizeList<3 of int32>"
    (nestedField "l" False (AFixedSizeList 3) $ V.fromList
      [ plainField "item" False (AInt 32 True) ])
    (ColFixedSizeList 3
       (ColInt32 (VP.fromList [1, 2, 3, 4, 5, 6])))

  roundTripNested "FixedSizeListMaybe<2 of int32>"
    (nestedField "l" True (AFixedSizeList 2) $ V.fromList
      [ plainField "item" False (AInt 32 True) ])
    (ColFixedSizeListMaybe 2
       (V.fromList [True, False, True])
       (ColInt32 (VP.fromList [1, 2, 3, 4, 5, 6])))

  -- Map<string, int32>. Arrow encodes maps as a list of struct
  -- <key, value> pairs; the map field has one child (the struct).
  roundTripNested "Map<string, int32>"
    (nestedField "m" False (AMap False) $ V.fromList
      [ nestedField "entries" False AStruct $ V.fromList
          [ plainField "key"   False AUtf8
          , plainField "value" False (AInt 32 True)
          ]
      ])
    (ColMap
       (VP.fromList [0, 2, 2, 3])
       (ColUtf8 (V.fromList ["a", "b", "c"]))
       (ColInt32 (VP.fromList [1, 2, 3])))

  -- Dense union over (int32, utf8).
  roundTripNested "DenseUnion<int32, utf8>"
    (nestedField "u" False (AUnion Dense (V.fromList [0, 1])) $ V.fromList
      [ plainField "v_int"  False (AInt 32 True)
      , plainField "v_text" False AUtf8
      ])
    (ColDenseUnion
       (VP.fromList [0, 1, 0])
       (VP.fromList [0, 0, 1])
       (V.fromList
         [ ColInt32 (VP.fromList [100, 200])
         , ColUtf8  (V.fromList ["hello"])
         ]))

  -- Sparse union over (bool, int32).
  roundTripNested "SparseUnion<bool, int32>"
    (nestedField "u" False (AUnion Sparse (V.fromList [0, 1])) $ V.fromList
      [ plainField "flag"  False ABool
      , plainField "value" False (AInt 32 True)
      ])
    (ColSparseUnion
       (VP.fromList [0, 1, 0])
       (V.fromList
         [ ColBool  (V.fromList [True,  False, False])
         , ColInt32 (VP.fromList [0, 42, 0])
         ]))

  -- FlatBuffers reader / writer round-trip: build a typical
  -- multi-column batch with the FB writer, parse back with the FB
  -- reader, assert schema equality.
  flatBufRoundTrip
  flatBufSchemaSelfCheck

  putStrLn "All wireform-arrow round-trip tests passed."

flatBufSchemaSelfCheck :: IO ()
flatBufSchemaSelfCheck = do
  let cases =
        [ Schema (V.fromList [plainField "a" False (AInt 32 True)]) Little
        , Schema (V.fromList
            [ plainField "id"     False (AInt 64 True)
            , plainField "name"   True  AUtf8
            , plainField "amount" False (ADecimal 12 4)
            , plainField "ts"     True  (ATimestamp Nanosecond (Just "UTC"))
            , plainField "blob"   True  ABinary
            , plainField "tag"    False (AFixedSizeBinary 16)
            ]) Little
        , -- Post-V5 type tags (Utf8View / BinaryView / RunEndEncoded /
          -- ListView / LargeListView). Arrow.Column doesn't materialise
          -- their data buffers, but the schema flatbuffer round-trips
          -- so wireform can interoperate with newer Arrow producers.
          Schema (V.fromList
            [ plainField "v"   True  AUtf8View
            , plainField "b"   True  ABinaryView
            , plainField "ree" True  ARunEndEncoded
            , plainField "lv"  True  AListView
            , plainField "llv" True  ALargeListView
            ]) Little
        ]
  mapM_ (\sch -> do
            let bs = buildSchemaMessage sch
            case decodeSchemaMessage bs of
              Right got | got == sch ->
                putStrLn $ "OK: FlatBuffers schema self-roundtrip: " ++ describe sch
              Right got ->
                failTest $ "FB schema roundtrip mismatch:\n got: "
                            ++ show got ++ "\n exp: " ++ show sch
              Left e ->
                failTest $ "FB schema roundtrip decode failed: " ++ e
        ) cases
  where
    describe sch =
      "(" ++ show (V.length (arrowFields sch)) ++ " field"
        ++ (if V.length (arrowFields sch) == 1 then "" else "s") ++ ")"

flatBufRoundTrip :: IO ()
flatBufRoundTrip = do
  -- Multi-column round-trip exercising the high-level API in
  -- "Arrow.Stream": Schema + batches go in, bytes come out, and
  -- the inverse recovers the same shape.
  highLevelRoundTrip "Multi-column"
    (Schema
       { arrowFields = V.fromList
           [ plainField "i" False (AInt 32 True)
           , plainField "s" True  AUtf8
           ]
       , arrowEndianness = Little
       })
    (V.fromList
       [ ColInt32      (VP.fromList ([1, 2, 3] :: [Int32]))
       , ColUtf8Maybe  (V.fromList [Just "x", Nothing, Just "z"])
       ])

  -- Post-V5 columns: writer + reader byte-compatible end to end.
  highLevelRoundTrip "Utf8View"
    (Schema (V.singleton (plainField "v" True AUtf8View)) Little)
    (V.singleton (ColUtf8ViewMaybe (V.fromList
       [ Just "short"
       , Nothing
       , Just "this string is definitely longer than twelve bytes"
       ])))

  highLevelRoundTrip "ListView<int32>"
    (Schema
       (V.singleton
          (nestedField "lv" False AListView (V.singleton
             (plainField "item" False (AInt 32 True)))))
       Little)
    (V.singleton (ColListView
       (VP.fromList ([0, 2, 5] :: [Int32]))
       (VP.fromList ([2, 3, 1] :: [Int32]))
       (ColInt32 (VP.fromList ([10,20,30,40,50,60] :: [Int32])))))

  highLevelRoundTrip "RunEndEncoded(int32, int64?)"
    (Schema
       (V.singleton
          (nestedField "ree" True ARunEndEncoded $ V.fromList
             [ plainField "run_ends" False (AInt 32 True)
             , plainField "values"   True  (AInt 64 True)
             ]))
       Little)
    (V.singleton (ColRunEndEncoded
       (ColInt32 (VP.fromList ([3, 5, 8] :: [Int32])))
       (ColInt64Maybe (V.fromList [Just 100, Nothing, Just 300]))))

  -- Dictionary-encoded utf8 — the high-level API auto-extracts
  -- the dictionary batch and auto-resolves on read.
  let dictField = Field "d" True AUtf8 V.empty
                    (Just (DictionaryEncoding 0 (AInt 32 True) False))
  highLevelRoundTrip "Dictionary<utf8>"
    (Schema (V.singleton dictField) Little)
    (V.singleton (ColDictionary 0
        (VP.fromList ([0, 1, 0, 2, 1] :: [Int32]))
        (ColUtf8 (V.fromList ["a", "b", "c"]))))

  -- Streaming reader: pull batches one at a time, then drain.
  streamingRoundTrip
    (Schema (V.fromList [plainField "n" False (AInt 32 True)]) Little)
    [ V.singleton (ColInt32 (VP.fromList ([1, 2] :: [Int32])))
    , V.singleton (ColInt32 (VP.fromList ([3] :: [Int32])))
    , V.singleton (ColInt32 (VP.fromList ([4, 5, 6, 7] :: [Int32])))
    ]

  -- ZSTD body compression (writer + reader): exercises
  -- BodyCompression on a multi-column batch, asserting the
  -- decoded values match the source.
  bodyCompressionRoundTrip BodyZstd
    (Schema (V.fromList
       [ plainField "n" False (AInt 64 True)
       , plainField "s" False AUtf8
       ]) Little)
    (V.fromList
       [ ColInt64 (VP.fromList
            ([1..1000] :: [Int64]))   -- enough bytes that ZSTD shrinks
       , ColUtf8 (V.replicate 1000 "highly-compressible-payload")
       ])

  -- LZ4_FRAME body compression: same shape / sizing as the ZSTD
  -- case. Verifies the lz4-hs Codec.Lz4 frame compressor +
  -- decompressor round-trip through the full BodyCompression
  -- pipeline (per-buffer envelope, length prefix, offsets
  -- rewritten on decode, etc.).
  bodyCompressionRoundTrip LZ4Frame
    (Schema (V.fromList
       [ plainField "n" False (AInt 64 True)
       , plainField "s" False AUtf8
       ]) Little)
    (V.fromList
       [ ColInt64 (VP.fromList ([1..1000] :: [Int64]))
       , ColUtf8 (V.replicate 1000 "highly-compressible-payload")
       ])

  -- DictReplaceOnChange: two batches with the SAME dict id but
  -- different values. The writer should emit two dict batches;
  -- the reader should resolve each record batch against the
  -- most-recently-emitted dict for that id.
  dictReplacementRoundTrip

  -- Tensor message round-trip.
  tensorRoundTrip

  -- SparseTensor (COO) round-trip.
  sparseTensorRoundTrip

sparseTensorRoundTrip :: IO ()
sparseTensorRoundTrip = do
  -- Tiny 3x3 sparse int32 tensor with 2 non-zeros at (0,1) and
  -- (2,0). COO indices are Int64 pairs.
  let !idx  = BS.pack
        [ 0,0,0,0,0,0,0,0  -- row 0
        , 1,0,0,0,0,0,0,0  -- col 1
        , 2,0,0,0,0,0,0,0  -- row 2
        , 0,0,0,0,0,0,0,0  -- col 0
        ]
      !vals = BS.pack
        [ 7,0,0,0    -- value 7
        , 9,0,0,0    -- value 9
        ]
      !st = SparseTensor
        { sparseTensorType       = AInt 32 True
        , sparseTensorShape      = V.fromList
            [ TensorDim 3 "rows", TensorDim 3 "cols" ]
        , sparseNonZeroLength    = 2
        , sparseIndicesType      = AInt 64 True
        , sparseIndicesBody      = idx
        , sparseIndicesCanonical = True
        , sparseTensorBody       = vals
        }
      !frame = encodeSparseTensorFrame st
  case decodeSparseTensorFrame frame of
    Left e -> failTest $ "decodeSparseTensorFrame: " ++ e
    Right (sout, _)
      | sparseTensorType sout == AInt 32 True
      , sparseNonZeroLength sout == 2
      , sparseIndicesBody sout == idx
      , sparseTensorBody sout == vals ->
          putStrLn "OK: SparseTensor message round-trip (COO, 3x3 int32, nnz=2)"
      | otherwise ->
          failTest $ "sparse tensor mismatch:\n got " ++ show sout

tensorRoundTrip :: IO ()
tensorRoundTrip = do
  -- 2×3 tensor of Int32: raw little-endian body, row-major.
  let !body = BS.pack
        [ 0x01,0,0,0, 0x02,0,0,0, 0x03,0,0,0
        , 0x04,0,0,0, 0x05,0,0,0, 0x06,0,0,0
        ]
      !tin = Tensor
        { tensorType = AInt 32 True
        , tensorShape = V.fromList
            [ TensorDim 2 "rows", TensorDim 3 "cols" ]
        , tensorStrides = V.empty
        , tensorBody = body
        }
      !frame = encodeTensorFrame tin
  case decodeTensorFrame frame of
    Left e -> failTest $ "decodeTensorFrame: " ++ e
    Right (tout, rest)
      | BS.null rest
      , tensorType tout == AInt 32 True
      , V.toList (tensorShape tout) ==
          [ TensorDim 2 "rows", TensorDim 3 "cols" ]
      , tensorBody tout == body ->
          putStrLn "OK: Tensor message round-trip (2x3 int32)"
      | otherwise ->
          failTest $ "tensor round-trip mismatch:\n got "
                      ++ show tout ++ " rest="
                      ++ show (BS.length rest) ++ "B"

dictReplacementRoundTrip :: IO ()
dictReplacementRoundTrip = do
  let !sch = Schema
        (V.singleton
           (Field "d" True AUtf8 V.empty
              (Just (DictionaryEncoding 0 (AInt 32 True) False))))
        Little
      !batch1 = V.singleton $ ColDictionary 0
        (VP.fromList [0, 1, 0])
        (ColUtf8 (V.fromList ["a", "b"]))
      !batch2 = V.singleton $ ColDictionary 0
        (VP.fromList [0, 1, 0])
        (ColUtf8 (V.fromList ["x", "y"]))
      !opts  = defaultWriteOptions { writeDictHandling = DictReplaceOnChange }
      !bytes = encodeArrowStreamWith opts sch [batch1, batch2]
  case decodeArrowStream bytes of
    Left e -> failTest $ "dict-replace round-trip: " ++ e
    Right (_, batches)
      | [b1, b2] <- batches
      , V.length b1 == 1
      , V.length b2 == 1 ->
          let !c1 = V.unsafeIndex b1 0
              !c2 = V.unsafeIndex b2 0
          in case (c1, c2) of
               (ColDictionary _ ix1 v1, ColDictionary _ ix2 v2)
                 | V.toList (valuesToList v1) == ["a", "b"]
                 , V.toList (valuesToList v2) == ["x", "y"]
                 , VP.toList ix1 == [0, 1, 0]
                 , VP.toList ix2 == [0, 1, 0] ->
                     putStrLn "OK: dictionary replacement across batches"
               _ -> failTest $ "dict-replace mismatch:\n got "
                                ++ show batches
      | otherwise ->
          failTest $ "dict-replace expected 2 batches, got "
                      ++ show (length batches)
  where
    valuesToList (ColUtf8 v)          = v
    valuesToList (ColUtf8Maybe v)     = V.mapMaybe id v
    valuesToList _                    = V.empty

bodyCompressionRoundTrip :: BodyCompressionCodec -> Schema -> V.Vector ColumnArray -> IO ()
bodyCompressionRoundTrip codec sch cols = do
  let !opts  = defaultWriteOptions { writeBodyCompression = Just codec }
      !bytes = encodeArrowStreamWith opts sch [cols]
  case decodeArrowStream bytes of
    Left e -> failTest $ "body-compression round-trip: " ++ e
    Right (_sch', batches)
      | [got] <- batches, got == cols ->
          putStrLn $ "OK: body compression " ++ show codec
                       ++ " (" ++ show (BS.length bytes) ++ " bytes)"
      | otherwise ->
          failTest $ "body-compression mismatch: got "
                       ++ show batches

streamingRoundTrip :: Schema -> [V.Vector ColumnArray] -> IO ()
streamingRoundTrip sch batches = do
  let bytes = encodeArrowStream defaultWriteOptions sch batches
  case openStreamReader bytes of
    Left e -> failTest $ "openStreamReader: " ++ e
    Right rd0 -> do
      when (streamReaderSchema rd0 /= sch) $
        failTest "streamReaderSchema mismatch"
      -- Pull first batch via streamReaderNext; drain rest via toList.
      case streamReaderNext rd0 of
        Left e -> failTest $ "streamReaderNext (first): " ++ e
        Right Nothing -> failTest "streamReaderNext: stream empty"
        Right (Just (cols0, rd1)) -> do
          when (cols0 /= head batches) $
            failTest $ "streamReaderNext (first) mismatch:\n got "
                        ++ show cols0 ++ "\n exp " ++ show (head batches)
          case streamReaderToList rd1 of
            Left e -> failTest $ "streamReaderToList: " ++ e
            Right rest
              | rest == drop 1 batches ->
                  putStrLn "OK: streaming reader iterates all batches"
              | otherwise ->
                  failTest $ "streamReaderToList: tail mismatch\n got "
                              ++ show rest
                              ++ "\n exp " ++ show (drop 1 batches)

-- | Generic single-batch round-trip helper for the high-level
-- 'encodeArrowStream' / 'decodeArrowStream' API.
highLevelRoundTrip :: String -> Schema -> V.Vector ColumnArray -> IO ()
highLevelRoundTrip label sch cols = do
  let bytes = encodeArrowStream defaultWriteOptions sch [cols]
  case decodeArrowStream bytes of
    Left e -> failTest $ label ++ ": decodeArrowStream: " ++ e
    Right (sch', batches)
      | length batches /= 1 ->
          failTest $ label ++ ": expected 1 batch, got "
                            ++ show (length batches)
      | sch' /= sch ->
          failTest $ label ++ ": schema mismatch"
      | otherwise ->
          let !got = head batches
          in  if got == cols
                then putStrLn $ "OK: high-level round-trip " ++ label
                else failTest $ label
                                 ++ ": column mismatch\n got: "
                                 ++ show (V.toList got)
                                 ++ "\n exp: " ++ show (V.toList cols)

-- | Build a simple leaf field with no children.
plainField :: Text -> Bool -> ArrowType -> Field
plainField nm nullable ty = Field nm nullable ty V.empty Nothing

-- | Field with explicit children, no dictionary.
nestedField :: Text -> Bool -> ArrowType -> V.Vector Field -> Field
nestedField nm nullable ty children = Field nm nullable ty children Nothing

-- | Round-trip a pre-built Field/ColumnArray pair.
roundTripNested :: String -> Field -> ColumnArray -> IO ()
roundTripNested label field col = do
  let !schema = Schema
        { arrowEndianness = Little
        , arrowFields = V.singleton field
        }
      !stream = writeArrowStream schema (V.singleton (V.singleton col))
  case readArrowStream stream of
    Left e -> failTest (label ++ ": readArrowStream: " ++ e)
    Right as -> do
      expect (label ++ ": batch count == 1")
        (V.length (asBatches as) == 1)
      let (rb, body) = V.unsafeIndex (asBatches as) 0
      case materializeRecordBatch (asSchema as) rb body of
        Left e -> failTest (label ++ ": materialize: " ++ e)
        Right cols -> do
          expect (label ++ ": column count == 1") (V.length cols == 1)
          let !got = V.unsafeIndex cols 0
          when (got /= col) $
            failTest (label ++ ": got " ++ show got
                             ++ ", expected " ++ show col)
      expect (label ++ ": nested round-trip preserves column") True

-- | Round-trip a single flat (non-nested) column through a single-field
-- single-batch Arrow stream.
roundTripPrim :: String -> ColumnArray -> IO ()
roundTripPrim label col = do
  let !ty       = inferArrowType col
      !nullable = isNullable col
      !schema   = Schema
        { arrowEndianness = Little
        , arrowFields = V.singleton Field
            { fieldName = T.pack label
            , fieldNullable = nullable
            , fieldType = ty
            , fieldChildren = V.empty
            , fieldDictionary = Nothing
            }
        }
      !stream = writeArrowStream schema (V.singleton (V.singleton col))
  case readArrowStream stream of
    Left e -> failTest (label ++ ": readArrowStream: " ++ e)
    Right as -> do
      expect (label ++ ": schema endianness")
        (arrowEndianness (asSchema as) == Little)
      expect (label ++ ": batch count == 1")
        (V.length (asBatches as) == 1)
      let (rb, body) = V.unsafeIndex (asBatches as) 0
      case materializeRecordBatch (asSchema as) rb body of
        Left e -> failTest (label ++ ": materialize: " ++ e)
        Right cols -> do
          expect (label ++ ": column count == 1") (V.length cols == 1)
          let !got = V.unsafeIndex cols 0
          expect (label ++ ": row count matches")
            (columnLength got == columnLength col)
          when (got /= col) $
            failTest (label ++ ": got " ++ show got ++ ", expected " ++ show col)
      expect (label ++ ": round-trip preserves column") True

-- | Derive the appropriate 'ArrowType' for a 'ColumnArray' variant
-- the test driver feeds in. Used only to build per-test schemas.
inferArrowType :: ColumnArray -> ArrowType
inferArrowType = \case
  ColInt8 _  -> AInt 8  True
  ColInt16 _ -> AInt 16 True
  ColInt32 _ -> AInt 32 True
  ColInt64 _ -> AInt 64 True
  ColUInt8 _  -> AInt 8  False
  ColUInt16 _ -> AInt 16 False
  ColUInt32 _ -> AInt 32 False
  ColUInt64 _ -> AInt 64 False
  ColFloat16 _ -> AFloatingPoint Half
  ColFloat _   -> AFloatingPoint Single
  ColDouble _  -> AFloatingPoint DoublePrecision
  ColBool _    -> ABool
  ColUtf8 _    -> AUtf8
  ColBinary _  -> ABinary
  ColLargeUtf8 _   -> ALargeUtf8
  ColLargeBinary _ -> ALargeBinary
  ColFixedSizeBinary w _ -> AFixedSizeBinary w
  ColDate32 _ -> ADate DateDay
  ColDate64 _ -> ADate DateMillisecond
  ColTime32 _ -> ATime Second 32
  ColTime64 _ -> ATime Microsecond 64
  ColTimestamp _ -> ATimestamp Nanosecond Nothing
  ColDuration _ -> ADuration Nanosecond
  ColDecimal128 p s _ -> ADecimal p s
  ColDecimal256 p s _ -> ADecimal256 p s
  ColIntervalYearMonth _ -> AInterval YearMonth
  ColIntervalDayTime _ _ -> AInterval DayTime
  ColIntervalMonthDayNano _ _ _ -> AInterval MonthDayNano

  ColInt8Maybe _  -> AInt 8  True
  ColInt16Maybe _ -> AInt 16 True
  ColInt32Maybe _ -> AInt 32 True
  ColInt64Maybe _ -> AInt 64 True
  ColUInt8Maybe _  -> AInt 8  False
  ColUInt16Maybe _ -> AInt 16 False
  ColUInt32Maybe _ -> AInt 32 False
  ColUInt64Maybe _ -> AInt 64 False
  ColFloat16Maybe _ -> AFloatingPoint Half
  ColFloatMaybe _   -> AFloatingPoint Single
  ColDoubleMaybe _  -> AFloatingPoint DoublePrecision
  ColBoolMaybe _    -> ABool
  ColUtf8Maybe _    -> AUtf8
  ColBinaryMaybe _  -> ABinary
  ColLargeUtf8Maybe _   -> ALargeUtf8
  ColLargeBinaryMaybe _ -> ALargeBinary
  ColFixedSizeBinaryMaybe w _ -> AFixedSizeBinary w
  ColDate32Maybe _ -> ADate DateDay
  ColDate64Maybe _ -> ADate DateMillisecond
  ColTime32Maybe _ -> ATime Second 32
  ColTime64Maybe _ -> ATime Microsecond 64
  ColTimestampMaybe _ -> ATimestamp Nanosecond Nothing
  ColDurationMaybe _ -> ADuration Nanosecond

  -- The test driver doesn't invoke inferArrowType for nested columns;
  -- those are fed through a dedicated roundTripNested helper below.
  other -> error ("inferArrowType: unsupported: " ++ show other)

isNullable :: ColumnArray -> Bool
isNullable = \case
  ColInt8Maybe _     -> True
  ColInt16Maybe _    -> True
  ColInt32Maybe _    -> True
  ColInt64Maybe _    -> True
  ColUInt8Maybe _    -> True
  ColUInt16Maybe _   -> True
  ColUInt32Maybe _   -> True
  ColUInt64Maybe _   -> True
  ColFloat16Maybe _  -> True
  ColFloatMaybe _    -> True
  ColDoubleMaybe _   -> True
  ColBoolMaybe _     -> True
  ColUtf8Maybe _     -> True
  ColBinaryMaybe _   -> True
  ColLargeUtf8Maybe _   -> True
  ColLargeBinaryMaybe _ -> True
  ColFixedSizeBinaryMaybe _ _ -> True
  ColDate32Maybe _   -> True
  ColDate64Maybe _   -> True
  ColTime32Maybe _   -> True
  ColTime64Maybe _   -> True
  ColTimestampMaybe _ -> True
  ColDurationMaybe _ -> True
  _                  -> False

expect :: String -> Bool -> IO ()
expect label True  = putStrLn ("OK: " ++ label)
expect label False = failTest ("FAIL: " ++ label)

failTest :: String -> IO ()
failTest msg = do
  putStrLn msg
  exitFailure
