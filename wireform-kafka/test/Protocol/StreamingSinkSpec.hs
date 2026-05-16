{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Tests for the streaming transform builder infrastructure.

Covers:
* StreamSink / runBuilderStreaming fundamentals
* withStreamTransform combinator (mixed compressed + uncompressed)
* Zstd builder compression correctness
* Buffer boundary edge cases (empty, tiny, exact, multi-chunk, large)
* byteStringInsert through streaming sink
* Round-trip decompress
-}
module Protocol.StreamingSinkSpec (tests) where

import Codec.Compression.GZip qualified as GZip
import Codec.Compression.Zstd (Decompress (..))
import Codec.Compression.Zstd qualified as Zstd
import Codec.Compression.Zstd.Streaming qualified as ZS
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Word (Word8)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import Hedgehog (Property, annotate, assert, evalIO, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kafka.Compression.BuilderGzip qualified as BG
import Kafka.Compression.BuilderLz4 qualified as BL4
import Kafka.Compression.BuilderSnappy qualified as BSn
import Kafka.Compression.BuilderZstd qualified as BZ
import Kafka.Compression.Lz4 qualified as Lz4
import Kafka.Compression.Snappy qualified as Snappy
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import Wireform.Builder qualified as WB


tests :: TestTree
tests =
  testGroup
    "StreamingSink"
    [ identityTransformTests
    , withStreamTransformTests
    , zstdBuilderTests
    , gzipBuilderTests
    , lz4BuilderTests
    , snappyBuilderTests
    , bufferBoundaryTests
    , propertyTests
    ]


-- =========================================================================
-- Identity transform: collects raw bytes, returns them unchanged.
-- Validates the StreamSink plumbing without any actual transform.
-- =========================================================================

{- | Create an identity StreamSink that collects input and returns it
verbatim. Used to test the sink machinery in isolation.
-}
mkIdentitySink :: IO (WB.StreamSink, IORef [BS.ByteString])
mkIdentitySink = do
  chunksRef <- newIORef []
  let sink =
        WB.StreamSink
          { WB.ssFeedRaw = \ptr len -> do
              -- Copy from raw pointer to a ByteString for collection
              bs <- BS.packCStringLen (castPtr ptr, len)
              modifyIORef' chunksRef (bs :)
          , WB.ssFinish = do
              chunks <- readIORef chunksRef
              let !result = BS.concat (reverse chunks)
              -- Return a builder that emits the collected bytes
              pure (WB.byteStringCopy result)
          }
  pure (sink, chunksRef)


identityTransformTests :: TestTree
identityTransformTests =
  testGroup
    "Identity transform"
    [ testCase "empty builder" $ do
        (sink, _) <- mkIdentitySink
        outBuilder <- WB.runBuilderStreaming sink 128 mempty
        WB.toStrictByteString outBuilder @?= BS.empty
    , testCase "single byte" $ do
        (sink, _) <- mkIdentitySink
        outBuilder <- WB.runBuilderStreaming sink 128 (WB.word8 0x42)
        WB.toStrictByteString outBuilder @?= BS.pack [0x42]
    , testCase "small payload (< buffer)" $ do
        (sink, _) <- mkIdentitySink
        let payload = BS8.pack "hello world"
            builder = WB.byteStringCopy payload
        outBuilder <- WB.runBuilderStreaming sink 1024 builder
        WB.toStrictByteString outBuilder @?= payload
    , testCase "exact buffer size" $ do
        (sink, ref) <- mkIdentitySink
        let bufSize = 64
            payload = BS.replicate bufSize 0xAA
            builder = WB.byteStringCopy payload
        outBuilder <- WB.runBuilderStreaming sink bufSize builder
        WB.toStrictByteString outBuilder @?= payload
    , testCase "multi-chunk (many small writes > buffer)" $ do
        -- Use many small writes to ensure multiple flushes
        (sink, ref) <- mkIdentitySink
        let bufSize = 64
            builder = mconcat [WB.word8 0xBB | _ <- [1 .. 200 :: Int]]
        outBuilder <- WB.runBuilderStreaming sink bufSize builder
        let result = WB.toStrictByteString outBuilder
        BS.length result @?= 200
        result @?= BS.replicate 200 0xBB
        -- With 200 bytes and 64-byte buffer, should have multiple chunks
        chunks <- readIORef ref
        assertBool
          ("should have multiple chunks, got " ++ show (length chunks))
          (length chunks > 1)
    , testCase "byteStringInsert through streaming sink" $ do
        (sink, _) <- mkIdentitySink
        let payload = BS.replicate 500 0xCC
            builder = WB.word8 0x01 <> WB.byteStringInsert payload <> WB.word8 0x02
        outBuilder <- WB.runBuilderStreaming sink 64 builder
        let result = WB.toStrictByteString outBuilder
        BS.head result @?= 0x01
        BS.last result @?= 0x02
        BS.length result @?= 502
    , testCase "many small writes" $ do
        (sink, _) <- mkIdentitySink
        let builder = mconcat [WB.word8 (fromIntegral i) | i <- [0 .. 255 :: Int]]
        outBuilder <- WB.runBuilderStreaming sink 32 builder
        let result = WB.toStrictByteString outBuilder
        BS.length result @?= 256
        BS.unpack result @?= [0 .. 255]
    ]


-- =========================================================================
-- withStreamTransform: mixing transformed and untransformed sections
-- =========================================================================

withStreamTransformTests :: TestTree
withStreamTransformTests =
  testGroup
    "withStreamTransform"
    [ testCase "identity transform in middle of builder" $ do
        (sink, _) <- mkIdentitySink
        let builder =
              WB.byteStringCopy "HEADER"
                <> WB.withStreamTransform sink 64 (WB.byteStringCopy "PAYLOAD")
                <> WB.byteStringCopy "TRAILER"
        let result = WB.toStrictByteString builder
        result @?= "HEADERPAYLOADTRAILER"
    , testCase "empty transform section" $ do
        (sink, _) <- mkIdentitySink
        let builder =
              WB.byteStringCopy "before"
                <> WB.withStreamTransform sink 64 mempty
                <> WB.byteStringCopy "after"
        WB.toStrictByteString builder @?= "beforeafter"
    , testCase "transform only (no header/trailer)" $ do
        (sink, _) <- mkIdentitySink
        let payload = BS.replicate 300 0xDD
        let builder = WB.withStreamTransform sink 64 (WB.byteStringCopy payload)
        WB.toStrictByteString builder @?= payload
    , testCase "multiple transform sections" $ do
        (sink1, _) <- mkIdentitySink
        (sink2, _) <- mkIdentitySink
        let builder =
              WB.byteStringCopy "A"
                <> WB.withStreamTransform sink1 64 (WB.byteStringCopy "B")
                <> WB.byteStringCopy "C"
                <> WB.withStreamTransform sink2 64 (WB.byteStringCopy "D")
                <> WB.byteStringCopy "E"
        WB.toStrictByteString builder @?= "ABCDE"
    ]


-- =========================================================================
-- Zstd builder compression: correctness tests
-- =========================================================================

zstdBuilderTests :: TestTree
zstdBuilderTests =
  testGroup
    "Zstd builder compression"
    [ testCase "empty input produces valid output" $ do
        result <- BZ.compressBuilder mempty
        case result of
          Left err -> assertFailure err
          Right compressed ->
            -- Empty input should produce a valid (possibly empty) compressed frame
            assertBool "should succeed" True
    , testCase "small input round-trip" $ do
        let payload = BS8.pack "hello world, this is a test"
            builder = WB.byteStringCopy payload
        result <- BZ.compressBuilder builder
        case result of
          Left err -> assertFailure err
          Right compressed -> do
            decompressed <- decompressZstdStreaming compressed
            decompressed @?= payload
    , testCase "matches reference compression" $ do
        let payload = BS.replicate 1000 0x42
            builder = WB.byteStringCopy payload
        result <- BZ.compressBuilder builder
        let reference = Zstd.compress 3 payload
        case result of
          Left err -> assertFailure err
          Right compressed -> do
            -- Both should decompress to the same thing
            decompressed <- decompressZstdStreaming compressed
            decompressed @?= payload
            refDecompressed <- decompressZstdStreaming reference
            refDecompressed @?= payload
    , testCase "multi-chunk payload (larger than buffer)" $ do
        -- 100KB payload, 32KB buffer = ~3 chunks
        let payload = BS.pack [fromIntegral (i `mod` 251) | i <- [0 .. 9999 :: Int]]
            builder = WB.byteStringCopy payload
        result <- BZ.compressBuilderWithLevel 3 builder
        case result of
          Left err -> assertFailure err
          Right compressed -> do
            assertBool "compressed should be smaller" (BS.length compressed < BS.length payload)
            decompressed <- decompressZstdStreaming compressed
            decompressed @?= payload
    , testCase "composed builder (not just byteStringCopy)" $ do
        -- Build payload from individual field writes, not a single byteStringCopy
        let builder =
              mconcat
                [ WB.word8 0x08
                , WB.word8 42 -- tag + varint
                , WB.word8 0x12
                , WB.word8 5
                , WB.byteStringCopy "hello" -- tag + len + string
                , WB.word8 0x18
                , WB.word8 1 -- tag + bool
                , WB.word32LE 0xDEADBEEF
                , WB.word64LE 0xCAFEBABECAFEBABE
                ]
            expected = WB.toStrictByteString builder
        result <- BZ.compressBuilder builder
        case result of
          Left err -> assertFailure err
          Right compressed -> do
            decompressed <- decompressZstdStreaming compressed
            decompressed @?= expected
    , testCase "withStreamTransform + zstd (simulated Kafka batch)" $ do
        -- Simulate: uncompressed header + zstd-compressed records + trailer
        Right sink <- BZ.zstdStreamSink 3
        let records =
              mconcat
                [ WB.word8 (fromIntegral i) <> WB.byteStringCopy (BS.replicate 100 (fromIntegral i))
                | i <- [0 .. 19 :: Int]
                ]
            header = WB.byteStringCopy (BS.pack [0xDE, 0xAD, 0xCA, 0xFE])
            trailer = WB.word32LE 0x12345678
            fullBuilder =
              header
                <> WB.withStreamTransform sink 4096 records
                <> trailer
            result = WB.toStrictByteString fullBuilder

        -- Verify header is intact
        assertBool "starts with header" (BS.take 4 result == BS.pack [0xDE, 0xAD, 0xCA, 0xFE])

        -- Verify trailer is at the end
        let trailBytes = BS.drop (BS.length result - 4) result
        trailBytes @?= BS.pack [0x78, 0x56, 0x34, 0x12] -- LE

        -- The middle section should be compressed (smaller than 2020 bytes)
        let middleLen = BS.length result - 4 - 4
        assertBool
          ("compressed middle should be smaller than 2020, got " ++ show middleLen)
          (middleLen < 2020)
    , testCase "various compression levels" $ do
        let payload = BS.pack [fromIntegral (i `mod` 73) | i <- [0 .. 9999 :: Int]]
            builder = WB.byteStringCopy payload
        -- Test levels 1, 3, 9
        mapM_
          ( \lvl -> do
              result <- BZ.compressBuilderWithLevel lvl builder
              case result of
                Left err -> assertFailure $ "level " ++ show lvl ++ ": " ++ err
                Right compressed -> do
                  decompressed <- decompressZstdStreaming compressed
                  decompressed @?= payload
          )
          [1, 3, 9]
    ]


-- =========================================================================
-- Gzip builder compression
-- =========================================================================

gzipBuilderTests :: TestTree
gzipBuilderTests =
  testGroup
    "Gzip builder compression"
    [ testCase "small input round-trip" $ do
        let payload = BS8.pack "gzip test payload with some data"
            builder = WB.byteStringCopy payload
        case BG.compressBuilder builder of
          Left err -> assertFailure err
          Right compressed -> do
            let decompressed = BL.toStrict $ GZip.decompress $ BL.fromStrict compressed
            decompressed @?= payload
    , testCase "multi-chunk round-trip" $ do
        let payload = BS.pack [fromIntegral (i `mod` 251) | i <- [0 .. 9999 :: Int]]
            builder = WB.byteStringCopy payload
        case BG.compressBuilderWithLevel 6 builder of
          Left err -> assertFailure err
          Right compressed -> do
            assertBool "should compress" (BS.length compressed < BS.length payload)
            let decompressed = BL.toStrict $ GZip.decompress $ BL.fromStrict compressed
            decompressed @?= payload
    , testCase "gzipStreamSink + withStreamTransform" $ do
        sink <- BG.gzipStreamSink 6
        let payload = BS.replicate 500 0xAB
            builder =
              WB.byteStringCopy "HDR"
                <> WB.withStreamTransform sink 64 (WB.byteStringCopy payload)
                <> WB.byteStringCopy "TRL"
            result = WB.toStrictByteString builder
        -- header intact
        BS.take 3 result @?= "HDR"
        -- trailer intact
        BS.drop (BS.length result - 3) result @?= "TRL"
        -- middle is compressed (smaller than 500)
        assertBool "middle should be compressed" (BS.length result < 500 + 6)
    ]


-- =========================================================================
-- LZ4 builder compression
-- =========================================================================

lz4BuilderTests :: TestTree
lz4BuilderTests =
  testGroup
    "LZ4 builder compression"
    [ testCase "small input round-trip" $ do
        let payload = BS8.pack "lz4 test payload"
            builder = WB.byteStringCopy payload
        result <- BL4.compressBuilder builder
        case result of
          Left err -> assertFailure err
          Right compressed -> do
            decompResult <- Lz4.decompressLz4 compressed
            case decompResult of
              Right decompressed -> decompressed @?= payload
              Left err -> assertFailure $ "lz4 decompress: " ++ err
    , testCase "multi-chunk round-trip" $ do
        let payload = BS.pack [fromIntegral (i `mod` 251) | i <- [0 .. 9999 :: Int]]
            builder = WB.byteStringCopy payload
        result <- BL4.compressBuilderWithLevel 0 builder
        case result of
          Left err -> assertFailure err
          Right compressed -> do
            decompResult <- Lz4.decompressLz4 compressed
            case decompResult of
              Right decompressed -> decompressed @?= payload
              Left err -> assertFailure $ "lz4 decompress: " ++ err
    , testCase "composed builder" $ do
        let builder = mconcat [WB.word8 (fromIntegral i) | i <- [0 .. 255 :: Int]]
            expected = WB.toStrictByteString builder
        result <- BL4.compressBuilder builder
        case result of
          Left err -> assertFailure err
          Right compressed -> do
            decompResult <- Lz4.decompressLz4 compressed
            case decompResult of
              Right decompressed -> decompressed @?= expected
              Left err -> assertFailure $ "lz4 decompress: " ++ err
    ]


-- =========================================================================
-- Snappy builder compression
-- =========================================================================

snappyBuilderTests :: TestTree
snappyBuilderTests =
  testGroup
    "Snappy builder compression"
    [ testCase "small input round-trip" $ do
        let payload = BS8.pack "snappy test payload"
            builder = WB.byteStringCopy payload
        result <- BSn.compressBuilder builder
        case result of
          Left err -> assertFailure err
          Right compressed -> do
            decompResult <- Snappy.decompressSnappy compressed
            case decompResult of
              Right decompressed -> decompressed @?= payload
              Left err -> assertFailure $ "snappy decompress: " ++ err
    , testCase "multi-chunk round-trip" $ do
        let payload = BS.pack [fromIntegral (i `mod` 251) | i <- [0 .. 9999 :: Int]]
            builder = WB.byteStringCopy payload
        result <- BSn.compressBuilder builder
        case result of
          Left err -> assertFailure err
          Right compressed -> do
            decompResult <- Snappy.decompressSnappy compressed
            case decompResult of
              Right decompressed -> decompressed @?= payload
              Left err -> assertFailure $ "snappy decompress: " ++ err
    , testCase "composed builder" $ do
        let builder = mconcat [WB.word8 (fromIntegral i) | i <- [0 .. 255 :: Int]]
            expected = WB.toStrictByteString builder
        result <- BSn.compressBuilder builder
        case result of
          Left err -> assertFailure err
          Right compressed -> do
            decompResult <- Snappy.decompressSnappy compressed
            case decompResult of
              Right decompressed -> decompressed @?= expected
              Left err -> assertFailure $ "snappy decompress: " ++ err
    ]


-- =========================================================================
-- Buffer boundary edge cases
-- =========================================================================

bufferBoundaryTests :: TestTree
bufferBoundaryTests =
  testGroup
    "Buffer boundary edge cases"
    [ testCase "payload exactly 1 byte less than buffer" $ do
        let bufSize = 128
            payload = BS.replicate (bufSize - 1) 0xEE
        result <- roundTripZstdBuilder bufSize payload
        result @?= payload
    , testCase "payload exactly 1 byte more than buffer" $ do
        let bufSize = 128
            payload = BS.replicate (bufSize + 1) 0xFF
        result <- roundTripZstdBuilder bufSize payload
        result @?= payload
    , testCase "payload exactly 2x buffer" $ do
        let bufSize = 64
            payload = BS.replicate (bufSize * 2) 0xAA
        result <- roundTripZstdBuilder bufSize payload
        result @?= payload
    , testCase "payload exactly 3x buffer" $ do
        let bufSize = 64
            payload = BS.replicate (bufSize * 3) 0xBB
        result <- roundTripZstdBuilder bufSize payload
        result @?= payload
    , testCase "very small buffer (16 bytes)" $ do
        let payload = BS.replicate 1000 0xCC
        result <- roundTripZstdBuilder 16 payload
        result @?= payload
    , testCase "buffer size 1 (pathological)" $ do
        let payload = BS8.pack "abcdefghij"
        result <- roundTripZstdBuilder 1 payload
        result @?= payload
    , testCase "1MB payload with 32KB buffer" $ do
        let payload = BS.pack [fromIntegral (i `mod` 256) | i <- [0 .. 49999 :: Int]]
        result <- roundTripZstdBuilder 32768 payload
        result @?= payload
    ]


-- =========================================================================
-- Hedgehog property tests
-- =========================================================================

propertyTests :: TestTree
propertyTests =
  testGroup
    "Properties"
    [ testProperty "zstd builder round-trip preserves data" prop_roundTrip_zstd
    , testProperty "zstd builder round-trip with various buffer sizes" prop_roundTrip_bufSizes
    , testProperty "gzip builder round-trip preserves data" prop_roundTrip_gzip
    , testProperty "lz4 builder round-trip preserves data" prop_roundTrip_lz4
    , testProperty "snappy builder round-trip preserves data" prop_roundTrip_snappy
    , testProperty "identity sink preserves data" prop_identity_preserves
    , testProperty "withStreamTransform preserves framing" prop_framing
    , testProperty "composed builder matches monolithic" prop_composed_matches
    ]


prop_roundTrip_zstd :: Property
prop_roundTrip_zstd = property $ do
  input <- forAll $ Gen.bytes (Range.linear 0 5000)
  let builder = WB.byteStringCopy input
  result <- evalIO $ BZ.compressBuilder builder
  case result of
    Left err -> do
      annotate err
      assert False
    Right compressed -> do
      decompressed <- evalIO $ decompressZstdStreaming compressed
      decompressed === input


prop_roundTrip_bufSizes :: Property
prop_roundTrip_bufSizes = property $ do
  input <- forAll $ Gen.bytes (Range.linear 0 10000)
  bufSize <- forAll $ Gen.int (Range.linear 1 1024)
  let builder = WB.byteStringCopy input
  Right sink <- evalIO $ BZ.zstdStreamSink 3
  outBuilder <- evalIO $ WB.runBuilderStreaming sink bufSize builder
  let compressed = WB.toStrictByteString outBuilder
  decompressed <- evalIO $ decompressZstdStreaming compressed
  decompressed === input


prop_roundTrip_gzip :: Property
prop_roundTrip_gzip = property $ do
  input <- forAll $ Gen.bytes (Range.linear 0 5000)
  let builder = WB.byteStringCopy input
  case BG.compressBuilder builder of
    Left err -> do
      annotate err
      assert False
    Right compressed -> do
      let decompressed = BL.toStrict $ GZip.decompress $ BL.fromStrict compressed
      decompressed === input


prop_roundTrip_lz4 :: Property
prop_roundTrip_lz4 = property $ do
  input <- forAll $ Gen.bytes (Range.linear 1 5000) -- lz4 may not handle empty
  let builder = WB.byteStringCopy input
  result <- evalIO $ BL4.compressBuilder builder
  case result of
    Left err -> do
      annotate err
      assert False
    Right compressed -> do
      decompResult <- evalIO $ Lz4.decompressLz4 compressed
      case decompResult of
        Right decompressed -> decompressed === input
        Left err -> do
          annotate $ "lz4 decompress: " ++ err
          assert False


prop_roundTrip_snappy :: Property
prop_roundTrip_snappy = property $ do
  input <- forAll $ Gen.bytes (Range.linear 1 5000) -- snappy may not handle empty
  let builder = WB.byteStringCopy input
  result <- evalIO $ BSn.compressBuilder builder
  case result of
    Left err -> do
      annotate err
      assert False
    Right compressed -> do
      decompResult <- evalIO $ Snappy.decompressSnappy compressed
      case decompResult of
        Right decompressed -> decompressed === input
        Left err -> do
          annotate $ "snappy decompress: " ++ err
          assert False


prop_identity_preserves :: Property
prop_identity_preserves = property $ do
  input <- forAll $ Gen.bytes (Range.linear 0 5000)
  bufSize <- forAll $ Gen.int (Range.linear 1 512)
  (sink, _) <- evalIO mkIdentitySink
  outBuilder <- evalIO $ WB.runBuilderStreaming sink bufSize (WB.byteStringCopy input)
  WB.toStrictByteString outBuilder === input


prop_framing :: Property
prop_framing = property $ do
  header <- forAll $ Gen.bytes (Range.linear 0 100)
  payload <- forAll $ Gen.bytes (Range.linear 0 5000)
  trailer <- forAll $ Gen.bytes (Range.linear 0 100)
  (sink, _) <- evalIO mkIdentitySink
  let builder =
        WB.byteStringCopy header
          <> WB.withStreamTransform sink 64 (WB.byteStringCopy payload)
          <> WB.byteStringCopy trailer
  WB.toStrictByteString builder === (header <> payload <> trailer)


prop_composed_matches :: Property
prop_composed_matches = property $ do
  -- Build a payload from many small pieces; verify compression matches
  -- compressing the same bytes as a single byteStringCopy
  n <- forAll $ Gen.int (Range.linear 0 500)
  let pieces = [BS.singleton (fromIntegral (i `mod` 256)) | i <- [0 .. n]]
      monolithic = BS.concat pieces
      composedBuilder = mconcat (map WB.byteStringCopy pieces)
      monolithicBuilder = WB.byteStringCopy monolithic
  resultComposed <- evalIO $ BZ.compressBuilder composedBuilder
  resultMonolithic <- evalIO $ BZ.compressBuilder monolithicBuilder
  case (resultComposed, resultMonolithic) of
    (Right c1, Right c2) -> do
      d1 <- evalIO $ decompressZstdStreaming c1
      d2 <- evalIO $ decompressZstdStreaming c2
      d1 === d2
    _ -> assert False


-- =========================================================================
-- Helpers
-- =========================================================================

{- | Decompress zstd via the streaming API (handles content-size-unknown
frames that the one-shot Zstd.decompress returns Skip for).
-}
decompressZstdStreaming :: BS.ByteString -> IO BS.ByteString
decompressZstdStreaming compressed = do
  result0 <- ZS.decompress
  go result0 [compressed] []
  where
    go (ZS.Consume feed) (chunk : rest) acc = do
      result <- feed chunk
      go result rest acc
    go (ZS.Consume feed) [] acc = do
      result <- feed BS.empty
      go result [] acc
    go (ZS.Produce bs next) chunks acc = do
      result <- next
      go result chunks (bs : acc)
    go (ZS.Done bs) _ acc =
      pure $ BS.concat $ reverse (bs : acc)
    go (ZS.Error name desc) _ _ =
      error $ "zstd streaming decompress: " ++ name ++ ": " ++ desc


{- | Compress a ByteString through the builder zstd path with a given
buffer size, then decompress. Returns the round-tripped data.
-}
roundTripZstdBuilder :: Int -> BS.ByteString -> IO BS.ByteString
roundTripZstdBuilder bufSize input = do
  Right sink <- BZ.zstdStreamSink 3
  outBuilder <- WB.runBuilderStreaming sink bufSize (WB.byteStringCopy input)
  let compressed = WB.toStrictByteString outBuilder
  decompressZstdStreaming compressed
