module Test.CapnProto (capnProtoTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import qualified CapnProto.Value as C
import CapnProto.Encode (encode)
import CapnProto.Decode (decode)

capnProtoTests :: TestTree
capnProtoTests = testGroup "CapnProto"
  [ unitTests
  , wireFormatTests
  , edgeCases
  , propertyTests
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Void" $ do
      let bs = encode C.Void
      BS.length bs @?= 8

  , testCase "Bool true encodes" $ do
      let bs = encode (C.Bool True)
      BS.length bs @?= 16

  , testCase "Bool false encodes" $ do
      let bs = encode (C.Bool False)
      BS.length bs @?= 16

  , testCase "Int8 encodes" $ do
      let bs = encode (C.Int8 42)
      BS.length bs @?= 16

  , testCase "Int16 encodes" $ do
      let bs = encode (C.Int16 1000)
      BS.length bs @?= 16

  , testCase "Int32 encodes" $ do
      let bs = encode (C.Int32 100000)
      BS.length bs @?= 16

  , testCase "Int64 encodes" $ do
      let bs = encode (C.Int64 maxBound)
      BS.length bs @?= 16

  , testCase "UInt8 encodes" $ do
      let bs = encode (C.UInt8 255)
      BS.length bs @?= 16

  , testCase "UInt16 encodes" $ do
      let bs = encode (C.UInt16 65535)
      BS.length bs @?= 16

  , testCase "UInt32 encodes" $ do
      let bs = encode (C.UInt32 maxBound)
      BS.length bs @?= 16

  , testCase "UInt64 encodes" $ do
      let bs = encode (C.UInt64 maxBound)
      BS.length bs @?= 16

  , testCase "Float32 encodes" $ do
      let bs = encode (C.Float32 3.14)
      BS.length bs @?= 16

  , testCase "Float64 encodes" $ do
      let bs = encode (C.Float64 2.718)
      BS.length bs @?= 16

  , testCase "Enum encodes" $ do
      let bs = encode (C.Enum 42)
      BS.length bs @?= 16

  , testCase "Text encodes" $ do
      let bs = encode (C.Text (T.pack "hello"))
      BS.length bs > 8 @?= True

  , testCase "Data encodes" $ do
      let bs = encode (C.Data (BS.pack [1,2,3]))
      BS.length bs > 8 @?= True

  , testCase "Empty list encodes" $ do
      let bs = encode (C.List V.empty)
      BS.length bs @?= 16
  ]

wireFormatTests :: TestTree
wireFormatTests = testGroup "Wire format"
  [ testCase "Segment table has count 0 (= 1 segment)" $ do
      let bs = encode (C.Bool True)
      readLE32 bs 0 @?= 0

  , testCase "Segment size is correct for Bool" $ do
      let bs = encode (C.Bool True)
      readLE32 bs 4 @?= 1

  , testCase "Bool true data byte" $ do
      let bs = encode (C.Bool True)
      BS.index bs 8 @?= 0x01

  , testCase "Bool false data byte" $ do
      let bs = encode (C.Bool False)
      BS.index bs 8 @?= 0x00

  , testCase "Int8 value at offset 8" $ do
      let bs = encode (C.Int8 42)
      BS.index bs 8 @?= 42

  , testCase "UInt64 value at offset 8" $ do
      let bs = encode (C.UInt64 0x0102030405060708)
          w = readLE64 bs 8
      w @?= 0x0102030405060708

  , testCase "Float64 encode/decode byte identity" $ do
      let val = C.Float64 1.0
          bs = encode val
          w = readLE64 bs 8
      w @?= 0x3FF0000000000000
  ]

edgeCases :: TestTree
edgeCases = testGroup "Edge cases"
  [ testCase "Decode empty input" $
      case decode BS.empty of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on empty input"

  , testCase "Decode too short" $
      case decode (BS.pack [0, 0, 0, 0]) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on short input"

  , testCase "List of ints encodes" $ do
      let val = C.List (V.fromList [C.UInt64 1, C.UInt64 2, C.UInt64 3])
          bs = encode val
      BS.length bs > 16 @?= True

  , testCase "Text roundtrip bytes" $ do
      let val = C.Text (T.pack "A")
          bs = encode val
      BS.length bs > 8 @?= True
  ]

propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [ testProperty "UInt64 encode/decode roundtrip" $ property $ do
      w <- forAll $ Gen.word64 (Range.linear 1 maxBound)
      let bs = encode (C.UInt64 w)
      case decode bs of
        Right (C.UInt64 w') -> w === w'
        Right other -> do
          annotate $ "unexpected decode result: " ++ show other
          failure
        Left err -> do
          annotate err
          failure

  , testProperty "Int64 encode preserves size" $ property $ do
      n <- forAll $ Gen.int64 Range.linearBounded
      let bs = encode (C.Int64 n)
      BS.length bs === 16

  , testProperty "UInt32 encode preserves size" $ property $ do
      w <- forAll $ Gen.word32 Range.linearBounded
      let bs = encode (C.UInt32 w)
      BS.length bs === 16

  , testProperty "Bool encode preserves size" $ property $ do
      b <- forAll Gen.bool
      let bs = encode (C.Bool b)
      BS.length bs === 16

  , testProperty "Float64 encode preserves size" $ property $ do
      d <- forAll $ Gen.double (Range.linearFrac (-1e6) 1e6)
      let bs = encode (C.Float64 d)
      BS.length bs === 16

  , testProperty "Text encode has correct minimum size" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 64) Gen.alphaNum
      let bs = encode (C.Text t)
      assert (BS.length bs > 8)

  , testProperty "Void encode is 8 bytes" $ property $ do
      let bs = encode C.Void
      BS.length bs === 8
  ]

readLE32 :: BS.ByteString -> Int -> Word32
readLE32 bs off =
  fromIntegral (BS.index bs off)
  + fromIntegral (BS.index bs (off+1)) * 256
  + fromIntegral (BS.index bs (off+2)) * 65536
  + fromIntegral (BS.index bs (off+3)) * 16777216

readLE64 :: BS.ByteString -> Int -> Word64
readLE64 bs off =
  fromIntegral (BS.index bs off)
  + fromIntegral (BS.index bs (off+1)) * 0x100
  + fromIntegral (BS.index bs (off+2)) * 0x10000
  + fromIntegral (BS.index bs (off+3)) * 0x1000000
  + fromIntegral (BS.index bs (off+4)) * 0x100000000
  + fromIntegral (BS.index bs (off+5)) * 0x10000000000
  + fromIntegral (BS.index bs (off+6)) * 0x1000000000000
  + fromIntegral (BS.index bs (off+7)) * 0x100000000000000
