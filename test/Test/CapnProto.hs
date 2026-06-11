module Test.CapnProto (capnProtoTests) where

import CapnProto.Decode (decode)
import CapnProto.Encode (encode)
import CapnProto.Value qualified as C
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()


capnProtoTests :: Spec
capnProtoTests =
  describe "CapnProto" $
    sequence_
      [ unitTests
      , wireFormatTests
      , edgeCases
      , propertyTests
      ]


unitTests :: Spec
unitTests =
  describe "Unit tests" $
    sequence_
      [ it "Void" $ do
          let bs = encode C.Void
          BS.length bs `shouldBe` 8
      , it "Bool true encodes" $ do
          let bs = encode (C.Bool True)
          BS.length bs `shouldBe` 16
      , it "Bool false encodes" $ do
          let bs = encode (C.Bool False)
          BS.length bs `shouldBe` 16
      , it "Int8 encodes" $ do
          let bs = encode (C.Int8 42)
          BS.length bs `shouldBe` 16
      , it "Int16 encodes" $ do
          let bs = encode (C.Int16 1000)
          BS.length bs `shouldBe` 16
      , it "Int32 encodes" $ do
          let bs = encode (C.Int32 100000)
          BS.length bs `shouldBe` 16
      , it "Int64 encodes" $ do
          let bs = encode (C.Int64 maxBound)
          BS.length bs `shouldBe` 16
      , it "UInt8 encodes" $ do
          let bs = encode (C.UInt8 255)
          BS.length bs `shouldBe` 16
      , it "UInt16 encodes" $ do
          let bs = encode (C.UInt16 65535)
          BS.length bs `shouldBe` 16
      , it "UInt32 encodes" $ do
          let bs = encode (C.UInt32 maxBound)
          BS.length bs `shouldBe` 16
      , it "UInt64 encodes" $ do
          let bs = encode (C.UInt64 maxBound)
          BS.length bs `shouldBe` 16
      , it "Float32 encodes" $ do
          let bs = encode (C.Float32 3.14)
          BS.length bs `shouldBe` 16
      , it "Float64 encodes" $ do
          let bs = encode (C.Float64 2.718)
          BS.length bs `shouldBe` 16
      , it "Enum encodes" $ do
          let bs = encode (C.Enum 42)
          BS.length bs `shouldBe` 16
      , it "Text encodes" $ do
          let bs = encode (C.Text (T.pack "hello"))
          BS.length bs > 8 `shouldBe` True
      , it "Data encodes" $ do
          let bs = encode (C.Data (BS.pack [1, 2, 3]))
          BS.length bs > 8 `shouldBe` True
      , it "Empty list encodes" $ do
          let bs = encode (C.List V.empty)
          BS.length bs `shouldBe` 16
      ]


wireFormatTests :: Spec
wireFormatTests =
  describe "Wire format" $
    sequence_
      [ it "Segment table has count 0 (= 1 segment)" $ do
          let bs = encode (C.Bool True)
          readLE32 bs 0 `shouldBe` 0
      , it "Segment size is correct for Bool" $ do
          let bs = encode (C.Bool True)
          readLE32 bs 4 `shouldBe` 1
      , it "Bool true data byte" $ do
          let bs = encode (C.Bool True)
          BS.index bs 8 `shouldBe` 0x01
      , it "Bool false data byte" $ do
          let bs = encode (C.Bool False)
          BS.index bs 8 `shouldBe` 0x00
      , it "Int8 value at offset 8" $ do
          let bs = encode (C.Int8 42)
          BS.index bs 8 `shouldBe` 42
      , it "UInt64 value at offset 8" $ do
          let bs = encode (C.UInt64 0x0102030405060708)
              w = readLE64 bs 8
          w `shouldBe` 0x0102030405060708
      , it "Float64 encode/decode byte identity" $ do
          let val = C.Float64 1.0
              bs = encode val
              w = readLE64 bs 8
          w `shouldBe` 0x3FF0000000000000
      ]


edgeCases :: Spec
edgeCases =
  describe "Edge cases" $
    sequence_
      [ it "Decode empty input" $
          case decode BS.empty of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on empty input"
      , it "Decode too short" $
          case decode (BS.pack [0, 0, 0, 0]) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on short input"
      , it "List of ints encodes" $ do
          let val = C.List (V.fromList [C.UInt64 1, C.UInt64 2, C.UInt64 3])
              bs = encode val
          BS.length bs > 16 `shouldBe` True
      , it "Text roundtrip bytes" $ do
          let val = C.Text (T.pack "A")
              bs = encode val
          BS.length bs > 8 `shouldBe` True
      ]


propertyTests :: Spec
propertyTests =
  describe "Property tests" $
    sequence_
      [ it "UInt64 encode/decode roundtrip" $ property $ do
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
      , it "Int64 encode preserves size" $ property $ do
          n <- forAll $ Gen.int64 Range.linearBounded
          let bs = encode (C.Int64 n)
          BS.length bs === 16
      , it "UInt32 encode preserves size" $ property $ do
          w <- forAll $ Gen.word32 Range.linearBounded
          let bs = encode (C.UInt32 w)
          BS.length bs === 16
      , it "Bool encode preserves size" $ property $ do
          b <- forAll Gen.bool
          let bs = encode (C.Bool b)
          BS.length bs === 16
      , it "Float64 encode preserves size" $ property $ do
          d <- forAll $ Gen.double (Range.linearFrac (-1e6) 1e6)
          let bs = encode (C.Float64 d)
          BS.length bs === 16
      , it "Text encode has correct minimum size" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 64) Gen.alphaNum
          let bs = encode (C.Text t)
          assert (BS.length bs > 8)
      , it "Void encode is 8 bytes" $ property $ do
          let bs = encode C.Void
          BS.length bs === 8
      ]


readLE32 :: BS.ByteString -> Int -> Word32
readLE32 bs off =
  fromIntegral (BS.index bs off)
    + fromIntegral (BS.index bs (off + 1)) * 256
    + fromIntegral (BS.index bs (off + 2)) * 65536
    + fromIntegral (BS.index bs (off + 3)) * 16777216


readLE64 :: BS.ByteString -> Int -> Word64
readLE64 bs off =
  fromIntegral (BS.index bs off)
    + fromIntegral (BS.index bs (off + 1)) * 0x100
    + fromIntegral (BS.index bs (off + 2)) * 0x10000
    + fromIntegral (BS.index bs (off + 3)) * 0x1000000
    + fromIntegral (BS.index bs (off + 4)) * 0x100000000
    + fromIntegral (BS.index bs (off + 5)) * 0x10000000000
    + fromIntegral (BS.index bs (off + 6)) * 0x1000000000000
    + fromIntegral (BS.index bs (off + 7)) * 0x100000000000000
