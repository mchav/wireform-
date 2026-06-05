module Test.Bond (bondTests) where

import Data.Bits (shiftL, shiftR, xor, (.&.))
import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word64)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import Bond.Value
import Bond.Encode (encode)
import Bond.Decode (decode)

bondTests :: Spec
bondTests = describe "Bond" $ sequence_
  [ primitiveRoundtrips
  , structTests
  , nestedStructTests
  , containerTests
  , zigzagTests
  , propertyRoundtrips
  ]

-- ============================================================
-- Primitive roundtrips
-- ============================================================

primitiveRoundtrips :: Spec
primitiveRoundtrips = describe "Primitive roundtrips" $ sequence_
  [ it "Bool true" $ roundtrip BT_BOOL (Bool True)
  , it "Bool false" $ roundtrip BT_BOOL (Bool False)
  , it "Int8 positive" $ roundtrip BT_INT8 (Int8 42)
  , it "Int8 negative" $ roundtrip BT_INT8 (Int8 (-1))
  , it "Int8 zero" $ roundtrip BT_INT8 (Int8 0)
  , it "Int16" $ roundtrip BT_INT16 (Int16 1000)
  , it "Int16 negative" $ roundtrip BT_INT16 (Int16 (-1000))
  , it "Int32" $ roundtrip BT_INT32 (Int32 100000)
  , it "Int32 negative" $ roundtrip BT_INT32 (Int32 (-100000))
  , it "Int64" $ roundtrip BT_INT64 (Int64 9999999999)
  , it "Int64 negative" $ roundtrip BT_INT64 (Int64 (-9999999999))
  , it "UInt8" $ roundtrip BT_UINT8 (UInt8 255)
  , it "UInt16" $ roundtrip BT_UINT16 (UInt16 65535)
  , it "UInt32" $ roundtrip BT_UINT32 (UInt32 4000000000)
  , it "UInt64" $ roundtrip BT_UINT64 (UInt64 18000000000000000000)
  , it "Float" $ roundtrip BT_FLOAT (Float 3.14)
  , it "Double" $ roundtrip BT_DOUBLE (Double 2.718281828)
  , it "String empty" $ roundtrip BT_STRING (String T.empty)
  , it "String hello" $ roundtrip BT_STRING (String (T.pack "hello world"))
  , it "WString" $ roundtrip BT_WSTRING (WString (T.pack "wide string"))
  , it "Enum encodes as Int32" $ do
      let bs = encode (Enum 42)
      decode BT_INT32 bs `shouldBe` Right (Int32 42)
  ]

-- ============================================================
-- Struct tests
-- ============================================================

structTests :: Spec
structTests = describe "Struct encode/decode" $ sequence_
  [ it "Empty struct" $ roundtrip BT_STRUCT (Struct V.empty V.empty)

  , it "Struct with one field" $ do
      let val = Struct V.empty (V.singleton (1, BT_INT32, Int32 42))
      roundtrip BT_STRUCT val

  , it "Struct with multiple fields" $ do
      let val = Struct V.empty (V.fromList
            [ (1, BT_BOOL, Bool True)
            , (2, BT_INT32, Int32 100)
            , (3, BT_STRING, String (T.pack "test"))
            , (4, BT_DOUBLE, Double 1.5)
            ])
      roundtrip BT_STRUCT val

  , it "Struct with non-sequential field ids" $ do
      let val = Struct V.empty (V.fromList
            [ (1, BT_INT32, Int32 10)
            , (10, BT_INT32, Int32 20)
            , (100, BT_STRING, String (T.pack "far apart"))
            ])
      roundtrip BT_STRUCT val
  ]

-- ============================================================
-- Nested structs
-- ============================================================

nestedStructTests :: Spec
nestedStructTests = describe "Nested structs" $ sequence_
  [ it "Struct containing struct" $ do
      let inner = Struct V.empty (V.fromList
            [ (1, BT_INT32, Int32 1)
            , (2, BT_STRING, String (T.pack "inner"))
            ])
          outer = Struct V.empty (V.fromList
            [ (1, BT_STRUCT, inner)
            , (2, BT_BOOL, Bool True)
            ])
      roundtrip BT_STRUCT outer

  , it "Deeply nested structs" $ do
      let level3 = Struct V.empty (V.singleton (1, BT_INT32, Int32 42))
          level2 = Struct V.empty (V.singleton (1, BT_STRUCT, level3))
          level1 = Struct V.empty (V.singleton (1, BT_STRUCT, level2))
      roundtrip BT_STRUCT level1
  ]

-- ============================================================
-- Container tests
-- ============================================================

containerTests :: Spec
containerTests = describe "Containers" $ sequence_
  [ it "Empty list" $
      roundtrip BT_LIST (List BT_INT32 V.empty)

  , it "List of ints" $
      roundtrip BT_LIST (List BT_INT32 (V.fromList [Int32 1, Int32 2, Int32 3]))

  , it "List of strings" $
      roundtrip BT_LIST (List BT_STRING (V.fromList
        [String (T.pack "a"), String (T.pack "b"), String (T.pack "c")]))

  , it "Empty set" $
      roundtrip BT_SET (Set BT_INT64 V.empty)

  , it "Set of int64" $
      roundtrip BT_SET (Set BT_INT64 (V.fromList [Int64 10, Int64 20, Int64 30]))

  , it "Empty map" $
      roundtrip BT_MAP (Map BT_STRING BT_INT32 V.empty)

  , it "Map string->int32" $
      roundtrip BT_MAP (Map BT_STRING BT_INT32 (V.fromList
        [ (String (T.pack "one"), Int32 1)
        , (String (T.pack "two"), Int32 2)
        ]))

  , it "List of structs" $ do
      let s1 = Struct V.empty (V.singleton (1, BT_INT32, Int32 1))
          s2 = Struct V.empty (V.singleton (1, BT_INT32, Int32 2))
      roundtrip BT_LIST (List BT_STRUCT (V.fromList [s1, s2]))

  , it "Large list (count > 7)" $
      roundtrip BT_LIST (List BT_INT32 (V.fromList (map Int32 [1..20])))
  ]

-- ============================================================
-- ZigZag varint encoding tests
-- ============================================================

zigzagTests :: Spec
zigzagTests = describe "ZigZag varint" $ sequence_
  [ it "ZigZag 0 -> 0" $ zigZagEncode 0 `shouldBe` 0
  , it "ZigZag -1 -> 1" $ zigZagEncode (-1) `shouldBe` 1
  , it "ZigZag 1 -> 2" $ zigZagEncode 1 `shouldBe` 2
  , it "ZigZag -2 -> 3" $ zigZagEncode (-2) `shouldBe` 3
  , it "ZigZag 2 -> 4" $ zigZagEncode 2 `shouldBe` 4
  , it "ZigZag maxBound Int64" $
      zigZagDecode (zigZagEncode (maxBound :: Int64)) `shouldBe` maxBound
  , it "ZigZag minBound Int64" $
      zigZagDecode (zigZagEncode (minBound :: Int64)) `shouldBe` minBound

  , it "Single-byte varint (0)" $ do
      let bs = encode (UInt8 0)
      BS.length bs `shouldBe` 1

  , it "Single-byte varint (127)" $ do
      let bs = encode (UInt8 127)
      BS.length bs `shouldBe` 1
  ]

-- ============================================================
-- Property-based roundtrips
-- ============================================================

propertyRoundtrips :: Spec
propertyRoundtrips = describe "Property roundtrips" $ sequence_
  [ it "Int32 roundtrip" $ property $ do
      n <- forAll $ Gen.int32 Range.linearBounded
      let val = Int32 n
      decode BT_INT32 (encode val) === Right val

  , it "Int64 roundtrip" $ property $ do
      n <- forAll $ Gen.int64 Range.linearBounded
      let val = Int64 n
      decode BT_INT64 (encode val) === Right val

  , it "UInt32 roundtrip" $ property $ do
      n <- forAll $ Gen.word32 Range.linearBounded
      let val = UInt32 n
      decode BT_UINT32 (encode val) === Right val

  , it "UInt64 roundtrip" $ property $ do
      n <- forAll $ Gen.word64 Range.linearBounded
      let val = UInt64 n
      decode BT_UINT64 (encode val) === Right val

  , it "String roundtrip" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 200) Gen.alphaNum
      let val = String t
      decode BT_STRING (encode val) === Right val

  , it "Bool roundtrip" $ property $ do
      b <- forAll Gen.bool
      let val = Bool b
      decode BT_BOOL (encode val) === Right val

  , it "Int8 roundtrip" $ property $ do
      n <- forAll $ Gen.int8 Range.linearBounded
      let val = Int8 n
      decode BT_INT8 (encode val) === Right val

  , it "Int16 roundtrip" $ property $ do
      n <- forAll $ Gen.int16 Range.linearBounded
      let val = Int16 n
      decode BT_INT16 (encode val) === Right val

  , it "UInt8 roundtrip" $ property $ do
      n <- forAll $ Gen.word8 Range.linearBounded
      let val = UInt8 n
      decode BT_UINT8 (encode val) === Right val

  , it "UInt16 roundtrip" $ property $ do
      n <- forAll $ Gen.word16 Range.linearBounded
      let val = UInt16 n
      decode BT_UINT16 (encode val) === Right val

  , it "Float roundtrip" $ property $ do
      f <- forAll $ Gen.float (Range.linearFrac (-1e6) 1e6)
      let val = Float f
      decode BT_FLOAT (encode val) === Right val

  , it "Double roundtrip" $ property $ do
      d <- forAll $ Gen.double (Range.linearFrac (-1e6) 1e6)
      let val = Double d
      decode BT_DOUBLE (encode val) === Right val

  , it "WString roundtrip" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
      let val = WString t
      decode BT_WSTRING (encode val) === Right val

  , it "Struct roundtrip" $ property $ do
      n <- forAll $ Gen.int32 Range.linearBounded
      t <- forAll $ Gen.text (Range.linear 0 64) Gen.alphaNum
      b <- forAll Gen.bool
      let val = Struct V.empty (V.fromList
            [ (1, BT_INT32, Int32 n)
            , (2, BT_STRING, String t)
            , (3, BT_BOOL, Bool b)
            ])
      decode BT_STRUCT (encode val) === Right val

  , it "List roundtrip" $ property $ do
      ns <- forAll $ Gen.list (Range.linear 0 15) (Gen.int32 Range.linearBounded)
      let val = List BT_INT32 (V.fromList (map Int32 ns))
      decode BT_LIST (encode val) === Right val
  ]

-- ============================================================
-- Helpers
-- ============================================================

roundtrip :: BondType -> Value -> IO ()
roundtrip bt val = decode bt (encode val) `shouldBe` Right val

zigZagEncode :: Int64 -> Word64
zigZagEncode n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))

zigZagDecode :: Word64 -> Int64
zigZagDecode n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))
