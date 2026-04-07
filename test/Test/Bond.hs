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
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Bond.Value
import Bond.Encode (encode)
import Bond.Decode (decode)

bondTests :: TestTree
bondTests = testGroup "Bond"
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

primitiveRoundtrips :: TestTree
primitiveRoundtrips = testGroup "Primitive roundtrips"
  [ testCase "Bool true" $ roundtrip BT_BOOL (Bool True)
  , testCase "Bool false" $ roundtrip BT_BOOL (Bool False)
  , testCase "Int8 positive" $ roundtrip BT_INT8 (Int8 42)
  , testCase "Int8 negative" $ roundtrip BT_INT8 (Int8 (-1))
  , testCase "Int8 zero" $ roundtrip BT_INT8 (Int8 0)
  , testCase "Int16" $ roundtrip BT_INT16 (Int16 1000)
  , testCase "Int16 negative" $ roundtrip BT_INT16 (Int16 (-1000))
  , testCase "Int32" $ roundtrip BT_INT32 (Int32 100000)
  , testCase "Int32 negative" $ roundtrip BT_INT32 (Int32 (-100000))
  , testCase "Int64" $ roundtrip BT_INT64 (Int64 9999999999)
  , testCase "Int64 negative" $ roundtrip BT_INT64 (Int64 (-9999999999))
  , testCase "UInt8" $ roundtrip BT_UINT8 (UInt8 255)
  , testCase "UInt16" $ roundtrip BT_UINT16 (UInt16 65535)
  , testCase "UInt32" $ roundtrip BT_UINT32 (UInt32 4000000000)
  , testCase "UInt64" $ roundtrip BT_UINT64 (UInt64 18000000000000000000)
  , testCase "Float" $ roundtrip BT_FLOAT (Float 3.14)
  , testCase "Double" $ roundtrip BT_DOUBLE (Double 2.718281828)
  , testCase "String empty" $ roundtrip BT_STRING (String T.empty)
  , testCase "String hello" $ roundtrip BT_STRING (String (T.pack "hello world"))
  , testCase "WString" $ roundtrip BT_WSTRING (WString (T.pack "wide string"))
  , testCase "Enum encodes as Int32" $ do
      let bs = encode (Enum 42)
      decode BT_INT32 bs @?= Right (Int32 42)
  ]

-- ============================================================
-- Struct tests
-- ============================================================

structTests :: TestTree
structTests = testGroup "Struct encode/decode"
  [ testCase "Empty struct" $ roundtrip BT_STRUCT (Struct V.empty V.empty)

  , testCase "Struct with one field" $ do
      let val = Struct V.empty (V.singleton (1, BT_INT32, Int32 42))
      roundtrip BT_STRUCT val

  , testCase "Struct with multiple fields" $ do
      let val = Struct V.empty (V.fromList
            [ (1, BT_BOOL, Bool True)
            , (2, BT_INT32, Int32 100)
            , (3, BT_STRING, String (T.pack "test"))
            , (4, BT_DOUBLE, Double 1.5)
            ])
      roundtrip BT_STRUCT val

  , testCase "Struct with non-sequential field ids" $ do
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

nestedStructTests :: TestTree
nestedStructTests = testGroup "Nested structs"
  [ testCase "Struct containing struct" $ do
      let inner = Struct V.empty (V.fromList
            [ (1, BT_INT32, Int32 1)
            , (2, BT_STRING, String (T.pack "inner"))
            ])
          outer = Struct V.empty (V.fromList
            [ (1, BT_STRUCT, inner)
            , (2, BT_BOOL, Bool True)
            ])
      roundtrip BT_STRUCT outer

  , testCase "Deeply nested structs" $ do
      let level3 = Struct V.empty (V.singleton (1, BT_INT32, Int32 42))
          level2 = Struct V.empty (V.singleton (1, BT_STRUCT, level3))
          level1 = Struct V.empty (V.singleton (1, BT_STRUCT, level2))
      roundtrip BT_STRUCT level1
  ]

-- ============================================================
-- Container tests
-- ============================================================

containerTests :: TestTree
containerTests = testGroup "Containers"
  [ testCase "Empty list" $
      roundtrip BT_LIST (List BT_INT32 V.empty)

  , testCase "List of ints" $
      roundtrip BT_LIST (List BT_INT32 (V.fromList [Int32 1, Int32 2, Int32 3]))

  , testCase "List of strings" $
      roundtrip BT_LIST (List BT_STRING (V.fromList
        [String (T.pack "a"), String (T.pack "b"), String (T.pack "c")]))

  , testCase "Empty set" $
      roundtrip BT_SET (Set BT_INT64 V.empty)

  , testCase "Set of int64" $
      roundtrip BT_SET (Set BT_INT64 (V.fromList [Int64 10, Int64 20, Int64 30]))

  , testCase "Empty map" $
      roundtrip BT_MAP (Map BT_STRING BT_INT32 V.empty)

  , testCase "Map string->int32" $
      roundtrip BT_MAP (Map BT_STRING BT_INT32 (V.fromList
        [ (String (T.pack "one"), Int32 1)
        , (String (T.pack "two"), Int32 2)
        ]))

  , testCase "List of structs" $ do
      let s1 = Struct V.empty (V.singleton (1, BT_INT32, Int32 1))
          s2 = Struct V.empty (V.singleton (1, BT_INT32, Int32 2))
      roundtrip BT_LIST (List BT_STRUCT (V.fromList [s1, s2]))

  , testCase "Large list (count > 7)" $
      roundtrip BT_LIST (List BT_INT32 (V.fromList (map Int32 [1..20])))
  ]

-- ============================================================
-- ZigZag varint encoding tests
-- ============================================================

zigzagTests :: TestTree
zigzagTests = testGroup "ZigZag varint"
  [ testCase "ZigZag 0 -> 0" $ zigZagEncode 0 @?= 0
  , testCase "ZigZag -1 -> 1" $ zigZagEncode (-1) @?= 1
  , testCase "ZigZag 1 -> 2" $ zigZagEncode 1 @?= 2
  , testCase "ZigZag -2 -> 3" $ zigZagEncode (-2) @?= 3
  , testCase "ZigZag 2 -> 4" $ zigZagEncode 2 @?= 4
  , testCase "ZigZag maxBound Int64" $
      zigZagDecode (zigZagEncode (maxBound :: Int64)) @?= maxBound
  , testCase "ZigZag minBound Int64" $
      zigZagDecode (zigZagEncode (minBound :: Int64)) @?= minBound

  , testCase "Single-byte varint (0)" $ do
      let bs = encode (UInt8 0)
      BS.length bs @?= 1

  , testCase "Single-byte varint (127)" $ do
      let bs = encode (UInt8 127)
      BS.length bs @?= 1
  ]

-- ============================================================
-- Property-based roundtrips
-- ============================================================

propertyRoundtrips :: TestTree
propertyRoundtrips = testGroup "Property roundtrips"
  [ testProperty "Int32 roundtrip" $ property $ do
      n <- forAll $ Gen.int32 Range.linearBounded
      let val = Int32 n
      decode BT_INT32 (encode val) === Right val

  , testProperty "Int64 roundtrip" $ property $ do
      n <- forAll $ Gen.int64 Range.linearBounded
      let val = Int64 n
      decode BT_INT64 (encode val) === Right val

  , testProperty "UInt32 roundtrip" $ property $ do
      n <- forAll $ Gen.word32 Range.linearBounded
      let val = UInt32 n
      decode BT_UINT32 (encode val) === Right val

  , testProperty "UInt64 roundtrip" $ property $ do
      n <- forAll $ Gen.word64 Range.linearBounded
      let val = UInt64 n
      decode BT_UINT64 (encode val) === Right val

  , testProperty "String roundtrip" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 200) Gen.alphaNum
      let val = String t
      decode BT_STRING (encode val) === Right val

  , testProperty "Bool roundtrip" $ property $ do
      b <- forAll Gen.bool
      let val = Bool b
      decode BT_BOOL (encode val) === Right val
  ]

-- ============================================================
-- Helpers
-- ============================================================

roundtrip :: BondType -> Value -> Assertion
roundtrip bt val = decode bt (encode val) @?= Right val

zigZagEncode :: Int64 -> Word64
zigZagEncode n = fromIntegral ((n `shiftL` 1) `xor` (n `shiftR` 63))

zigZagDecode :: Word64 -> Int64
zigZagDecode n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))
