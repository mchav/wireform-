module Test.MsgPack (msgPackTests) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Word (Word8, Word64)
import qualified Data.Vector as V

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import MsgPack.Encode (encode)
import MsgPack.Decode (decode)
import MsgPack.JSON (toJSON, fromJSON)
import qualified MsgPack.Value as MV

msgPackTests :: TestTree
msgPackTests = testGroup "MsgPack Encode/Decode"
  [ propertyRoundtrip
  , unitExactEncoding
  , unitEdgeCases
  , unitLargeContainers
  , unitExtRoundtrip
  , unitTimestampRoundtrip
  , jsonConversionTests
  , conformanceVectors
  ]

--------------------------------------------------------------------------------
-- Property: roundtrip for each primitive type
--------------------------------------------------------------------------------

propertyRoundtrip :: TestTree
propertyRoundtrip = testGroup "Roundtrip (property)"
  [ testProperty "Nil" $ property $ do
      let v = MV.Nil
      decode (encode v) === Right v

  , testProperty "Bool" $ property $ do
      b <- forAll Gen.bool
      let v = MV.Bool b
      decode (encode v) === Right v

  , testProperty "positive Word64" $ property $ do
      n <- forAll $ Gen.word64 (Range.linear 0 maxBound)
      let v = MV.Word n
      decode (encode v) === Right v

  , testProperty "negative Int64" $ property $ do
      n <- forAll $ Gen.int64 (Range.linear minBound (-1))
      let v = MV.Int n
      decode (encode v) === Right v

  , testProperty "Int64 full range" $ property $ do
      n <- forAll $ Gen.int64 Range.linearBounded
      let v = if n >= 0 then MV.Word (fromIntegral n) else MV.Int n
          encoded = encode v
          decoded = decode encoded
      case decoded of
        Right (MV.Word w) | n >= 0 -> fromIntegral w === n
        Right (MV.Int i)  | n < 0  -> i === n
        other -> do
          annotate (show other)
          failure

  , testProperty "Float" $ property $ do
      f <- forAll $ Gen.float (Range.linearFrac (-1e6) 1e6)
      let v = MV.Float f
      decode (encode v) === Right v

  , testProperty "Double" $ property $ do
      d <- forAll $ Gen.double (Range.linearFrac (-1e12) 1e12)
      let v = MV.Double d
      decode (encode v) === Right v

  , testProperty "String" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 128) Gen.unicode
      let v = MV.String t
      decode (encode v) === Right v

  , testProperty "Binary" $ property $ do
      bs <- forAll $ Gen.bytes (Range.linear 0 256)
      let v = MV.Binary bs
      decode (encode v) === Right v

  , testProperty "Array of ints" $ property $ do
      ns <- forAll $ Gen.list (Range.linear 0 30) (Gen.word64 (Range.linear 0 0xFFFF))
      let v = MV.Array (V.fromList (map MV.Word ns))
      decode (encode v) === Right v

  , testProperty "Map of string->int" $ property $ do
      entries <- forAll $ Gen.list (Range.linear 0 20) $ do
        k <- Gen.text (Range.linear 1 16) Gen.alphaNum
        n <- Gen.int64 (Range.linear (-1000) 1000)
        pure (k, n)
      let v = MV.Map (V.fromList
                [ (MV.String k, if n >= 0 then MV.Word (fromIntegral n) else MV.Int n)
                | (k, n) <- entries
                ])
      decode (encode v) === Right v
  ]

--------------------------------------------------------------------------------
-- Unit: exact byte encoding
--------------------------------------------------------------------------------

unitExactEncoding :: TestTree
unitExactEncoding = testGroup "Exact byte encoding"
  [ testCase "nil = 0xc0" $
      encode MV.Nil @?= BS.pack [0xc0]

  , testCase "false = 0xc2" $
      encode (MV.Bool False) @?= BS.pack [0xc2]

  , testCase "true = 0xc3" $
      encode (MV.Bool True) @?= BS.pack [0xc3]

  -- fixint
  , testCase "fixint 0 = 0x00" $
      encode (MV.Word 0) @?= BS.pack [0x00]

  , testCase "fixint 127 = 0x7f" $
      encode (MV.Word 127) @?= BS.pack [0x7f]

  , testCase "fixint -1 = 0xff" $
      encode (MV.Int (-1)) @?= BS.pack [0xff]

  , testCase "fixint -32 = 0xe0" $
      encode (MV.Int (-32)) @?= BS.pack [0xe0]

  -- uint8
  , testCase "uint8 128 = 0xcc 0x80" $
      encode (MV.Word 128) @?= BS.pack [0xcc, 0x80]

  , testCase "uint8 255 = 0xcc 0xff" $
      encode (MV.Word 255) @?= BS.pack [0xcc, 0xff]

  -- uint16
  , testCase "uint16 256 = 0xcd 0x01 0x00" $
      encode (MV.Word 256) @?= BS.pack [0xcd, 0x01, 0x00]

  -- int8
  , testCase "int8 -33 = 0xd0 0xdf" $
      encode (MV.Int (-33)) @?= BS.pack [0xd0, 0xdf]

  , testCase "int8 -128 = 0xd0 0x80" $
      encode (MV.Int (-128)) @?= BS.pack [0xd0, 0x80]

  -- int16
  , testCase "int16 -129 = 0xd1 0xff 0x7f" $
      encode (MV.Int (-129)) @?= BS.pack [0xd1, 0xff, 0x7f]

  -- fixstr
  , testCase "fixstr \"\" = 0xa0" $
      encode (MV.String "") @?= BS.pack [0xa0]

  , testCase "fixstr \"hello\" = 0xa5 + bytes" $
      encode (MV.String "hello") @?= BS.pack [0xa5, 0x68, 0x65, 0x6c, 0x6c, 0x6f]

  -- fixarray
  , testCase "fixarray [] = 0x90" $
      encode (MV.Array V.empty) @?= BS.pack [0x90]

  , testCase "fixarray [1,2] = 0x92 0x01 0x02" $
      encode (MV.Array (V.fromList [MV.Word 1, MV.Word 2]))
        @?= BS.pack [0x92, 0x01, 0x02]

  -- fixmap
  , testCase "fixmap {} = 0x80" $
      encode (MV.Map V.empty) @?= BS.pack [0x80]

  , testCase "fixmap {1:2} = 0x81 0x01 0x02" $
      encode (MV.Map (V.fromList [(MV.Word 1, MV.Word 2)]))
        @?= BS.pack [0x81, 0x01, 0x02]

  -- float32
  , testCase "float32 0.0" $
      encode (MV.Float 0.0) @?= BS.pack [0xca, 0x00, 0x00, 0x00, 0x00]

  -- float64
  , testCase "float64 0.0" $
      encode (MV.Double 0.0) @?= BS.pack [0xcb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]

  -- bin8
  , testCase "bin8 empty" $
      encode (MV.Binary BS.empty) @?= BS.pack [0xc4, 0x00]

  , testCase "bin8 [0xDE, 0xAD]" $
      encode (MV.Binary (BS.pack [0xDE, 0xAD])) @?= BS.pack [0xc4, 0x02, 0xDE, 0xAD]
  ]

--------------------------------------------------------------------------------
-- Unit: edge cases
--------------------------------------------------------------------------------

unitEdgeCases :: TestTree
unitEdgeCases = testGroup "Edge cases"
  [ testCase "empty array roundtrip" $ do
      let v = MV.Array V.empty
      decode (encode v) @?= Right v

  , testCase "empty map roundtrip" $ do
      let v = MV.Map V.empty
      decode (encode v) @?= Right v

  , testCase "empty string roundtrip" $ do
      let v = MV.String ""
      decode (encode v) @?= Right v

  , testCase "empty binary roundtrip" $ do
      let v = MV.Binary BS.empty
      decode (encode v) @?= Right v

  , testCase "nil roundtrip" $ do
      decode (encode MV.Nil) @?= Right MV.Nil

  , testCase "max Word64" $ do
      let v = MV.Word maxBound
      decode (encode v) @?= Right v

  , testCase "min Int64" $ do
      let v = MV.Int minBound
      decode (encode v) @?= Right v

  , testCase "max Int64 as Word" $ do
      let v = MV.Word (fromIntegral (maxBound :: Int64))
      decode (encode v) @?= Right v

  , testCase "Word 0" $ do
      let v = MV.Word 0
      decode (encode v) @?= Right v

  , testCase "Int -1" $ do
      let v = MV.Int (-1)
      decode (encode v) @?= Right v

  , testCase "nested array" $ do
      let v = MV.Array (V.fromList [MV.Array (V.fromList [MV.Word 1, MV.Word 2]), MV.String "x"])
      decode (encode v) @?= Right v

  , testCase "nested map" $ do
      let v = MV.Map (V.fromList
                [(MV.String "inner", MV.Map (V.fromList [(MV.Word 1, MV.Bool True)]))])
      decode (encode v) @?= Right v

  , testCase "decode empty input fails" $
      case decode BS.empty of
        Left _  -> pure ()
        Right _ -> assertFailure "expected failure on empty input"

  , testCase "str8 (32+ bytes)" $ do
      let t = T.replicate 32 "x"
          v = MV.String t
      decode (encode v) @?= Right v

  , testCase "str16 (256+ bytes)" $ do
      let t = T.replicate 300 "a"
          v = MV.String t
      decode (encode v) @?= Right v

  , testCase "bin16 (256+ bytes)" $ do
      let bs = BS.replicate 300 0x42
          v = MV.Binary bs
      decode (encode v) @?= Right v
  ]

--------------------------------------------------------------------------------
-- Unit: large containers (> 15 elements, triggers array16/map16)
--------------------------------------------------------------------------------

unitLargeContainers :: TestTree
unitLargeContainers = testGroup "Large containers (array16/map16)"
  [ testCase "array with 16 elements" $ do
      let v = MV.Array (V.fromList [MV.Word (fromIntegral i) | i <- [0..15 :: Int]])
      decode (encode v) @?= Right v

  , testCase "array with 20 elements" $ do
      let v = MV.Array (V.fromList [MV.Word (fromIntegral i) | i <- [0..19 :: Int]])
          bs = encode v
      BS.index bs 0 @?= 0xdc  -- array16 tag
      decode bs @?= Right v

  , testCase "array with 100 elements" $ do
      let v = MV.Array (V.fromList [MV.Word (fromIntegral i) | i <- [0..99 :: Int]])
      decode (encode v) @?= Right v

  , testCase "map with 16 entries" $ do
      let v = MV.Map (V.fromList
                [(MV.String (T.pack (show i)), MV.Word (fromIntegral i)) | i <- [0..15 :: Int]])
      decode (encode v) @?= Right v

  , testCase "map with 20 entries" $ do
      let v = MV.Map (V.fromList
                [(MV.String (T.pack (show i)), MV.Word (fromIntegral i)) | i <- [0..19 :: Int]])
          bs = encode v
      BS.index bs 0 @?= 0xde  -- map16 tag
      decode bs @?= Right v
  ]

--------------------------------------------------------------------------------
-- Unit: Ext type roundtrip
--------------------------------------------------------------------------------

unitExtRoundtrip :: TestTree
unitExtRoundtrip = testGroup "Ext type roundtrip"
  [ testCase "fixext1" $ do
      let v = MV.Ext 1 (BS.pack [0x42])
      decode (encode v) @?= Right v

  , testCase "fixext2" $ do
      let v = MV.Ext 2 (BS.pack [0x42, 0x43])
      decode (encode v) @?= Right v

  , testCase "fixext4" $ do
      let v = MV.Ext 3 (BS.pack [0x01, 0x02, 0x03, 0x04])
      decode (encode v) @?= Right v

  , testCase "fixext8" $ do
      let v = MV.Ext 4 (BS.replicate 8 0xAA)
      decode (encode v) @?= Right v

  , testCase "fixext16" $ do
      let v = MV.Ext 5 (BS.replicate 16 0xBB)
      decode (encode v) @?= Right v

  , testCase "ext8 (3 bytes)" $ do
      let v = MV.Ext 10 (BS.pack [0x01, 0x02, 0x03])
      decode (encode v) @?= Right v

  , testCase "ext8 (100 bytes)" $ do
      let v = MV.Ext 42 (BS.replicate 100 0xCC)
      decode (encode v) @?= Right v

  , testCase "negative ext type" $ do
      let v = MV.Ext (-5) (BS.pack [0x01])
      decode (encode v) @?= Right v
  ]

--------------------------------------------------------------------------------
-- Unit: Timestamp roundtrip
--------------------------------------------------------------------------------

unitTimestampRoundtrip :: TestTree
unitTimestampRoundtrip = testGroup "Timestamp roundtrip"
  [ testCase "timestamp32 (seconds only)" $ do
      let v = MV.Timestamp 1000 0
      decode (encode v) @?= Right v

  , testCase "timestamp32 (max uint32 seconds)" $ do
      let v = MV.Timestamp 0xFFFFFFFF 0
      decode (encode v) @?= Right v

  , testCase "timestamp32 (zero)" $ do
      let v = MV.Timestamp 0 0
      decode (encode v) @?= Right v

  , testCase "timestamp64 (with nanoseconds)" $ do
      let v = MV.Timestamp 1000 500000000
      decode (encode v) @?= Right v

  , testCase "timestamp96 (negative seconds)" $ do
      let v = MV.Timestamp (-1) 0
      decode (encode v) @?= Right v

  , testCase "timestamp96 (large negative)" $ do
      let v = MV.Timestamp (-1000000) 123456
      decode (encode v) @?= Right v
  ]

--------------------------------------------------------------------------------
-- JSON conversion tests
--------------------------------------------------------------------------------

jsonConversionTests :: TestTree
jsonConversionTests = testGroup "JSON conversion"
  [ testCase "nil -> null -> nil" $ do
      let v = MV.Nil
      fromJSON (toJSON v) @?= v

  , testCase "bool true roundtrip" $ do
      let v = MV.Bool True
      fromJSON (toJSON v) @?= v

  , testCase "bool false roundtrip" $ do
      let v = MV.Bool False
      fromJSON (toJSON v) @?= v

  , testCase "small int roundtrip" $ do
      let v = MV.Int (-42)
      fromJSON (toJSON v) @?= v

  , testCase "word roundtrip" $ do
      let v = MV.Word 12345
          result = fromJSON (toJSON v)
      case result of
        MV.Int n  -> fromIntegral n @?= (12345 :: Word64)
        MV.Word n -> n @?= 12345
        other     -> assertFailure $ "unexpected: " ++ show other

  , testCase "string roundtrip" $ do
      let v = MV.String "hello world"
      fromJSON (toJSON v) @?= v

  , testCase "array roundtrip" $ do
      let v = MV.Array (V.fromList [MV.Nil, MV.Bool True, MV.String "x"])
          result = fromJSON (toJSON v)
      case result of
        MV.Array elems -> V.length elems @?= 3
        other -> assertFailure $ "unexpected: " ++ show other

  , testCase "map with string keys -> object" $ do
      let v = MV.Map (V.fromList [(MV.String "a", MV.Bool True)])
          result = fromJSON (toJSON v)
      case result of
        MV.Map kvs -> V.length kvs @?= 1
        other -> assertFailure $ "unexpected: " ++ show other
  ]

--------------------------------------------------------------------------------
-- Conformance: kawanet/msgpack-test-suite vectors (embedded)
-- For each entry, we verify that the FIRST msgpack encoding (the most compact
-- canonical form) decodes to the expected value.
--------------------------------------------------------------------------------

parseHexDash :: String -> [Word8]
parseHexDash = go
  where
    go [] = []
    go ('-':rest) = go rest
    go (a:b:rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : go rest
    go _ = error "parseHexDash: odd number of hex digits"

    digitToInt :: Char -> Int
    digitToInt c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise = error $ "digitToInt: invalid hex: " ++ [c]

conformanceVectors :: TestTree
conformanceVectors = testGroup "kawanet/msgpack-test-suite conformance"
  [ testGroup "nil" nilDecodeTests
  , testGroup "bool" boolDecodeTests
  , testGroup "positive numbers" positiveNumberDecodeTests
  , testGroup "negative numbers" negativeNumberDecodeTests
  , testGroup "strings" stringDecodeTests
  , testGroup "binary" binaryDecodeTests
  , testGroup "arrays" arrayDecodeTests
  , testGroup "maps" mapDecodeTests
  , testGroup "ext" extDecodeTests
  ]
  where
    mkDecodeTest :: String -> String -> MV.Value -> TestTree
    mkDecodeTest name hex expected =
      testCase (name ++ " [" ++ hex ++ "]") $
        decode (BS.pack (parseHexDash hex)) @?= Right expected

    nilDecodeTests =
      [ mkDecodeTest "nil" "c0" MV.Nil ]

    boolDecodeTests =
      [ mkDecodeTest "false" "c2" (MV.Bool False)
      , mkDecodeTest "true" "c3" (MV.Bool True)
      ]

    positiveNumberDecodeTests =
      [ mkDecodeTest "0 fixint" "00" (MV.Word 0)
      , mkDecodeTest "1 fixint" "01" (MV.Word 1)
      , mkDecodeTest "127 fixint" "7f" (MV.Word 127)
      , mkDecodeTest "128 uint8" "cc-80" (MV.Word 128)
      , mkDecodeTest "255 uint8" "cc-ff" (MV.Word 255)
      , mkDecodeTest "256 uint16" "cd-01-00" (MV.Word 256)
      , mkDecodeTest "65535 uint16" "cd-ff-ff" (MV.Word 65535)
      , mkDecodeTest "65536 uint32" "ce-00-01-00-00" (MV.Word 65536)
      , mkDecodeTest "2147483647 uint32" "ce-7f-ff-ff-ff" (MV.Word 2147483647)
      , mkDecodeTest "2147483648 uint32" "ce-80-00-00-00" (MV.Word 2147483648)
      , mkDecodeTest "4294967295 uint32" "ce-ff-ff-ff-ff" (MV.Word 4294967295)
      , mkDecodeTest "4294967296 uint64" "cf-00-00-00-01-00-00-00-00" (MV.Word 4294967296)
      , mkDecodeTest "max uint64" "cf-ff-ff-ff-ff-ff-ff-ff-ff" (MV.Word 18446744073709551615)
      ]

    negativeNumberDecodeTests =
      [ mkDecodeTest "-1 fixint" "ff" (MV.Int (-1))
      , mkDecodeTest "-32 fixint" "e0" (MV.Int (-32))
      , mkDecodeTest "-33 int8" "d0-df" (MV.Int (-33))
      , mkDecodeTest "-128 int8" "d0-80" (MV.Int (-128))
      , mkDecodeTest "-256 int16" "d1-ff-00" (MV.Int (-256))
      , mkDecodeTest "-32768 int16" "d1-80-00" (MV.Int (-32768))
      , mkDecodeTest "-65536 int32" "d2-ff-ff-00-00" (MV.Int (-65536))
      , mkDecodeTest "-2147483648 int32" "d2-80-00-00-00" (MV.Int (-2147483648))
      ]

    stringDecodeTests =
      [ mkDecodeTest "empty string" "a0" (MV.String "")
      , mkDecodeTest "\"a\"" "a1-61" (MV.String "a")
      , mkDecodeTest "31-char fixstr"
          "bf-31-32-33-34-35-36-37-38-39-30-31-32-33-34-35-36-37-38-39-30-31-32-33-34-35-36-37-38-39-30-31"
          (MV.String "1234567890123456789012345678901")
      , mkDecodeTest "32-char str8"
          "d9-20-31-32-33-34-35-36-37-38-39-30-31-32-33-34-35-36-37-38-39-30-31-32-33-34-35-36-37-38-39-30-31-32"
          (MV.String "12345678901234567890123456789012")
      -- UTF-8 strings
      , mkDecodeTest "Cyrillic" "b2-d0-9a-d0-b8-d1-80-d0-b8-d0-bb-d0-bb-d0-b8-d1-86-d0-b0"
          (MV.String "\1050\1080\1088\1080\1083\1083\1080\1094\1072")
      , mkDecodeTest "Hiragana" "ac-e3-81-b2-e3-82-89-e3-81-8c-e3-81-aa"
          (MV.String "\12402\12425\12364\12394")
      , mkDecodeTest "Korean" "a6-ed-95-9c-ea-b8-80"
          (MV.String "\54620\44544")
      , mkDecodeTest "Emoji heart" "a3-e2-9d-a4"
          (MV.String "\10084")
      , mkDecodeTest "Emoji beer" "a4-f0-9f-8d-ba"
          (MV.String "\127866")
      ]

    binaryDecodeTests =
      [ mkDecodeTest "empty bin" "c4-00" (MV.Binary BS.empty)
      , mkDecodeTest "bin [0x01]" "c4-01-01" (MV.Binary (BS.pack [0x01]))
      , mkDecodeTest "bin [0x00,0xff]" "c4-02-00-ff" (MV.Binary (BS.pack [0x00, 0xff]))
      ]

    arrayDecodeTests =
      [ mkDecodeTest "empty array" "90" (MV.Array V.empty)
      , mkDecodeTest "array [1]" "91-01" (MV.Array (V.fromList [MV.Word 1]))
      , mkDecodeTest "array [1..15]" "9f-01-02-03-04-05-06-07-08-09-0a-0b-0c-0d-0e-0f"
          (MV.Array (V.fromList [MV.Word (fromIntegral i) | i <- [1..15 :: Int]]))
      , mkDecodeTest "array [1..16] array16"
          "dc-00-10-01-02-03-04-05-06-07-08-09-0a-0b-0c-0d-0e-0f-10"
          (MV.Array (V.fromList [MV.Word (fromIntegral i) | i <- [1..16 :: Int]]))
      , mkDecodeTest "array [\"a\"]" "91-a1-61"
          (MV.Array (V.fromList [MV.String "a"]))
      , mkDecodeTest "nested [[]]" "91-90"
          (MV.Array (V.fromList [MV.Array V.empty]))
      , mkDecodeTest "nested [{}]" "91-80"
          (MV.Array (V.fromList [MV.Map V.empty]))
      ]

    mapDecodeTests =
      [ mkDecodeTest "empty map" "80" (MV.Map V.empty)
      , mkDecodeTest "map {\"a\":1}" "81-a1-61-01"
          (MV.Map (V.fromList [(MV.String "a", MV.Word 1)]))
      , mkDecodeTest "map {\"a\":\"A\"}" "81-a1-61-a1-41"
          (MV.Map (V.fromList [(MV.String "a", MV.String "A")]))
      , mkDecodeTest "nested {\"a\":{}}" "81-a1-61-80"
          (MV.Map (V.fromList [(MV.String "a", MV.Map V.empty)]))
      , mkDecodeTest "nested {\"a\":[]}" "81-a1-61-90"
          (MV.Map (V.fromList [(MV.String "a", MV.Array V.empty)]))
      ]

    extDecodeTests =
      [ mkDecodeTest "fixext1 type=1" "d4-01-10"
          (MV.Ext 1 (BS.pack [0x10]))
      , mkDecodeTest "fixext2 type=2" "d5-02-20-21"
          (MV.Ext 2 (BS.pack [0x20, 0x21]))
      , mkDecodeTest "fixext4 type=3" "d6-03-30-31-32-33"
          (MV.Ext 3 (BS.pack [0x30, 0x31, 0x32, 0x33]))
      , mkDecodeTest "fixext8 type=4" "d7-04-40-41-42-43-44-45-46-47"
          (MV.Ext 4 (BS.pack [0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47]))
      , mkDecodeTest "fixext16 type=5"
          "d8-05-50-51-52-53-54-55-56-57-58-59-5a-5b-5c-5d-5e-5f"
          (MV.Ext 5 (BS.pack [0x50..0x5f]))
      , mkDecodeTest "ext8 type=6 empty" "c7-00-06"
          (MV.Ext 6 BS.empty)
      , mkDecodeTest "ext8 type=7 3bytes" "c7-03-07-70-71-72"
          (MV.Ext 7 (BS.pack [0x70, 0x71, 0x72]))
      ]
