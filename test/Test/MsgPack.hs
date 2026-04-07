module Test.MsgPack (msgPackTests) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Word (Word64)
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
