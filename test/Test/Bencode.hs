{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Bencode (bencodeTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import Bencode.Value
import Bencode.Encode
import Bencode.Decode
import Bencode.Class

bencodeTests :: TestTree
bencodeTests = testGroup "Bencode"
  [ valueTests
  , encodeTests
  , decodeTests
  , roundtripTests
  , classTests
  , genericTests
  , edgeCaseTests
  ]

valueTests :: TestTree
valueTests = testGroup "Value types"
  [ testCase "BString construction" $
      BString "hello" @?= BString "hello"
  , testCase "BInteger construction" $
      BInteger 42 @?= BInteger 42
  , testCase "BList construction" $
      BList (V.fromList [BInteger 1, BInteger 2]) @?=
        BList (V.fromList [BInteger 1, BInteger 2])
  , testCase "BDict construction" $
      BDict (V.fromList [("key", BString "val")]) @?=
        BDict (V.fromList [("key", BString "val")])
  ]

encodeTests :: TestTree
encodeTests = testGroup "Encode"
  [ testCase "encode string: 5:hello" $ do
      let result = encode (BString "hello")
      result @?= "5:hello"

  , testCase "encode empty string: 0:" $ do
      let result = encode (BString "")
      result @?= "0:"

  , testCase "encode integer: i42e" $ do
      let result = encode (BInteger 42)
      result @?= "i42e"

  , testCase "encode negative integer: i-3e" $ do
      let result = encode (BInteger (-3))
      result @?= "i-3e"

  , testCase "encode zero: i0e" $ do
      let result = encode (BInteger 0)
      result @?= "i0e"

  , testCase "encode list: l5:helloi42ee" $ do
      let result = encode (BList (V.fromList [BString "hello", BInteger 42]))
      result @?= "l5:helloi42ee"

  , testCase "encode empty list: le" $ do
      let result = encode (BList V.empty)
      result @?= "le"

  , testCase "encode dict" $ do
      let result = encode (BDict (V.fromList [("age", BInteger 30), ("name", BString "Alice")]))
      result @?= "d3:agei30e4:name5:Alicee"
  ]

decodeTests :: TestTree
decodeTests = testGroup "Decode"
  [ testCase "decode string: 5:hello" $ do
      let Right val = decode "5:hello"
      val @?= BString "hello"

  , testCase "decode integer: i42e" $ do
      let Right val = decode "i42e"
      val @?= BInteger 42

  , testCase "decode negative integer: i-3e" $ do
      let Right val = decode "i-3e"
      val @?= BInteger (-3)

  , testCase "decode list: l5:helloi42ee" $ do
      let Right val = decode "l5:helloi42ee"
      val @?= BList (V.fromList [BString "hello", BInteger 42])

  , testCase "decode dict" $ do
      let Right val = decode "d3:agei30e4:name5:Alicee"
      val @?= BDict (V.fromList [("age", BInteger 30), ("name", BString "Alice")])

  , testCase "decode empty list: le" $ do
      let Right val = decode "le"
      val @?= BList V.empty

  , testCase "decode empty dict: de" $ do
      let Right val = decode "de"
      val @?= BDict V.empty

  , testCase "decode error on empty input" $ do
      let result = decode ""
      assertBool "should be Left" (isLeft result)

  , testCase "decode error on trailing data" $ do
      let result = decode "i42eextra"
      assertBool "should be Left" (isLeft result)
  ]

roundtripTests :: TestTree
roundtripTests = testGroup "Roundtrip"
  [ testCase "string roundtrip" $ do
      let val = BString "hello world"
      decode (encode val) @?= Right val

  , testCase "integer roundtrip" $ do
      let val = BInteger 123456789
      decode (encode val) @?= Right val

  , testCase "list roundtrip" $ do
      let val = BList (V.fromList [BString "a", BInteger 1, BList V.empty])
      decode (encode val) @?= Right val

  , testCase "dict roundtrip" $ do
      let val = BDict (V.fromList
            [ ("author", BString "Alice")
            , ("title", BString "Book")
            , ("year", BInteger 2024)
            ])
      decode (encode val) @?= Right val

  , testCase "nested structure roundtrip" $ do
      let val = BDict (V.fromList
            [ ("files", BList (V.fromList
                [ BDict (V.fromList [("length", BInteger 1024), ("path", BString "/file.txt")])
                , BDict (V.fromList [("length", BInteger 2048), ("path", BString "/img.png")])
                ]))
            , ("name", BString "torrent")
            ])
      decode (encode val) @?= Right val
  ]

classTests :: TestTree
classTests = testGroup "Class instances"
  [ testCase "Int roundtrip via class" $ do
      let val = 42 :: Int
      fromBencode (toBencode val) @?= Right val

  , testCase "Text roundtrip via class" $ do
      let val = "hello" :: ByteString
      fromBencode (toBencode val) @?= Right val

  , testCase "Bool roundtrip via class" $ do
      fromBencode (toBencode True) @?= Right True
      fromBencode (toBencode False) @?= Right False

  , testCase "List roundtrip via class" $ do
      let val = [1, 2, 3] :: [Int]
      fromBencode (toBencode val) @?= Right val

  , testCase "Vector roundtrip via class" $ do
      let val = V.fromList [10, 20, 30] :: Vector Int
      fromBencode (toBencode val) @?= Right val

  , testCase "Maybe roundtrip via class" $ do
      let val = Just (42 :: Int)
      fromBencode (toBencode val) @?= Right val
      let nothing = Nothing :: Maybe Int
      fromBencode (toBencode nothing) @?= Right nothing
  ]

data TorrentInfo = TorrentInfo
  { name :: !ByteString
  , length_ :: !Int
  } deriving stock (Show, Eq, Generic)

instance ToBencode TorrentInfo where
  toBencode (TorrentInfo n l) = BDict (V.fromList
    [("length_", toBencode l), ("name", toBencode n)])

instance FromBencode TorrentInfo where
  fromBencode (BDict kvs) = do
    n <- maybe (Left "missing name") fromBencode (lookupBencode "name" kvs)
    l <- maybe (Left "missing length_") fromBencode (lookupBencode "length_" kvs)
    Right (TorrentInfo n l)
  fromBencode _ = Left "expected BDict"

lookupBencode :: ByteString -> Vector (ByteString, Value) -> Maybe Value
lookupBencode key kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (k, v) <- kvs V.! i, k == key = Just v
      | otherwise = go (i + 1)

genericTests :: TestTree
genericTests = testGroup "Generic deriving"
  [ testCase "torrent info roundtrip" $ do
      let info = TorrentInfo "test.txt" 4096
          encoded = encodeBencode info
          Right decoded = decodeBencode encoded :: Either String TorrentInfo
      decoded @?= info
  ]

data SimpleRec = SimpleRec
  { sName :: !ByteString
  , sAge :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToBencode, FromBencode)

edgeCaseTests :: TestTree
edgeCaseTests = testGroup "Edge cases"
  [ testCase "dict key ordering preserved on decode" $ do
      let Right val = decode "d1:bi2e1:ai1ee"
      case val of
        BDict kvs ->
          V.toList (V.map fst kvs) @?= ["a", "b"]
        _ -> assertFailure "expected BDict"

  , testCase "large integer" $ do
      let val = BInteger 999999999999999
      decode (encode val) @?= Right val

  , testCase "binary data in string" $ do
      let val = BString (BS.pack [0, 1, 2, 255, 254, 253])
      decode (encode val) @?= Right val

  , testCase "deeply nested lists" $ do
      let val = BList (V.singleton (BList (V.singleton (BList (V.singleton (BInteger 42))))))
      decode (encode val) @?= Right val

  , testCase "generic record roundtrip" $ do
      let rec = SimpleRec "Alice" 30
          encoded = encodeBencode rec
      decodeBencode encoded @?= Right rec
  ]

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
