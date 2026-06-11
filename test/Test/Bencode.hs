{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Bencode (bencodeTests) where

import Bencode.Class
import Bencode.Decode
import Bencode.Encode
import Bencode.Value
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Test.Syd


bencodeTests :: Spec
bencodeTests =
  describe "Bencode" $
    sequence_
      [ valueTests
      , encodeTests
      , decodeTests
      , roundtripTests
      , classTests
      , genericTests
      , edgeCaseTests
      ]


valueTests :: Spec
valueTests =
  describe "Value types" $
    sequence_
      [ it "BString construction" $
          BString "hello" `shouldBe` BString "hello"
      , it "BInteger construction" $
          BInteger 42 `shouldBe` BInteger 42
      , it "BList construction" $
          BList (V.fromList [BInteger 1, BInteger 2])
            `shouldBe` BList (V.fromList [BInteger 1, BInteger 2])
      , it "BDict construction" $
          BDict (V.fromList [("key", BString "val")])
            `shouldBe` BDict (V.fromList [("key", BString "val")])
      ]


encodeTests :: Spec
encodeTests =
  describe "Encode" $
    sequence_
      [ it "encode string: 5:hello" $ do
          let result = encode (BString "hello")
          result `shouldBe` "5:hello"
      , it "encode empty string: 0:" $ do
          let result = encode (BString "")
          result `shouldBe` "0:"
      , it "encode integer: i42e" $ do
          let result = encode (BInteger 42)
          result `shouldBe` "i42e"
      , it "encode negative integer: i-3e" $ do
          let result = encode (BInteger (-3))
          result `shouldBe` "i-3e"
      , it "encode zero: i0e" $ do
          let result = encode (BInteger 0)
          result `shouldBe` "i0e"
      , it "encode list: l5:helloi42ee" $ do
          let result = encode (BList (V.fromList [BString "hello", BInteger 42]))
          result `shouldBe` "l5:helloi42ee"
      , it "encode empty list: le" $ do
          let result = encode (BList V.empty)
          result `shouldBe` "le"
      , it "encode dict" $ do
          let result = encode (BDict (V.fromList [("age", BInteger 30), ("name", BString "Alice")]))
          result `shouldBe` "d3:agei30e4:name5:Alicee"
      ]


decodeTests :: Spec
decodeTests =
  describe "Decode" $
    sequence_
      [ it "decode string: 5:hello" $ do
          let Right val = decode "5:hello"
          val `shouldBe` BString "hello"
      , it "decode integer: i42e" $ do
          let Right val = decode "i42e"
          val `shouldBe` BInteger 42
      , it "decode negative integer: i-3e" $ do
          let Right val = decode "i-3e"
          val `shouldBe` BInteger (-3)
      , it "decode list: l5:helloi42ee" $ do
          let Right val = decode "l5:helloi42ee"
          val `shouldBe` BList (V.fromList [BString "hello", BInteger 42])
      , it "decode dict" $ do
          let Right val = decode "d3:agei30e4:name5:Alicee"
          val `shouldBe` BDict (V.fromList [("age", BInteger 30), ("name", BString "Alice")])
      , it "decode empty list: le" $ do
          let Right val = decode "le"
          val `shouldBe` BList V.empty
      , it "decode empty dict: de" $ do
          let Right val = decode "de"
          val `shouldBe` BDict V.empty
      , it "decode error on empty input" $ do
          let result = decode ""
          (isLeft result) `shouldBe` True
      , it "decode error on trailing data" $ do
          let result = decode "i42eextra"
          (isLeft result) `shouldBe` True
      ]


roundtripTests :: Spec
roundtripTests =
  describe "Roundtrip" $
    sequence_
      [ it "string roundtrip" $ do
          let val = BString "hello world"
          decode (encode val) `shouldBe` Right val
      , it "integer roundtrip" $ do
          let val = BInteger 123456789
          decode (encode val) `shouldBe` Right val
      , it "list roundtrip" $ do
          let val = BList (V.fromList [BString "a", BInteger 1, BList V.empty])
          decode (encode val) `shouldBe` Right val
      , it "dict roundtrip" $ do
          let val =
                BDict
                  ( V.fromList
                      [ ("author", BString "Alice")
                      , ("title", BString "Book")
                      , ("year", BInteger 2024)
                      ]
                  )
          decode (encode val) `shouldBe` Right val
      , it "nested structure roundtrip" $ do
          let val =
                BDict
                  ( V.fromList
                      [
                        ( "files"
                        , BList
                            ( V.fromList
                                [ BDict (V.fromList [("length", BInteger 1024), ("path", BString "/file.txt")])
                                , BDict (V.fromList [("length", BInteger 2048), ("path", BString "/img.png")])
                                ]
                            )
                        )
                      , ("name", BString "torrent")
                      ]
                  )
          decode (encode val) `shouldBe` Right val
      ]


classTests :: Spec
classTests =
  describe "Class instances" $
    sequence_
      [ it "Int roundtrip via class" $ do
          let val = 42 :: Int
          fromBencode (toBencode val) `shouldBe` Right val
      , it "Text roundtrip via class" $ do
          let val = "hello" :: ByteString
          fromBencode (toBencode val) `shouldBe` Right val
      , it "Bool roundtrip via class" $ do
          fromBencode (toBencode True) `shouldBe` Right True
          fromBencode (toBencode False) `shouldBe` Right False
      , it "List roundtrip via class" $ do
          let val = [1, 2, 3] :: [Int]
          fromBencode (toBencode val) `shouldBe` Right val
      , it "Vector roundtrip via class" $ do
          let val = V.fromList [10, 20, 30] :: Vector Int
          fromBencode (toBencode val) `shouldBe` Right val
      , it "Maybe roundtrip via class" $ do
          let val = Just (42 :: Int)
          fromBencode (toBencode val) `shouldBe` Right val
          let nothing = Nothing :: Maybe Int
          fromBencode (toBencode nothing) `shouldBe` Right nothing
      ]


data TorrentInfo = TorrentInfo
  { name :: !ByteString
  , length_ :: !Int
  }
  deriving stock (Show, Eq, Generic)


instance ToBencode TorrentInfo where
  toBencode (TorrentInfo n l) =
    BDict
      ( V.fromList
          [("length_", toBencode l), ("name", toBencode n)]
      )


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


genericTests :: Spec
genericTests =
  describe "Generic deriving" $
    sequence_
      [ it "torrent info roundtrip" $ do
          let info = TorrentInfo "test.txt" 4096
              encoded = encodeBencode info
              Right decoded = decodeBencode encoded :: Either String TorrentInfo
          decoded `shouldBe` info
      ]


data SimpleRec = SimpleRec
  { sName :: !ByteString
  , sAge :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToBencode, FromBencode)


edgeCaseTests :: Spec
edgeCaseTests =
  describe "Edge cases" $
    sequence_
      [ it "dict key ordering preserved on decode" $ do
          let Right val = decode "d1:bi2e1:ai1ee"
          case val of
            BDict kvs ->
              V.toList (V.map fst kvs) `shouldBe` ["a", "b"]
            _ -> expectationFailure "expected BDict"
      , it "large integer" $ do
          let val = BInteger 999999999999999
          decode (encode val) `shouldBe` Right val
      , it "binary data in string" $ do
          let val = BString (BS.pack [0, 1, 2, 255, 254, 253])
          decode (encode val) `shouldBe` Right val
      , it "deeply nested lists" $ do
          let val = BList (V.singleton (BList (V.singleton (BList (V.singleton (BInteger 42))))))
          decode (encode val) `shouldBe` Right val
      , it "generic record roundtrip" $ do
          let rec = SimpleRec "Alice" 30
              encoded = encodeBencode rec
          decodeBencode encoded `shouldBe` Right rec
      ]


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
