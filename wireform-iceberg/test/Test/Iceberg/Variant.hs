{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the Iceberg V3 / Spark Variant binary encoding.
module Test.Iceberg.Variant (tests) where

import qualified Data.Aeson as Aeson
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.Variant

tests :: TestTree
tests = testGroup "Iceberg.Variant"
  [ testCase "primitive scalars round-trip" $ do
      let cases =
            [ VNull, VBool True, VBool False
            , VInt8 (-1), VInt8 127, VInt8 (-128)
            , VInt16 1234, VInt16 (-32000)
            , VInt32 100000, VInt32 (-1000000)
            , VInt64 1000000000000, VInt64 (-9000000000000)
            , VFloat 1.5, VFloat (-2.25)
            , VDouble 3.14159, VDouble (-1e9)
            , VString "alpha"
            , VString "longer text that exceeds the 64-byte short string optimisation - aaaaaaaaaaaaaaaaa"
            , VBinary (BS.pack [0, 1, 2, 0xff])
            ]
      mapM_ (\v ->
                let (m, x) = encodeVariant v
                 in case decodeVariant m x of
                      Right v' -> v' @?= v
                      Left  e  -> assertFailure ("primitive: " ++ e
                                                  ++ " (input " ++ show v ++ ")"))
            cases

  , testCase "short string optimisation triggers for < 64 bytes" $ do
      let (_, val) = encodeVariant (VString "hi")
      -- value_metadata byte: basic_type=1 (short string), length=2.
      -- (2 << 2) | 1 == 9.
      BS.head val @?= 9

  , testCase "object preserves keys + values" $ do
      let obj = VObject (Map.fromList
                          [ ("a", VInt32 1)
                          , ("b", VString "hello")
                          , ("c", VBool True)
                          ])
          (m, x) = encodeVariant obj
      case decodeVariant m x of
        Right v -> v @?= obj
        Left e  -> assertFailure e

  , testCase "array preserves order" $ do
      let arr = VArray (V.fromList [VInt32 1, VInt32 2, VInt32 3, VString "x"])
          (m, x) = encodeVariant arr
      case decodeVariant m x of
        Right v -> v @?= arr
        Left e  -> assertFailure e

  , testCase "deeply nested structure round-trips" $ do
      let nested = VObject (Map.fromList
            [ ("name", VString "alice")
            , ("scores", VArray (V.fromList
                          [ VObject (Map.fromList [("subject", VString "math"),  ("score", VInt32 95)])
                          , VObject (Map.fromList [("subject", VString "lit"),   ("score", VInt32 88)])
                          ]))
            , ("active", VBool True)
            , ("notes", VNull)
            ])
          (m, x) = encodeVariant nested
      case decodeVariant m x of
        Right v -> v @?= nested
        Left  e -> assertFailure e

  , testCase "metadata header carries version 1 + sorted_strings flag" $ do
      let (m, _) = encodeVariant
                     (VObject (Map.fromList [("z", VNull), ("a", VNull)]))
          !hdr   = fromIntegral (BS.head m) :: Int
      (hdr .&. 0x0F)         @?= 1
      ((hdr `shiftR` 4) .&. 0x01) @?= 1

  , testCase "JSON ↔ Variant round-trip" $ do
      let j = Aeson.object
                [ ("name",   Aeson.String "alice")
                , ("age",    Aeson.Number 30)
                , ("scores", Aeson.Array (V.fromList
                              [ Aeson.Number 95, Aeson.Number 88, Aeson.Null ]))
                , ("active", Aeson.Bool True)
                ]
          v = variantFromJSON j
          (m, x) = encodeVariant v
      case decodeVariant m x of
        Right v' -> do
          v' @?= v
          variantToJSON v' @?= j
        Left e -> assertFailure e

  , testCase "empty array + empty object" $ do
      let (m1, v1) = encodeVariant (VArray V.empty)
      case decodeVariant m1 v1 of
        Right v -> v @?= VArray V.empty
        Left  e -> assertFailure ("empty array: " ++ e)
      let (m2, v2) = encodeVariant (VObject Map.empty)
      case decodeVariant m2 v2 of
        Right v -> v @?= VObject Map.empty
        Left  e -> assertFailure ("empty object: " ++ e)

  , testCase "decimal4 / decimal8 / decimal16 round-trip" $ do
      let cases =
            [ VDecimal4 0 0
            , VDecimal4 2 1234           -- 12.34
            , VDecimal4 9 999999999      -- 0.999999999
            , VDecimal4 0 (minBound :: Int32)
            , VDecimal4 0 (maxBound :: Int32)
            , VDecimal8 4 1234567890      -- 123456.7890
            , VDecimal8 0 (minBound :: Int64)
            , VDecimal8 0 (maxBound :: Int64)
            -- 30-digit unscaled magnitude exercises the int128 path.
            , VDecimal16 5 (10 ^ (30 :: Int) + 7)
            , VDecimal16 8 (negate (10 ^ (35 :: Int) + 1))
            , VDecimal16 0 0
            ]
      mapM_ roundTrip cases

  , testCase "date / time / timestamp{,Ntz}{,Nanos} round-trip" $ do
      let cases =
            [ VDate 0
            , VDate 19000        -- 2022-01-08 ish
            , VDate (-1)         -- before epoch
            , VTime 0
            , VTime 12345678
            , VTime (-1)         -- negative not really legal but encoder is total
            , VTimestamp 0
            , VTimestamp 1700000000000000   -- ~2023-11-14
            , VTimestamp (-1)
            , VTimestampNtz 1700000000000000
            , VTimestampNanos 1700000000000000000
            , VTimestampNtzNanos 1700000000000000000
            ]
      mapM_ roundTrip cases

  , testCase "uuid round-trip preserves all 16 bytes" $ do
      let bytes = BS.pack [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08
                          ,0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10]
      roundTrip (VUuid bytes)

  , testCase "decimal4 binary layout: 1-byte scale + LE int32 unscaled" $ do
      let (_, val) = encodeVariant (VDecimal4 2 1234)
      -- value_metadata: basic_type=0, primitive_header=8 -> (8 << 2) | 0 = 0x20.
      -- payload: scale(1) + int32 LE (4)
      BS.unpack val @?= [0x20, 0x02, 0xD2, 0x04, 0x00, 0x00]

  , testCase "uuid binary layout: 16 raw bytes after primitive header" $ do
      let bytes = BS.pack [0..15]
          (_, val) = encodeVariant (VUuid bytes)
      -- primitive 20 -> (20 << 2) | 0 = 0x50.
      BS.head val           @?= 0x50
      BS.length val         @?= 17
      BS.tail val           @?= bytes

  , testCase "JSON projection: date/timestamp/uuid render canonical strings" $ do
      variantToJSON (VDate 0)
        @?= Aeson.String "1970-01-01"
      variantToJSON (VDate 19000)
        @?= Aeson.String "2022-01-08"
      -- 1700000000 seconds == 2023-11-14T22:13:20Z
      variantToJSON (VTimestamp 1700000000000000)
        @?= Aeson.String "2023-11-14T22:13:20Z"
      variantToJSON (VTimestampNtz 1700000000000000)
        @?= Aeson.String "2023-11-14T22:13:20"
      variantToJSON (VTime (3600 * 1000000 + 30 * 60 * 1000000 + 45 * 1000000))
        @?= Aeson.String "01:30:45"
      variantToJSON (VUuid (BS.pack
        [0x55,0x0e,0x84,0x00, 0xe2,0x9b, 0x41,0xd4
        ,0xa7,0x16, 0x44,0x66,0x55,0x44,0x00,0x00]))
        @?= Aeson.String "550e8400-e29b-41d4-a716-446655440000"

  , testCase "JSON projection: decimal renders canonical text" $ do
      variantToJSON (VDecimal4 2 1234)
        @?= Aeson.String "12.34"
      variantToJSON (VDecimal4 0 5)
        @?= Aeson.String "5"
      variantToJSON (VDecimal8 4 (-1234567890))
        @?= Aeson.String "-123456.7890"
      variantToJSON (VDecimal16 5 (10 ^ (30 :: Int)))
        @?= Aeson.String "10000000000000000000000000.00000"
  ]
  where
    roundTrip v =
      let (m, x) = encodeVariant v
       in case decodeVariant m x of
            Right v' -> v' @?= v
            Left  e  -> assertFailure ("round-trip " ++ show v ++ ": " ++ e)
