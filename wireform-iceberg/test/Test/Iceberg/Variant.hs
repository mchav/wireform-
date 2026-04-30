{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the Iceberg V3 / Spark Variant binary encoding.
module Test.Iceberg.Variant (tests) where

import qualified Data.Aeson as Aeson
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
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
  ]
