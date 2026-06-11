{- | Tests for "Iceberg.Geometry" — WKB POINT encode/decode used as the
single-value bounds for V3 geometry / geography columns.
-}
module Test.Iceberg.Geometry (tests) where

import Data.ByteString qualified as BS
import Iceberg.Geometry
import Test.Syd


tests :: Spec
tests =
  describe "Iceberg.Geometry" $
    sequence_
      [ it "wkbEncodePoint produces 21 bytes" $ do
          BS.length (wkbEncodePoint (Point 1 2)) `shouldBe` 21
      , it "WKB POINT round-trip (origin)" $ do
          let p = Point 0 0
              bs = wkbEncodePoint p
          wkbDecodePoint bs `shouldBe` Right p
      , it "WKB POINT round-trip (lon/lat)" $ do
          let p = Point (-122.4194) 37.7749 -- San Francisco
              bs = wkbEncodePoint p
          case wkbDecodePoint bs of
            Right p' -> p' `shouldBe` p
            Left e -> expectationFailure e
      , it "WKB header bytes are spec-correct" $ do
          let bs = wkbEncodePoint (Point 1 2)
          -- byte 0: little-endian flag = 1
          BS.index bs 0 `shouldBe` 1
          -- bytes 1..4: WKB type = 1 (POINT) in little-endian
          BS.index bs 1 `shouldBe` 1
          BS.index bs 2 `shouldBe` 0
          BS.index bs 3 `shouldBe` 0
          BS.index bs 4 `shouldBe` 0
      , it "WKB POINT decode rejects truncated input" $ do
          case wkbDecodePoint (BS.replicate 10 0x00) of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected truncation error"
      , it "WKB POINT decode rejects unknown geometry type" $ do
          let badType =
                BS.pack
                  [ 1 -- LE
                  , 2
                  , 0
                  , 0
                  , 0 -- type = 2 (LineString), not POINT
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  ]
          case wkbDecodePoint badType of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected unknown-type error"
      , it "Big-endian WKB POINT also decodes" $ do
          -- Same point as Point 1 2 but byte-order = 0 (BE).
          let beBytes =
                BS.pack
                  [ 0
                  , 0
                  , 0
                  , 0
                  , 1 -- type = 1, BE
                  , 0x3F
                  , 0xF0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0 -- 1.0 BE
                  , 0x40
                  , 0x00
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0
                  , 0 -- 2.0 BE
                  ]
          case wkbDecodePoint beBytes of
            Right p -> p `shouldBe` Point 1.0 2.0
            Left e -> expectationFailure e
      ]
