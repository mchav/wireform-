-- | Tests for "Iceberg.Geometry" — WKB POINT encode/decode used as the
-- single-value bounds for V3 geometry / geography columns.
module Test.Iceberg.Geometry (tests) where

import qualified Data.ByteString as BS
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.Geometry

tests :: TestTree
tests = testGroup "Iceberg.Geometry"
  [ testCase "wkbEncodePoint produces 21 bytes" $ do
      BS.length (wkbEncodePoint (Point 1 2)) @?= 21

  , testCase "WKB POINT round-trip (origin)" $ do
      let p   = Point 0 0
          bs  = wkbEncodePoint p
      wkbDecodePoint bs @?= Right p

  , testCase "WKB POINT round-trip (lon/lat)" $ do
      let p  = Point (-122.4194) 37.7749  -- San Francisco
          bs = wkbEncodePoint p
      case wkbDecodePoint bs of
        Right p' -> p' @?= p
        Left e   -> assertFailure e

  , testCase "WKB header bytes are spec-correct" $ do
      let bs = wkbEncodePoint (Point 1 2)
      -- byte 0: little-endian flag = 1
      BS.index bs 0 @?= 1
      -- bytes 1..4: WKB type = 1 (POINT) in little-endian
      BS.index bs 1 @?= 1
      BS.index bs 2 @?= 0
      BS.index bs 3 @?= 0
      BS.index bs 4 @?= 0

  , testCase "WKB POINT decode rejects truncated input" $ do
      case wkbDecodePoint (BS.replicate 10 0x00) of
        Left _  -> pure ()
        Right _ -> assertFailure "expected truncation error"

  , testCase "WKB POINT decode rejects unknown geometry type" $ do
      let badType = BS.pack
            [ 1                   -- LE
            , 2, 0, 0, 0          -- type = 2 (LineString), not POINT
            , 0,0,0,0,0,0,0,0
            , 0,0,0,0,0,0,0,0
            ]
      case wkbDecodePoint badType of
        Left _  -> pure ()
        Right _ -> assertFailure "expected unknown-type error"

  , testCase "Big-endian WKB POINT also decodes" $ do
      -- Same point as Point 1 2 but byte-order = 0 (BE).
      let beBytes = BS.pack
            [ 0
            , 0, 0, 0, 1                     -- type = 1, BE
            , 0x3F, 0xF0, 0,0,0,0,0,0        -- 1.0 BE
            , 0x40, 0x00, 0,0,0,0,0,0        -- 2.0 BE
            ]
      case wkbDecodePoint beBytes of
        Right p -> p @?= Point 1.0 2.0
        Left e  -> assertFailure e
  ]
