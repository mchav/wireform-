{-# LANGUAGE OverloadedStrings #-}
module Test.Iceberg.Expression (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Syd

import Iceberg.Expression
import Iceberg.SingleValue
import Iceberg.Types

mkSF :: Int -> Text -> IcebergType -> StructField
mkSF i n t = StructField i n True t Nothing Nothing Nothing

testSchema :: Schema
testSchema = Schema
  { schemaId = 0
  , schemaFields = V.fromList
      [ mkSF 1 "id"  TLong
      , mkSF 2 "name" TString
      ]
  , schemaIdentifierFieldIds = V.empty
  }

mkMetrics :: [(Int, (Maybe Integer, Maybe Integer))] -> [(Int, Integer)] -> [(Int, Integer)] -> FileMetrics
mkMetrics rngs valCounts nullCounts =
  let lows  = Map.fromList [ (k, encodeInt64 (fromIntegral lo)) | (k, (Just lo, _)) <- rngs ]
      highs = Map.fromList [ (k, encodeInt64 (fromIntegral hi)) | (k, (_, Just hi)) <- rngs ]
   in FileMetrics
        { fmRecordCount = 100
        , fmValueCounts  = Map.fromList [(k, fromIntegral v) | (k, v) <- valCounts]
        , fmNullCounts   = Map.fromList [(k, fromIntegral v) | (k, v) <- nullCounts]
        , fmNanCounts    = Map.empty
        , fmLowerBounds  = lows
        , fmUpperBounds  = highs
        }

tests :: Spec
tests = describe "Iceberg.Expression" $ sequence_
  [ it "id == 5 prunes a file whose id range is [10, 20]" $ do
      let fm = mkMetrics [(1, (Just 10, Just 20))] [(1, 100)] [(1, 0)]
          expr = equal "id" (LLong 5)
      evaluateInclusive testSchema fm expr `shouldBe` False

  , it "id == 15 keeps a file whose id range is [10, 20]" $ do
      let fm = mkMetrics [(1, (Just 10, Just 20))] [(1, 100)] [(1, 0)]
          expr = equal "id" (LLong 15)
      evaluateInclusive testSchema fm expr `shouldBe` True

  , it "id < 5 prunes a file whose id range starts at 10" $ do
      let fm = mkMetrics [(1, (Just 10, Just 20))] [(1, 100)] [(1, 0)]
          expr = lessThan "id" (LLong 5)
      evaluateInclusive testSchema fm expr `shouldBe` False

  , it "id < 25 keeps a file whose id range is [10, 20]" $ do
      let fm = mkMetrics [(1, (Just 10, Just 20))] [(1, 100)] [(1, 0)]
          expr = lessThan "id" (LLong 25)
      evaluateInclusive testSchema fm expr `shouldBe` True

  , it "isNull on a non-null column prunes" $ do
      let fm = mkMetrics [(1, (Just 10, Just 20))] [(1, 100)] [(1, 0)]
          expr = isNull "id"
      evaluateInclusive testSchema fm expr `shouldBe` False

  , it "Strict id < 25 holds for a file whose id range is [10, 20]" $ do
      let fm = mkMetrics [(1, (Just 10, Just 20))] [(1, 100)] [(1, 0)]
          expr = lessThan "id" (LLong 25)
      evaluateStrict testSchema fm expr `shouldBe` True

  , it "Boolean composition" $ do
      let fm = mkMetrics [(1, (Just 10, Just 20))] [(1, 100)] [(1, 0)]
          expr = (greaterThan "id" (LLong 5)) `and_` (lessThan "id" (LLong 25))
      evaluateInclusive testSchema fm expr `shouldBe` True
      evaluateStrict testSchema fm expr `shouldBe` True
  ]
