{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Parquet.Derive (tests) where

import qualified Data.Vector as V
import Test.Syd

import qualified Parquet.Nested as PN
import qualified Parquet.Types as P
import Parquet.Derive
  ( fromParquetRow
  , parquetSchemaFor
  , toParquetRow
  )

import Test.Parquet.Derive.Instances (sumTypeDeriveSucceeded)
import Test.Parquet.Derive.Types

tests :: Spec
tests = describe "Parquet.Derive" $ sequence_
  [ schemaTests
  , rowTests
  , coercedTests
  , spliceTests
  ]

-- | Spliced once at compile time so the test functions can
-- pattern-match without re-running TH per case.
saleSchema :: V.Vector P.SchemaElement
saleSchema = $(parquetSchemaFor ''Sale)

orderSchema :: V.Vector P.SchemaElement
orderSchema = $(parquetSchemaFor ''Order)

-- ---------------------------------------------------------------------------
-- Schema
-- ---------------------------------------------------------------------------

schemaTests :: Spec
schemaTests = describe "schema" $ sequence_
  [ it "synthetic root + one leaf per field" $ do
      V.length saleSchema `shouldBe` 4
      P.seName        (V.unsafeIndex saleSchema 0) `shouldBe` "schema"
      P.seNumChildren (V.unsafeIndex saleSchema 0) `shouldBe` Just 3

  , it "rename modifier reflected on schema element name" $ do
      P.seName (V.unsafeIndex saleSchema 1) `shouldBe` "amount"
      P.seName (V.unsafeIndex saleSchema 2) `shouldBe` "product"

  , it "absent rename falls back to backend's snake_case style" $
      P.seName (V.unsafeIndex saleSchema 3) `shouldBe` "sale_region"

  , it "amount -> INT64 required" $ do
      let se = V.unsafeIndex saleSchema 1
      P.seType       se `shouldBe` Just P.PTInt64
      P.seRepetition se `shouldBe` Just P.Required

  , it "product -> BYTE_ARRAY/UTF8 required" $ do
      let se = V.unsafeIndex saleSchema 2
      P.seType          se `shouldBe` Just P.PTByteArray
      P.seConvertedType se `shouldBe` Just P.CTUtf8
      P.seRepetition    se `shouldBe` Just P.Required

  , it "Maybe Text region -> BYTE_ARRAY/UTF8 optional" $ do
      let se = V.unsafeIndex saleSchema 3
      P.seType          se `shouldBe` Just P.PTByteArray
      P.seConvertedType se `shouldBe` Just P.CTUtf8
      P.seRepetition    se `shouldBe` Just P.Optional
  ]

-- ---------------------------------------------------------------------------
-- Per-row codec
-- ---------------------------------------------------------------------------

rowTests :: Spec
rowTests = describe "row" $ sequence_
  [ it "Just region produces three Just leaves" $ do
      let s   = Sale 100 "widget" (Just "us-east")
          row = toParquetRow s
      V.length row `shouldBe` 3
      V.unsafeIndex row 0 `shouldBe` Just (PN.LvInt64  100)
      V.unsafeIndex row 1 `shouldBe` Just (PN.LvString "widget")
      V.unsafeIndex row 2 `shouldBe` Just (PN.LvString "us-east")

  , it "Nothing region produces a Nothing slot" $ do
      let s   = Sale 7 "gizmo" Nothing
          row = toParquetRow s
      V.unsafeIndex row 2 `shouldBe` Nothing

  , it "round-trip Sale through toParquetRow/fromParquetRow" $ do
      let s = Sale 0xCAFE "wrench" (Just "eu-west")
      case fromParquetRow (toParquetRow s) of
        Right s' -> s' `shouldBe` s
        Left  e  -> expectationFailure e

  , it "round-trip Sale with Nothing region" $ do
      let s = Sale 1 "spanner" Nothing
      case fromParquetRow (toParquetRow s) of
        Right s' -> s' `shouldBe` s
        Left  e  -> expectationFailure e

  , it "fromParquetRow rejects wrong leaf count" $
      case (fromParquetRow (V.singleton (Just (PN.LvInt64 1)))
              :: Either String Sale) of
        Left _  -> pure ()
        Right s -> expectationFailure ("unexpected success: " ++ show s)

  , it "fromParquetRow rejects null in required column" $
      case (fromParquetRow (V.fromList
              [ Nothing
              , Just (PN.LvString "x")
              , Nothing
              ]) :: Either String Sale) of
        Left _  -> pure ()
        Right s -> expectationFailure ("unexpected success: " ++ show s)
  ]

-- ---------------------------------------------------------------------------
-- coerced newtype roundtrip
-- ---------------------------------------------------------------------------

coercedTests :: Spec
coercedTests = describe "coerced" $ sequence_
  [ it "Order schema picks up Int64 from coerced ''Int64" $ do
      V.length orderSchema `shouldBe` 2
      let leaf = V.unsafeIndex orderSchema 1
      P.seType       leaf `shouldBe` Just P.PTInt64
      P.seRepetition leaf `shouldBe` Just P.Required

  , it "Order encodes via the underlying Int64 representation" $ do
      let o   = Order (OrderId 42)
          row = toParquetRow o
      V.length row `shouldBe` 1
      V.unsafeIndex row 0 `shouldBe` Just (PN.LvInt64 42)

  , it "Order round-trips through the coerced leaf" $ do
      let o = Order (OrderId 0xDEAD)
      case fromParquetRow (toParquetRow o) of
        Right o' -> o' `shouldBe` o
        Left  e  -> expectationFailure e
  ]

-- ---------------------------------------------------------------------------
-- Splice-time refusal of sum types
-- ---------------------------------------------------------------------------

spliceTests :: Spec
spliceTests = describe "splice-time" $ sequence_
  [ it "deriveParquet refuses sum types" $
      (not sumTypeDeriveSucceeded) `shouldBe` True
  ]
