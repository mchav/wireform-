{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Parquet.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import qualified Parquet.Nested as PN
import qualified Parquet.Types as P
import Parquet.Derive
  ( fromParquetRow
  , parquetSchemaFor
  , toParquetRow
  )

import Test.Parquet.Derive.Instances (sumTypeDeriveSucceeded)
import Test.Parquet.Derive.Types

tests :: TestTree
tests = testGroup "Parquet.Derive"
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

schemaTests :: TestTree
schemaTests = testGroup "schema"
  [ testCase "synthetic root + one leaf per field" $ do
      V.length saleSchema @?= 4
      P.seName        (V.unsafeIndex saleSchema 0) @?= "schema"
      P.seNumChildren (V.unsafeIndex saleSchema 0) @?= Just 3

  , testCase "rename modifier reflected on schema element name" $ do
      P.seName (V.unsafeIndex saleSchema 1) @?= "amount"
      P.seName (V.unsafeIndex saleSchema 2) @?= "product"

  , testCase "absent rename falls back to backend's snake_case style" $
      P.seName (V.unsafeIndex saleSchema 3) @?= "sale_region"

  , testCase "amount -> INT64 required" $ do
      let se = V.unsafeIndex saleSchema 1
      P.seType       se @?= Just P.PTInt64
      P.seRepetition se @?= Just P.Required

  , testCase "product -> BYTE_ARRAY/UTF8 required" $ do
      let se = V.unsafeIndex saleSchema 2
      P.seType          se @?= Just P.PTByteArray
      P.seConvertedType se @?= Just P.CTUtf8
      P.seRepetition    se @?= Just P.Required

  , testCase "Maybe Text region -> BYTE_ARRAY/UTF8 optional" $ do
      let se = V.unsafeIndex saleSchema 3
      P.seType          se @?= Just P.PTByteArray
      P.seConvertedType se @?= Just P.CTUtf8
      P.seRepetition    se @?= Just P.Optional
  ]

-- ---------------------------------------------------------------------------
-- Per-row codec
-- ---------------------------------------------------------------------------

rowTests :: TestTree
rowTests = testGroup "row"
  [ testCase "Just region produces three Just leaves" $ do
      let s   = Sale 100 "widget" (Just "us-east")
          row = toParquetRow s
      V.length row @?= 3
      V.unsafeIndex row 0 @?= Just (PN.LvInt64  100)
      V.unsafeIndex row 1 @?= Just (PN.LvString "widget")
      V.unsafeIndex row 2 @?= Just (PN.LvString "us-east")

  , testCase "Nothing region produces a Nothing slot" $ do
      let s   = Sale 7 "gizmo" Nothing
          row = toParquetRow s
      V.unsafeIndex row 2 @?= Nothing

  , testCase "round-trip Sale through toParquetRow/fromParquetRow" $ do
      let s = Sale 0xCAFE "wrench" (Just "eu-west")
      case fromParquetRow (toParquetRow s) of
        Right s' -> s' @?= s
        Left  e  -> assertFailure e

  , testCase "round-trip Sale with Nothing region" $ do
      let s = Sale 1 "spanner" Nothing
      case fromParquetRow (toParquetRow s) of
        Right s' -> s' @?= s
        Left  e  -> assertFailure e

  , testCase "fromParquetRow rejects wrong leaf count" $
      case (fromParquetRow (V.singleton (Just (PN.LvInt64 1)))
              :: Either String Sale) of
        Left _  -> pure ()
        Right s -> assertFailure ("unexpected success: " ++ show s)

  , testCase "fromParquetRow rejects null in required column" $
      case (fromParquetRow (V.fromList
              [ Nothing
              , Just (PN.LvString "x")
              , Nothing
              ]) :: Either String Sale) of
        Left _  -> pure ()
        Right s -> assertFailure ("unexpected success: " ++ show s)
  ]

-- ---------------------------------------------------------------------------
-- coerced newtype roundtrip
-- ---------------------------------------------------------------------------

coercedTests :: TestTree
coercedTests = testGroup "coerced"
  [ testCase "Order schema picks up Int64 from coerced ''Int64" $ do
      V.length orderSchema @?= 2
      let leaf = V.unsafeIndex orderSchema 1
      P.seType       leaf @?= Just P.PTInt64
      P.seRepetition leaf @?= Just P.Required

  , testCase "Order encodes via the underlying Int64 representation" $ do
      let o   = Order (OrderId 42)
          row = toParquetRow o
      V.length row @?= 1
      V.unsafeIndex row 0 @?= Just (PN.LvInt64 42)

  , testCase "Order round-trips through the coerced leaf" $ do
      let o = Order (OrderId 0xDEAD)
      case fromParquetRow (toParquetRow o) of
        Right o' -> o' @?= o
        Left  e  -> assertFailure e
  ]

-- ---------------------------------------------------------------------------
-- Splice-time refusal of sum types
-- ---------------------------------------------------------------------------

spliceTests :: TestTree
spliceTests = testGroup "splice-time"
  [ testCase "deriveParquet refuses sum types" $
      assertBool "deriveParquet ''Color must fail at splice time"
        (not sumTypeDeriveSucceeded)
  ]
