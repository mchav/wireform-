{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.ORC.Derive (tests) where

import qualified Data.Vector as V
import Language.Haskell.TH (recover)
import Language.Haskell.TH.Syntax (lift)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

import ORC.Derive
  ( FromORC (..)
  , HasORCSchema (..)
  , LeafValue (..)
  , ToORC (..)
  , deriveORC
  , orcSchemaFor
  )
import ORC.Types (ORCType (..), TypeKind (..))

import Test.ORC.Derive.Instances ()
import Test.ORC.Derive.Types

tests :: TestTree
tests = testGroup "ORC.Derive"
  [ schemaTests
  , rowCodecTests
  , coercedTests
  , spliceRejectionTests
  ]

-- ---------------------------------------------------------------------------
-- Schema
-- ---------------------------------------------------------------------------

schemaTests :: TestTree
schemaTests = testGroup "schema"
  [ testCase "root struct + leaves laid out in declaration order" $ do
      let sch = orcSchema (proxyOf (undefined :: Sale))
      -- Skipped fields are dropped from the schema entirely:
      -- saleOrderId / saleAmount / saleQty / saleNotes / saleActive
      -- (saleRegion is skipped under backendOrc).
      V.length sch @?= 6
      let root = V.head sch
      otKind root @?= TKStruct
      V.toList (otSubtypes root) @?= [1, 2, 3, 4, 5]

  , testCase "rename rewrites the column name in otFieldNames" $ do
      let sch = orcSchema (proxyOf (undefined :: Sale))
          root = V.head sch
          names = V.toList (otFieldNames root)
      -- Defaults to backendOrc's idiomatic SnakeCase for the
      -- non-renamed leaves; saleAmount is forced to "amount" by
      -- {-# ANN saleAmount (rename "amount") #-}.
      assertBool "amount column is the explicit rename"
        ("amount" `elem` names)
      assertBool "amount took precedence over snake_case"
        (notElem "sale_amount" names)
      -- The skipped saleRegion does not appear at all.
      assertBool "skipped saleRegion missing from schema"
        (notElem "sale_region" names)

  , testCase "leaf TypeKinds match field types (post-coerce)" $ do
      let sch  = orcSchema (proxyOf (undefined :: Sale))
          kinds = V.toList (V.map otKind (V.tail sch))
      -- saleOrderId -> coerced 'Int64 -> TKLong
      -- saleAmount  -> Double         -> TKDouble
      -- saleQty     -> Maybe Int32    -> TKInt
      -- saleNotes   -> Text           -> TKString
      -- saleActive  -> Bool           -> TKBoolean
      kinds @?= [TKLong, TKDouble, TKInt, TKString, TKBoolean]

  , testCase "orcSchemaFor splices the same vector as orcSchema" $ do
      let viaClass  = orcSchema (proxyOf (undefined :: Sale))
          viaSplice = $(orcSchemaFor ''Sale)
      viaSplice @?= viaClass
  ]

-- ---------------------------------------------------------------------------
-- Per-row codec
-- ---------------------------------------------------------------------------

rowCodecTests :: TestTree
rowCodecTests = testGroup "per-row codec"
  [ testCase "toORCRow projects to the right LeafValue constructors" $ do
      let s   = sampleSale
          row = toORCRow s
      V.length row @?= 5
      row V.! 0 @?= LVInt64 (Just 7)
      row V.! 1 @?= LVDouble (Just 12.5)
      row V.! 2 @?= LVInt32 (Just 3)
      row V.! 3 @?= LVText (Just "hello")
      row V.! 4 @?= LVBool (Just True)

  , testCase "Maybe field round-trips Nothing as a null cell" $ do
      let s   = sampleSale { saleQty = Nothing }
          row = toORCRow s
      row V.! 2 @?= LVInt32 Nothing

  , testCase "round-trip restores skipped saleRegion from defaults" $
      roundTrip sampleSale (\s' -> saleRegion s' @?= defaultRegion)

  , testCase "round-trip preserves all non-skipped fields" $
      roundTrip sampleSale $ \s' -> do
        saleOrderId s' @?= saleOrderId sampleSale
        saleAmount  s' @?= saleAmount  sampleSale
        saleQty     s' @?= saleQty     sampleSale
        saleNotes   s' @?= saleNotes   sampleSale
        saleActive  s' @?= saleActive  sampleSale
  ]

-- ---------------------------------------------------------------------------
-- Coerced newtype roundtrip
-- ---------------------------------------------------------------------------

coercedTests :: TestTree
coercedTests = testGroup "coerced newtype"
  [ testCase "saleOrderId encodes as the underlying Int64 leaf" $ do
      let s   = sampleSale { saleOrderId = OrderId 42 }
          row = toORCRow s
      row V.! 0 @?= LVInt64 (Just 42)

  , testCase "saleOrderId decodes back through the OrderId newtype" $ do
      let s = sampleSale { saleOrderId = OrderId 99 }
      case fromORCRow (toORCRow s) of
        Right s' -> saleOrderId s' @?= OrderId 99
        Left e   -> fail e
  ]

-- ---------------------------------------------------------------------------
-- Splice-time rejection
-- ---------------------------------------------------------------------------

-- | Run 'deriveORC' on the sum type 'Shape' inside a 'recover'
-- bracket. 'deriveORC' should call 'fail', so the splice resolves
-- to the literal "rejected".
shapeDeriveOutcome :: String
shapeDeriveOutcome =
  $(recover (lift ("rejected" :: String))
            (do _ <- deriveORC ''Shape
                lift ("succeeded" :: String)))

spliceRejectionTests :: TestTree
spliceRejectionTests = testGroup "splice-time rejection"
  [ testCase "deriveORC on a sum type fails at splice time" $
      shapeDeriveOutcome @?= "rejected"
  ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

sampleSale :: Sale
sampleSale = Sale
  { saleOrderId = OrderId 7
  , saleAmount  = 12.5
  , saleQty     = Just 3
  , saleNotes   = "hello"
  , saleActive  = True
  , saleRegion  = "EU-WEST"
  }

roundTrip :: Sale -> (Sale -> Assertion) -> Assertion
roundTrip s k = case fromORCRow (toORCRow s) of
  Right s' -> k s'
  Left e   -> fail e

-- | Type-restricted Proxy alias — keeps callers 'TypeApplications'-free.
proxyOf :: a -> Proxy a
proxyOf _ = Proxy

data Proxy a = Proxy
