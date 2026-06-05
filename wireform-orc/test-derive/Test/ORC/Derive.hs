{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.ORC.Derive (tests) where

import qualified Data.Vector as V
import Language.Haskell.TH (recover)
import Language.Haskell.TH.Syntax (lift)
import Test.Syd

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

tests :: Spec
tests = describe "ORC.Derive" $ sequence_
  [ schemaTests
  , rowCodecTests
  , coercedTests
  , spliceRejectionTests
  ]

-- ---------------------------------------------------------------------------
-- Schema
-- ---------------------------------------------------------------------------

schemaTests :: Spec
schemaTests = describe "schema" $ sequence_
  [ it "root struct + leaves laid out in declaration order" $ do
      let sch = orcSchema (proxyOf (undefined :: Sale))
      -- Skipped fields are dropped from the schema entirely:
      -- saleOrderId / saleAmount / saleQty / saleNotes / saleActive
      -- (saleRegion is skipped under backendOrc).
      V.length sch `shouldBe` 6
      let root = V.head sch
      otKind root `shouldBe` TKStruct
      V.toList (otSubtypes root) `shouldBe` [1, 2, 3, 4, 5]

  , it "rename rewrites the column name in otFieldNames" $ do
      let sch = orcSchema (proxyOf (undefined :: Sale))
          root = V.head sch
          names = V.toList (otFieldNames root)
      -- Defaults to backendOrc's idiomatic SnakeCase for the
      -- non-renamed leaves; saleAmount is forced to "amount" by
      -- {-# ANN saleAmount (rename "amount") #-}.
      ("amount" `elem` names) `shouldBe` True
      (notElem "sale_amount" names) `shouldBe` True
      -- The skipped saleRegion does not appear at all.
      (notElem "sale_region" names) `shouldBe` True

  , it "leaf TypeKinds match field types (post-coerce)" $ do
      let sch  = orcSchema (proxyOf (undefined :: Sale))
          kinds = V.toList (V.map otKind (V.tail sch))
      -- saleOrderId -> coerced 'Int64 -> TKLong
      -- saleAmount  -> Double         -> TKDouble
      -- saleQty     -> Maybe Int32    -> TKInt
      -- saleNotes   -> Text           -> TKString
      -- saleActive  -> Bool           -> TKBoolean
      kinds `shouldBe` [TKLong, TKDouble, TKInt, TKString, TKBoolean]

  , it "orcSchemaFor splices the same vector as orcSchema" $ do
      let viaClass  = orcSchema (proxyOf (undefined :: Sale))
          viaSplice = $(orcSchemaFor ''Sale)
      viaSplice `shouldBe` viaClass
  ]

-- ---------------------------------------------------------------------------
-- Per-row codec
-- ---------------------------------------------------------------------------

rowCodecTests :: Spec
rowCodecTests = describe "per-row codec" $ sequence_
  [ it "toORCRow projects to the right LeafValue constructors" $ do
      let s   = sampleSale
          row = toORCRow s
      V.length row `shouldBe` 5
      row V.! 0 `shouldBe` LVInt64 (Just 7)
      row V.! 1 `shouldBe` LVDouble (Just 12.5)
      row V.! 2 `shouldBe` LVInt32 (Just 3)
      row V.! 3 `shouldBe` LVText (Just "hello")
      row V.! 4 `shouldBe` LVBool (Just True)

  , it "Maybe field round-trips Nothing as a null cell" $ do
      let s   = sampleSale { saleQty = Nothing }
          row = toORCRow s
      row V.! 2 `shouldBe` LVInt32 Nothing

  , it "round-trip restores skipped saleRegion from defaults" $
      roundTrip sampleSale (\s' -> saleRegion s' `shouldBe` defaultRegion)

  , it "round-trip preserves all non-skipped fields" $
      roundTrip sampleSale $ \s' -> do
        saleOrderId s' `shouldBe` saleOrderId sampleSale
        saleAmount  s' `shouldBe` saleAmount  sampleSale
        saleQty     s' `shouldBe` saleQty     sampleSale
        saleNotes   s' `shouldBe` saleNotes   sampleSale
        saleActive  s' `shouldBe` saleActive  sampleSale
  ]

-- ---------------------------------------------------------------------------
-- Coerced newtype roundtrip
-- ---------------------------------------------------------------------------

coercedTests :: Spec
coercedTests = describe "coerced newtype" $ sequence_
  [ it "saleOrderId encodes as the underlying Int64 leaf" $ do
      let s   = sampleSale { saleOrderId = OrderId 42 }
          row = toORCRow s
      row V.! 0 `shouldBe` LVInt64 (Just 42)

  , it "saleOrderId decodes back through the OrderId newtype" $ do
      let s = sampleSale { saleOrderId = OrderId 99 }
      case fromORCRow (toORCRow s) of
        Right s' -> saleOrderId s' `shouldBe` OrderId 99
        Left e   -> expectationFailure e
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

spliceRejectionTests :: Spec
spliceRejectionTests = describe "splice-time rejection" $ sequence_
  [ it "deriveORC on a sum type fails at splice time" $
      shapeDeriveOutcome `shouldBe` "rejected"
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

roundTrip :: Sale -> (Sale -> IO ()) -> IO ()
roundTrip s k = case fromORCRow (toORCRow s) of
  Right s' -> k s'
  Left e   -> expectationFailure e

-- | Type-restricted Proxy alias — keeps callers 'TypeApplications'-free.
proxyOf :: a -> Proxy a
proxyOf _ = Proxy

data Proxy a = Proxy
