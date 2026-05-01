{-# LANGUAGE OverloadedStrings #-}
module Test.Iceberg.SchemaCompat (tests) where

import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.SchemaCompat
import Iceberg.Types

mkSF :: Int -> StructField
mkSF i = StructField i "x" True TInt Nothing Nothing Nothing

s :: V.Vector StructField -> Schema
s fs = Schema 0 fs V.empty

tests :: TestTree
tests = testGroup "Iceberg.SchemaCompat"
  [ testCase "Identical schemas are compatible" $
      validateEvolution (s (V.singleton (mkSF 1))) (s (V.singleton (mkSF 1)))
        @?= EvolutionOk

  , testCase "int -> long is allowed" $ do
      let old = s (V.singleton (mkSF 1))
          new = s (V.singleton (mkSF 1) { sfType = TLong })
      validateEvolution old new @?= EvolutionOk

  , testCase "int -> string is rejected" $ do
      let old = s (V.singleton (mkSF 1))
          new = s (V.singleton (mkSF 1) { sfType = TString })
      case validateEvolution old new of
        EvolutionOk      -> assertFailure "expected EvolutionErrors"
        EvolutionErrors _ -> pure ()

  , testCase "Adding a required field without default is rejected" $ do
      let old = s (V.singleton (mkSF 1))
          newField = StructField 2 "y" True TInt Nothing Nothing Nothing
          new = s (V.fromList [mkSF 1, newField])
      case validateEvolution old new of
        EvolutionOk -> assertFailure "expected EvolutionErrors"
        EvolutionErrors _ -> pure ()

  , testCase "Adding an optional field is allowed" $ do
      let old = s (V.singleton (mkSF 1))
          newField = StructField 2 "y" False TInt Nothing Nothing Nothing
          new = s (V.fromList [mkSF 1, newField])
      validateEvolution old new @?= EvolutionOk

  , testCase "decimal precision widening is allowed" $ do
      isPromotionAllowed (TDecimal 5 2) (TDecimal 9 2) @?= True

  , testCase "decimal scale change is rejected" $ do
      isPromotionAllowed (TDecimal 5 2) (TDecimal 9 3) @?= False
  ]
