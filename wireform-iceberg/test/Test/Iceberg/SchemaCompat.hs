{-# LANGUAGE OverloadedStrings #-}

module Test.Iceberg.SchemaCompat (tests) where

import Data.Vector qualified as V
import Iceberg.SchemaCompat
import Iceberg.Types
import Test.Syd


mkSF :: Int -> StructField
mkSF i = StructField i "x" True TInt Nothing Nothing Nothing


s :: V.Vector StructField -> Schema
s fs = Schema 0 fs V.empty


tests :: Spec
tests =
  describe "Iceberg.SchemaCompat" $
    sequence_
      [ it "Identical schemas are compatible" $
          validateEvolution (s (V.singleton (mkSF 1))) (s (V.singleton (mkSF 1)))
            `shouldBe` EvolutionOk
      , it "int -> long is allowed" $ do
          let old = s (V.singleton (mkSF 1))
              new = s (V.singleton (mkSF 1) {sfType = TLong})
          validateEvolution old new `shouldBe` EvolutionOk
      , it "int -> string is rejected" $ do
          let old = s (V.singleton (mkSF 1))
              new = s (V.singleton (mkSF 1) {sfType = TString})
          case validateEvolution old new of
            EvolutionOk -> expectationFailure "expected EvolutionErrors"
            EvolutionErrors _ -> pure ()
      , it "Adding a required field without default is rejected" $ do
          let old = s (V.singleton (mkSF 1))
              newField = StructField 2 "y" True TInt Nothing Nothing Nothing
              new = s (V.fromList [mkSF 1, newField])
          case validateEvolution old new of
            EvolutionOk -> expectationFailure "expected EvolutionErrors"
            EvolutionErrors _ -> pure ()
      , it "Adding an optional field is allowed" $ do
          let old = s (V.singleton (mkSF 1))
              newField = StructField 2 "y" False TInt Nothing Nothing Nothing
              new = s (V.fromList [mkSF 1, newField])
          validateEvolution old new `shouldBe` EvolutionOk
      , it "decimal precision widening is allowed" $ do
          isPromotionAllowed (TDecimal 5 2) (TDecimal 9 2) `shouldBe` True
      , it "decimal scale change is rejected" $ do
          isPromotionAllowed (TDecimal 5 2) (TDecimal 9 3) `shouldBe` False
      ]
