{-# LANGUAGE OverloadedStrings #-}

module Test.Iceberg.Sort (tests) where

import Avro.Value qualified as AV
import Data.Vector qualified as V
import Iceberg.Sort
import Iceberg.Types
import Test.Syd


mkSF :: Int -> StructField
mkSF i = StructField i "id" True TLong Nothing Nothing Nothing


schema :: Schema
schema = Schema 0 (V.singleton (mkSF 1)) V.empty


ascOrder :: SortOrder
ascOrder =
  SortOrder
    1
    ( V.singleton
        ( SortField
            { sortSourceId = 1
            , sortTransform = Identity
            , sortDirection = Asc
            , sortNullOrder = NullsLast
            }
        )
    )


descOrder :: SortOrder
descOrder =
  SortOrder
    2
    ( V.singleton
        ( SortField
            { sortSourceId = 1
            , sortTransform = Identity
            , sortDirection = Desc
            , sortNullOrder = NullsLast
            }
        )
    )


tests :: Spec
tests =
  describe "Iceberg.Sort" $
    sequence_
      [ it "ASC keys compare numerically" $ do
          Right a <- pure $ buildSortKey ascOrder schema (\_ -> Just (AV.Long 1))
          Right b <- pure $ buildSortKey ascOrder schema (\_ -> Just (AV.Long 2))
          compareSortKeys a b `shouldBe` LT
          compareSortKeys b a `shouldBe` GT
      , it "DESC inverts the order" $ do
          Right a <- pure $ buildSortKey descOrder schema (\_ -> Just (AV.Long 1))
          Right b <- pure $ buildSortKey descOrder schema (\_ -> Just (AV.Long 2))
          compareSortKeys a b `shouldBe` GT
      , it "NullsLast: null > non-null in ASC order" $ do
          Right a <- pure $ buildSortKey ascOrder schema (\_ -> Just (AV.Long 1))
          Right b <- pure $ buildSortKey ascOrder schema (\_ -> Nothing)
          compareSortKeys a b `shouldBe` LT
      ]
