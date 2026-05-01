{-# LANGUAGE OverloadedStrings #-}
module Test.Iceberg.Sort (tests) where

import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import qualified Avro.Value as AV
import Iceberg.Sort
import Iceberg.Types

mkSF :: Int -> StructField
mkSF i = StructField i "id" True TLong Nothing Nothing Nothing

schema :: Schema
schema = Schema 0 (V.singleton (mkSF 1)) V.empty

ascOrder :: SortOrder
ascOrder = SortOrder 1 (V.singleton
  (SortField { sortSourceId = 1, sortTransform = Identity
             , sortDirection = Asc, sortNullOrder = NullsLast }))

descOrder :: SortOrder
descOrder = SortOrder 2 (V.singleton
  (SortField { sortSourceId = 1, sortTransform = Identity
             , sortDirection = Desc, sortNullOrder = NullsLast }))

tests :: TestTree
tests = testGroup "Iceberg.Sort"
  [ testCase "ASC keys compare numerically" $ do
      Right a <- pure $ buildSortKey ascOrder schema (\_ -> Just (AV.Long 1))
      Right b <- pure $ buildSortKey ascOrder schema (\_ -> Just (AV.Long 2))
      compareSortKeys a b @?= LT
      compareSortKeys b a @?= GT

  , testCase "DESC inverts the order" $ do
      Right a <- pure $ buildSortKey descOrder schema (\_ -> Just (AV.Long 1))
      Right b <- pure $ buildSortKey descOrder schema (\_ -> Just (AV.Long 2))
      compareSortKeys a b @?= GT

  , testCase "NullsLast: null > non-null in ASC order" $ do
      Right a <- pure $ buildSortKey ascOrder schema (\_ -> Just (AV.Long 1))
      Right b <- pure $ buildSortKey ascOrder schema (\_ -> Nothing)
      compareSortKeys a b @?= LT
  ]
