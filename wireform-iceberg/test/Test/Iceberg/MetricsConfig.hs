{-# LANGUAGE OverloadedStrings #-}
module Test.Iceberg.MetricsConfig (tests) where

import qualified Data.Map.Strict as Map
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.MetricsConfig

tests :: TestTree
tests = testGroup "Iceberg.MetricsConfig"
  [ testCase "parseMetricsMode handles every spec value" $ do
      parseMetricsMode "none"        @?= Just MetricsNone
      parseMetricsMode "counts"      @?= Just MetricsCounts
      parseMetricsMode "full"        @?= Just MetricsFull
      parseMetricsMode "truncate(8)" @?= Just (MetricsTruncate 8)
      parseMetricsMode "garbage"     @?= Nothing

  , testCase "metricsModeForColumn picks per-column override first" $ do
      let props = Map.fromList
            [ ("write.metadata.metrics.default", "none")
            , ("write.metadata.metrics.column.id", "full")
            ]
      metricsModeForColumn props "id"   @?= MetricsFull
      metricsModeForColumn props "name" @?= MetricsNone

  , testCase "metricsModeForColumn falls back to defaultMetricsMode" $
      metricsModeForColumn Map.empty "x" @?= defaultMetricsMode
  ]
