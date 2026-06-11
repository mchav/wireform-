{-# LANGUAGE OverloadedStrings #-}

module Test.Iceberg.MetricsConfig (tests) where

import Data.Map.Strict qualified as Map
import Iceberg.MetricsConfig
import Test.Syd


tests :: Spec
tests =
  describe "Iceberg.MetricsConfig" $
    sequence_
      [ it "parseMetricsMode handles every spec value" $ do
          parseMetricsMode "none" `shouldBe` Just MetricsNone
          parseMetricsMode "counts" `shouldBe` Just MetricsCounts
          parseMetricsMode "full" `shouldBe` Just MetricsFull
          parseMetricsMode "truncate(8)" `shouldBe` Just (MetricsTruncate 8)
          parseMetricsMode "garbage" `shouldBe` Nothing
      , it "metricsModeForColumn picks per-column override first" $ do
          let props =
                Map.fromList
                  [ ("write.metadata.metrics.default", "none")
                  , ("write.metadata.metrics.column.id", "full")
                  ]
          metricsModeForColumn props "id" `shouldBe` MetricsFull
          metricsModeForColumn props "name" `shouldBe` MetricsNone
      , it "metricsModeForColumn falls back to defaultMetricsMode" $
          metricsModeForColumn Map.empty "x" `shouldBe` defaultMetricsMode
      ]
