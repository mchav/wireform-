{-# LANGUAGE OverloadedStrings #-}

{- | Iceberg per-column metrics modes, controlling how much per-column
statistics writers emit into each manifest entry.

Iceberg supports four modes per column:

- @none@        — no value/null/NaN counts and no bounds
- @counts@      — value, null, NaN counts; no bounds
- @truncate(N)@ — counts plus min/max bounds truncated to @N@ characters
- @full@        — counts plus full min/max bounds

Modes are configured via the @write.metadata.metrics.column.&lt;name&gt;@
table properties (overriding the table-wide default
@write.metadata.metrics.default@).
-}
module Iceberg.MetricsConfig (
  MetricsMode (..),
  parseMetricsMode,
  metricsModeForColumn,
  defaultMetricsMode,
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR


data MetricsMode
  = MetricsNone
  | MetricsCounts
  | MetricsTruncate !Int
  | MetricsFull
  deriving (Show, Eq)


-- | Parse an Iceberg metrics-mode string. Unknown strings yield 'Nothing'.
parseMetricsMode :: Text -> Maybe MetricsMode
parseMetricsMode raw = case T.toLower (T.strip raw) of
  "none" -> Just MetricsNone
  "counts" -> Just MetricsCounts
  "full" -> Just MetricsFull
  other
    | "truncate(" `T.isPrefixOf` other && ")" `T.isSuffixOf` other ->
        case TR.decimal (T.dropEnd 1 (T.drop (T.length "truncate(") other)) of
          Right (n, rest) | T.null rest -> Just (MetricsTruncate n)
          _ -> Nothing
    | otherwise -> Nothing


-- | The Iceberg default mode is @truncate(16)@ for primitive columns.
defaultMetricsMode :: MetricsMode
defaultMetricsMode = MetricsTruncate 16


{- | Resolve a column's effective metrics mode from the table property map,
consulting first the per-column override and then the table-wide default.
-}
metricsModeForColumn :: Map.Map Text Text -> Text -> MetricsMode
metricsModeForColumn props column =
  let columnKey = "write.metadata.metrics.column." <> column
  in case Map.lookup columnKey props >>= parseMetricsMode of
       Just m -> m
       Nothing ->
         case Map.lookup "write.metadata.metrics.default" props >>= parseMetricsMode of
           Just m -> m
           Nothing -> defaultMetricsMode
