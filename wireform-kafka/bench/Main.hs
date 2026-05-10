{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Benchmark suite entry point
Copyright   : (c) 2025
License     : BSD-3-Clause

Main entry point for the kafka-native benchmark suite. Configures criterion
to output JSON results for historical tracking and HTML reports for visualization.

Usage:
  cabal bench
  cabal bench --benchmark-options="--output benchmark/results/report.html"
  cabal bench --benchmark-options="--quick"
  cabal bench --benchmark-options="--list"
-}
module Main (main) where

import Criterion.Main (defaultMainWith, defaultConfig)
import Criterion.Types
  ( Config(..)
  , Verbosity(..)
  , measureAccessors
  , measureKeys
  )
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.FilePath ((</>))
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import qualified Benchmarks.CRC32C as CRC32C
import qualified Benchmarks.Serialization as Serialization
import qualified Benchmarks.ClientOps as ClientOps
import qualified Benchmarks.StatsAndStamping as StatsAndStamping
import qualified Benchmarks.HotPath as HotPath
import qualified Benchmarks.HwKafkaComparison as HwKafkaComparison

-- -----------------------------------------------------------------------------
-- Main Entry Point
-- -----------------------------------------------------------------------------

main :: IO ()
main = do
  -- Ensure benchmark results directory exists
  resultsDir <- getResultsDir
  createDirectoryIfMissing True resultsDir
  
  -- Get timestamp for file naming
  timestamp <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" timestamp
  
  -- Configure criterion with JSON and HTML output
  let config = defaultConfig
        { -- Output JSON results for historical tracking
          jsonFile = Just $ resultsDir </> ("benchmark-" ++ timeStr ++ ".json")
          
          -- Can also generate CSV for easier analysis
        , csvFile = Just $ resultsDir </> ("benchmark-" ++ timeStr ++ ".csv")
          
          -- Enable verbose output to see progress
        , verbosity = Normal
          
          -- Configure reporting
        , reportFile = Just $ resultsDir </> ("benchmark-" ++ timeStr ++ ".html")
        }
  
  -- Run all benchmarks
  defaultMainWith config
    [ CRC32C.benchmarks
    , Serialization.benchmarks
    , ClientOps.benchmarks
    , StatsAndStamping.benchmarks
    , HotPath.benchmarks
    , HwKafkaComparison.benchmarks
    ]

-- -----------------------------------------------------------------------------
-- Utilities
-- -----------------------------------------------------------------------------

-- | Get the path to the benchmark results directory.
-- Uses "benchmark/results" relative to the current working directory.
getResultsDir :: IO FilePath
getResultsDir = do
  cwd <- getCurrentDirectory
  return $ cwd </> "benchmark" </> "results"

{- NOTES ON CRITERION CONFIGURATION

Criterion provides many configuration options. Some useful ones:

1. Time Limits:
   - timeLimit :: Double -- Total time to run benchmarks (seconds)
   - Set via --time-limit flag

2. Sampling:
   - resamples :: Int -- Number of bootstrap resamples (default 1000)
   - Set via --resamples flag

3. Quick Mode:
   - Use --quick flag for faster but less accurate results
   - Good for development/iteration

4. Filtering:
   - Use -p/--pattern to filter benchmarks by name
   - Example: --pattern "CRC32C/Medium"

5. Listing:
   - Use --list to see all available benchmarks
   - Useful for selecting specific benchmarks

6. Output Control:
   - --no-json, --no-html to skip output generation
   - --output FILE for custom HTML output location

7. Statistical Options:
   - --regression ANALYSIS (least-squares, L2 norm)
   - --ci CI (confidence interval, default 0.95)

8. Verbosity:
   - --quiet, --normal, --verbose

Example command lines:

  # Quick benchmark for development
  cabal bench --benchmark-options="--quick"
  
  # Only run CRC32C benchmarks
  cabal bench --benchmark-options="--pattern CRC32C"
  
  # Generate HTML report to specific location
  cabal bench --benchmark-options="--output my-report.html"
  
  # Run with more samples for publication
  cabal bench --benchmark-options="--resamples 10000"
  
  # List all benchmarks
  cabal bench --benchmark-options="--list"
  
  # Combine options
  cabal bench --benchmark-options="--quick --pattern CRC32C/Small"

COMPARING RESULTS OVER TIME:

To compare benchmark results over time:

1. Run benchmarks periodically and commit JSON results to git
2. Use criterion's comparison tools:
   - criterion-cmp tool (if available)
   - Manual comparison of JSON files
   
3. Build custom analysis tools:
   - Parse JSON outputs
   - Track specific metrics over time
   - Generate trend graphs
   
4. Consider using bench-show or similar tools for visualization
   
5. For CI integration:
   - Compare against baseline
   - Fail if performance regresses beyond threshold
   - Track performance trends

Example analysis workflow:

  # Run benchmarks
  cabal bench
  
  # Compare with previous run
  jq '.results[].mean.estPoint' benchmark/results/benchmark-20250103-100000.json
  jq '.results[].mean.estPoint' benchmark/results/benchmark-20250103-110000.json
  
  # Or use criterion's built-in comparison if you have baseline
  cabal bench --benchmark-options="--baseline baseline.csv"
-}

