{-# LANGUAGE OverloadedStrings #-}
module Test.Bench (tests) where

import qualified Data.Aeson as A
import Data.Time.Clock (UTCTime (..))
import Data.Time.Calendar (fromGregorian)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Wireform.Stats.Bench as Bench
import qualified Wireform.Stats.SVG   as SVG
import qualified Wireform.Stats.Table as Tbl

tests :: TestTree
tests = testGroup "Bench"
  [ testCase "summary JSON round-trips" jsonRoundTrip
  , testCase "summaryToTable: header columns + row count"  tableShape
  , testCase "summaryToBarChart: series + groups preserved" chartShape
  ]

sampleSummary :: Bench.BenchSummary
sampleSummary = Bench.BenchSummary
  { Bench.bsId             = "cbor-vs-cborg-encode"
  , Bench.bsTitle          = "wireform-cbor vs cborg"
  , Bench.bsUnit           = Bench.Nanos
  , Bench.bsHigherIsBetter = False
  , Bench.bsGroups         = ["encode", "decode"]
  , Bench.bsSeries         =
      [ Bench.BenchSeries "wireform-cbor" [3200, 4700]
      , Bench.BenchSeries "cborg"         [4100, 5900]
      ]
  , Bench.bsBaseline       = Just "wireform-cbor"
  , Bench.bsCapturedAt     = UTCTime (fromGregorian 2026 5 13) 0
  , Bench.bsToolchain      = "ghc-9.8.4 on darwin-aarch64"
  }

jsonRoundTrip :: Assertion
jsonRoundTrip = do
  let bytes = A.encode sampleSummary
  case A.eitherDecode bytes of
    Right (back :: Bench.BenchSummary) -> back @?= sampleSummary
    Left  err                          -> assertFailure err

tableShape :: Assertion
tableShape = do
  let t = Bench.summaryToTable sampleSummary
  -- 4 header columns: Operation + 2 series + ratio.
  length (Tbl.tableHeader t) @?= 4
  -- 2 rows: one per group.
  length (Tbl.tableRows t)   @?= 2
  -- Each row has the same arity as the header.
  mapM_ (\r -> length r @?= 4) (Tbl.tableRows t)

chartShape :: Assertion
chartShape = do
  let c = Bench.summaryToBarChart sampleSummary
  length (SVG.chartGroups c) @?= 2
  length (SVG.chartSeries c) @?= 2
  SVG.chartUnit c @?= "ns"
  SVG.chartHigherIsBetter c @?= False
