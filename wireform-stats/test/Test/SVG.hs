{-# LANGUAGE OverloadedStrings #-}
module Test.SVG (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Test.Tasty
import Test.Tasty.HUnit

import qualified Wireform.Stats.SVG as SVG

tests :: TestTree
tests = testGroup "SVG"
  [ testCase "renders a non-trivial chart" rendersNontrivial
  , testCase "light + dark differ"          lightDarkDiffer
  , testCase "single-series chart works"    singleSeries
  , testCase "empty chart still emits an svg root" emptyChart
  ]

sampleChart :: SVG.BarChart
sampleChart = SVG.BarChart
  { SVG.chartTitle    = "wireform-cbor vs cborg"
  , SVG.chartSubtitle = Just "ghc-9.8.4 on darwin-aarch64"
  , SVG.chartUnit     = "ns"
  , SVG.chartGroups   = ["encode", "decode"]
  , SVG.chartSeries   =
      [ SVG.Series "wireform-cbor" [3200, 4700]
      , SVG.Series "cborg"         [4100, 5900]
      ]
  , SVG.chartHigherIsBetter = False
  }

rendersNontrivial :: Assertion
rendersNontrivial = do
  let svg = SVG.renderBarChart SVG.lightTheme sampleChart
  assertBool "non-empty"
    (BS.length svg > 256)
  assertBool "starts with <?xml"
    (BS.take 5 svg == BS8.pack "<?xml")
  assertBool "contains <svg"
    (BS8.pack "<svg" `BS.isInfixOf` svg)
  assertBool "title text present"
    (BS8.pack "wireform-cbor vs cborg" `BS.isInfixOf` svg)
  assertBool "encode label present"
    (BS8.pack "encode" `BS.isInfixOf` svg)

lightDarkDiffer :: Assertion
lightDarkDiffer = do
  let (l, d) = SVG.renderBarChartBoth sampleChart
  assertBool "differ" (l /= d)
  -- Light has a near-white background; dark has near-black.
  assertBool "light has white bg" (BS8.pack "#ffffff" `BS.isInfixOf` l)
  assertBool "dark has dark bg"   (BS8.pack "#0d1117" `BS.isInfixOf` d)

singleSeries :: Assertion
singleSeries = do
  let chart = sampleChart
        { SVG.chartSeries =
            [SVG.Series "only" [1, 2, 3]]
        , SVG.chartGroups = ["a", "b", "c"]
        }
      svg = SVG.renderBarChart SVG.lightTheme chart
  assertBool "renders" (BS.length svg > 256)

emptyChart :: Assertion
emptyChart = do
  let chart = SVG.defaultGitHubBarChart "empty" "ns"
      svg = SVG.renderBarChart SVG.lightTheme chart
  assertBool "renders an svg root" (BS8.pack "<svg" `BS.isInfixOf` svg)
