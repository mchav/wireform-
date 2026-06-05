{-# LANGUAGE OverloadedStrings #-}
module Test.SVG (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Test.Syd

import qualified Wireform.Stats.SVG as SVG

tests :: Spec
tests = describe "SVG" $ sequence_
  [ it "renders a non-trivial chart" rendersNontrivial
  , it "light + dark differ"          lightDarkDiffer
  , it "single-series chart works"    singleSeries
  , it "empty chart still emits an svg root" emptyChart
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

rendersNontrivial :: IO ()
rendersNontrivial = do
  let svg = SVG.renderBarChart SVG.lightTheme sampleChart
  (BS.length svg > 256) `shouldBe` True
  (BS.take 5 svg == BS8.pack "<?xml") `shouldBe` True
  (BS8.pack "<svg" `BS.isInfixOf` svg) `shouldBe` True
  (BS8.pack "wireform-cbor vs cborg" `BS.isInfixOf` svg) `shouldBe` True
  (BS8.pack "encode" `BS.isInfixOf` svg) `shouldBe` True

lightDarkDiffer :: IO ()
lightDarkDiffer = do
  let (l, d) = SVG.renderBarChartBoth sampleChart
  (l /= d) `shouldBe` True
  -- Light has a near-white background; dark has near-black.
  (BS8.pack "#ffffff" `BS.isInfixOf` l) `shouldBe` True
  (BS8.pack "#0d1117" `BS.isInfixOf` d) `shouldBe` True

singleSeries :: IO ()
singleSeries = do
  let chart = sampleChart
        { SVG.chartSeries =
            [SVG.Series "only" [1, 2, 3]]
        , SVG.chartGroups = ["a", "b", "c"]
        }
      svg = SVG.renderBarChart SVG.lightTheme chart
  (BS.length svg > 256) `shouldBe` True

emptyChart :: IO ()
emptyChart = do
  let chart = SVG.defaultGitHubBarChart "empty" "ns"
      svg = SVG.renderBarChart SVG.lightTheme chart
  (BS8.pack "<svg" `BS.isInfixOf` svg) `shouldBe` True
