{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.Marker
import qualified Test.SVG
import qualified Test.Bench
import qualified Test.Test
import qualified Test.Coverage

main :: IO ()
main = defaultMain $ testGroup "wireform-stats"
  [ Test.Marker.tests
  , Test.SVG.tests
  , Test.Bench.tests
  , Test.Test.tests
  , Test.Coverage.tests
  , trivial
  ]

trivial :: TestTree
trivial = testCase "trivial sanity" $ True @?= True
