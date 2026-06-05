{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Syd

import qualified Test.Marker
import qualified Test.SVG
import qualified Test.Bench
import qualified Test.Test
import qualified Test.Coverage

main :: IO ()
main = sydTest $ describe "wireform-stats" $ sequence_
  [ Test.Marker.tests
  , Test.SVG.tests
  , Test.Bench.tests
  , Test.Test.tests
  , Test.Coverage.tests
  , trivial
  ]

trivial :: Spec
trivial = it "trivial sanity" $ True `shouldBe` True
