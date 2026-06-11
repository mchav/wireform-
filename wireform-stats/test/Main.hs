{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Bench qualified
import Test.Coverage qualified
import Test.Marker qualified
import Test.SVG qualified
import Test.Syd
import Test.Test qualified


main :: IO ()
main =
  sydTest $
    describe "wireform-stats" $
      sequence_
        [ Test.Marker.tests
        , Test.SVG.tests
        , Test.Bench.tests
        , Test.Test.tests
        , Test.Coverage.tests
        , trivial
        ]


trivial :: Spec
trivial = it "trivial sanity" $ True `shouldBe` True
