module Main (main) where

import Test.Ion.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-ion-derive" $
      sequence_
        [ Test.Ion.Derive.tests
        ]
