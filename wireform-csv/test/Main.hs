module Main (main) where

import Test.CSV.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-csv-derive" $
      sequence_
        [ Test.CSV.Derive.tests
        ]
