module Main (main) where

import Test.Parquet.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-parquet-derive" $
      sequence_
        [ Test.Parquet.Derive.tests
        ]
