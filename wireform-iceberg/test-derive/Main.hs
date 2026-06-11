module Main (main) where

import Test.Iceberg.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-iceberg-derive" $
      sequence_
        [ Test.Iceberg.Derive.tests
        ]
