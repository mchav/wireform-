module Main (main) where

import Test.NDJSON.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-ndjson-derive" $
      sequence_
        [ Test.NDJSON.Derive.tests
        ]
