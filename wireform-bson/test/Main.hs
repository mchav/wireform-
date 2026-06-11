module Main (main) where

import Test.BSON.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-bson-derive" $
      sequence_
        [ Test.BSON.Derive.tests
        ]
