module Main (main) where

import Test.Arrow.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-arrow-derive" $
      sequence_
        [ Test.Arrow.Derive.tests
        ]
