module Main (main) where

import Test.Bond.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-bond-derive" $
      sequence_
        [ Test.Bond.Derive.tests
        ]
