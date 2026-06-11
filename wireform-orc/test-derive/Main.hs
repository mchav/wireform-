module Main (main) where

import Test.ORC.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-orc-derive" $
      sequence_
        [ Test.ORC.Derive.tests
        ]
