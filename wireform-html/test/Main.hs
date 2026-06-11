module Main (main) where

import Test.HTML.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-html-derive" $
      sequence_
        [ Test.HTML.Derive.tests
        ]
