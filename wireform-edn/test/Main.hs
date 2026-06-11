module Main (main) where

import Test.EDN.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-edn-derive" $
      sequence_
        [ Test.EDN.Derive.tests
        ]
