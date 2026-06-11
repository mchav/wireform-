module Main (main) where

import Test.Syd
import Test.TOML.Conformance qualified
import Test.TOML.Derive qualified


main :: IO ()
main = do
  conf <- Test.TOML.Conformance.tests
  sydTest $
    describe "wireform-toml" $
      sequence_
        [ Test.TOML.Derive.tests
        , conf
        ]
