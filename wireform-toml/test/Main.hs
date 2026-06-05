module Main (main) where

import Test.Syd
import qualified Test.TOML.Conformance
import qualified Test.TOML.Derive

main :: IO ()
main = do
  conf <- Test.TOML.Conformance.tests
  sydTest $ describe "wireform-toml" $ sequence_
    [ Test.TOML.Derive.tests
    , conf
    ]
