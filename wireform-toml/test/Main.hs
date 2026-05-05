module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.TOML.Conformance
import qualified Test.TOML.Derive

main :: IO ()
main = do
  conf <- Test.TOML.Conformance.tests
  defaultMain $ testGroup "wireform-toml"
    [ Test.TOML.Derive.tests
    , conf
    ]
