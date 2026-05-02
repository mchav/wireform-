module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.TOML.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-toml-derive"
  [ Test.TOML.Derive.tests
  ]
