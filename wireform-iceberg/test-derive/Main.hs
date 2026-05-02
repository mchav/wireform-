module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.Iceberg.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-iceberg-derive"
  [ Test.Iceberg.Derive.tests
  ]
