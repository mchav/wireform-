module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.Parquet.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-parquet-derive"
  [ Test.Parquet.Derive.tests
  ]
