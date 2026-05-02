module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.CSV.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-csv-derive"
  [ Test.CSV.Derive.tests
  ]
