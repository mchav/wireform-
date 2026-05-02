module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.NDJSON.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-ndjson-derive"
  [ Test.NDJSON.Derive.tests
  ]
