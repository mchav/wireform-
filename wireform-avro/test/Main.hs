module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.Avro.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-avro-derive"
  [ Test.Avro.Derive.tests
  ]
