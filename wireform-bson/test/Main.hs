module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.BSON.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-bson-derive"
  [ Test.BSON.Derive.tests
  ]
