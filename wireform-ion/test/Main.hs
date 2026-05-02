module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.Ion.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-ion-derive"
  [ Test.Ion.Derive.tests
  ]
