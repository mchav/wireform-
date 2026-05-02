module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.Bond.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-bond-derive"
  [ Test.Bond.Derive.tests
  ]
