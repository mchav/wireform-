module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.Arrow.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-arrow-derive"
  [ Test.Arrow.Derive.tests
  ]
