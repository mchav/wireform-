module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.ORC.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-orc-derive"
  [ Test.ORC.Derive.tests
  ]
