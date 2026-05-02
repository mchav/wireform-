module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.EDN.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-edn-derive"
  [ Test.EDN.Derive.tests
  ]
