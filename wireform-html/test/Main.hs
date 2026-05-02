module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.HTML.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-html-derive"
  [ Test.HTML.Derive.tests
  ]
