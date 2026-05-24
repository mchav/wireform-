module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.WAI

main :: IO ()
main = defaultMain $ testGroup "wireform-http-wai"
  [ Test.WAI.tests
  ]
