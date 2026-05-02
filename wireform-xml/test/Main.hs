module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.XML.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-xml-derive"
  [ Test.XML.Derive.tests
  ]
