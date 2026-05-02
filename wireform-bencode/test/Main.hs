module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.Bencode.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-bencode-derive"
  [ Test.Bencode.Derive.tests
  ]
