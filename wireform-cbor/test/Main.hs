module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.CBOR.Derive

main :: IO ()
main = defaultMain (testGroup "wireform-cbor"
  [ Test.CBOR.Derive.tests
  ])
