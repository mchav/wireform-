module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.Fury.Encoding
import qualified Test.Fury.Value
import qualified Test.Fury.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-fury"
  [ Test.Fury.Encoding.tests
  , Test.Fury.Value.tests
  , Test.Fury.Derive.tests
  ]
