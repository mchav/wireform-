module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Proto.Derive
import qualified Test.Proto.Derive.Oneof

main :: IO ()
main = defaultMain $
  testGroup "wireform-proto:Derive"
    [ Test.Proto.Derive.tests
    , Test.Proto.Derive.Oneof.tests
    ]
