module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Thrift.Derive

main :: IO ()
main = defaultMain (testGroup "wireform-thrift"
  [ Test.Thrift.Derive.tests
  ])
