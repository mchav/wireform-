module Main (main) where

import Test.Syd

import qualified Test.Thrift.Derive

main :: IO ()
main = sydTest (describe "wireform-thrift" $ sequence_
  [ Test.Thrift.Derive.tests
  ])
