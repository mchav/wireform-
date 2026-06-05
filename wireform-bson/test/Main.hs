module Main (main) where

import Test.Syd
import qualified Test.BSON.Derive

main :: IO ()
main = sydTest $ describe "wireform-bson-derive" $ sequence_
  [ Test.BSON.Derive.tests
  ]
