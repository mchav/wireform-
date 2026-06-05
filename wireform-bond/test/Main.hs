module Main (main) where

import Test.Syd
import qualified Test.Bond.Derive

main :: IO ()
main = sydTest $ describe "wireform-bond-derive" $ sequence_
  [ Test.Bond.Derive.tests
  ]
