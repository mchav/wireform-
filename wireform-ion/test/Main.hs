module Main (main) where

import Test.Syd
import qualified Test.Ion.Derive

main :: IO ()
main = sydTest $ describe "wireform-ion-derive" $ sequence_
  [ Test.Ion.Derive.tests
  ]
