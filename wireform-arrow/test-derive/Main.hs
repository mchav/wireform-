module Main (main) where

import Test.Syd
import qualified Test.Arrow.Derive

main :: IO ()
main = sydTest $ describe "wireform-arrow-derive" $ sequence_
  [ Test.Arrow.Derive.tests
  ]
