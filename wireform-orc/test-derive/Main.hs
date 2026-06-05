module Main (main) where

import Test.Syd
import qualified Test.ORC.Derive

main :: IO ()
main = sydTest $ describe "wireform-orc-derive" $ sequence_
  [ Test.ORC.Derive.tests
  ]
