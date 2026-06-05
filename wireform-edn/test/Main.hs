module Main (main) where

import Test.Syd
import qualified Test.EDN.Derive

main :: IO ()
main = sydTest $ describe "wireform-edn-derive" $ sequence_
  [ Test.EDN.Derive.tests
  ]
