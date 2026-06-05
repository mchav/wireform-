module Main (main) where

import Test.Syd
import qualified Test.HTML.Derive

main :: IO ()
main = sydTest $ describe "wireform-html-derive" $ sequence_
  [ Test.HTML.Derive.tests
  ]
