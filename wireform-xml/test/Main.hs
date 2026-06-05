module Main (main) where

import Test.Syd
import qualified Test.XML.Derive

main :: IO ()
main = sydTest $ describe "wireform-xml-derive" $ sequence_
  [ Test.XML.Derive.tests
  ]
