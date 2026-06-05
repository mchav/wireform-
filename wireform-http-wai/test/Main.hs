module Main (main) where

import Test.Syd
import qualified Test.WAI

main :: IO ()
main = sydTest $ describe "wireform-http-wai" $ sequence_
  [ Test.WAI.tests
  ]
