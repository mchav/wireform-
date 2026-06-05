module Main (main) where

import Test.Syd
import qualified Test.NDJSON.Derive

main :: IO ()
main = sydTest $ describe "wireform-ndjson-derive" $ sequence_
  [ Test.NDJSON.Derive.tests
  ]
