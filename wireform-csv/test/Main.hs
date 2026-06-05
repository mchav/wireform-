module Main (main) where

import Test.Syd
import qualified Test.CSV.Derive

main :: IO ()
main = sydTest $ describe "wireform-csv-derive" $ sequence_
  [ Test.CSV.Derive.tests
  ]
