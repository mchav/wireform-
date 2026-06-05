module Main (main) where

import Test.Syd
import qualified Test.Parquet.Derive

main :: IO ()
main = sydTest $ describe "wireform-parquet-derive" $ sequence_
  [ Test.Parquet.Derive.tests
  ]
