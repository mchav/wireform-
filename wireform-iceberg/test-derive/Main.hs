module Main (main) where

import Test.Syd
import qualified Test.Iceberg.Derive

main :: IO ()
main = sydTest $ describe "wireform-iceberg-derive" $ sequence_
  [ Test.Iceberg.Derive.tests
  ]
