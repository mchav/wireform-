module Main (main) where

import Test.Syd
import Test.Thrift.Derive qualified


main :: IO ()
main =
  sydTest
    ( describe "wireform-thrift" $
        sequence_
          [ Test.Thrift.Derive.tests
          ]
    )
