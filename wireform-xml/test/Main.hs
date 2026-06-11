module Main (main) where

import Test.Syd
import Test.XML.Derive qualified


main :: IO ()
main =
  sydTest $
    describe "wireform-xml-derive" $
      sequence_
        [ Test.XML.Derive.tests
        ]
