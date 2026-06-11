module Main (main) where

import Test.Syd
import Test.WAI qualified


main :: IO ()
main =
  sydTest $
    describe "wireform-http-wai" $
      sequence_
        [ Test.WAI.tests
        ]
