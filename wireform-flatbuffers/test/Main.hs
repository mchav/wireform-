module Main (main) where

import Test.FlatBuffers.Derive qualified
import Test.FlatBuffers.View qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-flatbuffers" $
      sequence_
        [ Test.FlatBuffers.Derive.tests
        , Test.FlatBuffers.View.tests
        ]
