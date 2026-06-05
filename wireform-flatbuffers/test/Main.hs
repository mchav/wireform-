module Main (main) where

import Test.Syd
import qualified Test.FlatBuffers.Derive
import qualified Test.FlatBuffers.View

main :: IO ()
main = sydTest $ describe "wireform-flatbuffers" $ sequence_
  [ Test.FlatBuffers.Derive.tests
  , Test.FlatBuffers.View.tests
  ]
