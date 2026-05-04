module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.FlatBuffers.Derive
import qualified Test.FlatBuffers.View

main :: IO ()
main = defaultMain $ testGroup "wireform-flatbuffers"
  [ Test.FlatBuffers.Derive.tests
  , Test.FlatBuffers.View.tests
  ]
