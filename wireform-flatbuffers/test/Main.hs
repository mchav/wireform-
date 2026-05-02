module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.FlatBuffers.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-flatbuffers-derive"
  [ Test.FlatBuffers.Derive.tests
  ]
