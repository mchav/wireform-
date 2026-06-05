module Main (main) where

import Test.Syd
import qualified Test.CapnProto.Derive

main :: IO ()
main = sydTest $ describe "wireform-capnproto-derive" $ sequence_
  [ Test.CapnProto.Derive.tests
  ]
