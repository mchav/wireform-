module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.CapnProto.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-capnproto-derive"
  [ Test.CapnProto.Derive.tests
  ]
