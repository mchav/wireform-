module Main (main) where

import Test.CapnProto.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-capnproto-derive" $
      sequence_
        [ Test.CapnProto.Derive.tests
        ]
