module Main (main) where

import Test.Syd

import qualified Test.MsgPack.Derive

main :: IO ()
main = sydTest (describe "wireform-msgpack" $ sequence_
  [ Test.MsgPack.Derive.tests
  ])
