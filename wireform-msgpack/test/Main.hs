module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.MsgPack.Derive

main :: IO ()
main = defaultMain (testGroup "wireform-msgpack"
  [ Test.MsgPack.Derive.tests
  ])
