module Main (main) where

import Test.MsgPack.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest
    ( describe "wireform-msgpack" $
        sequence_
          [ Test.MsgPack.Derive.tests
          ]
    )
