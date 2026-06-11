module Main (main) where

import Test.Bencode.Derive qualified
import Test.Syd (describe, sydTest)


main :: IO ()
main = sydTest $ describe "wireform-bencode-derive" Test.Bencode.Derive.spec
