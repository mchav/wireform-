module Main (main) where

import Test.Syd (sydTest, describe)
import qualified Test.Bencode.Derive

main :: IO ()
main = sydTest $ describe "wireform-bencode-derive" Test.Bencode.Derive.spec
