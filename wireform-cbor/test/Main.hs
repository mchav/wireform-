module Main (main) where

import Test.Syd (sydTest, describe)

import qualified Test.CBOR.Derive

main :: IO ()
main = sydTest $ describe "wireform-cbor" Test.CBOR.Derive.spec
