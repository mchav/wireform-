module Main (main) where

import Test.CBOR.Derive qualified
import Test.Syd (describe, sydTest)


main :: IO ()
main = sydTest $ describe "wireform-cbor" Test.CBOR.Derive.spec
