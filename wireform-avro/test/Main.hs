module Main (main) where

import Test.Avro.Derive qualified
import Test.Syd (describe, sydTest)


main :: IO ()
main = sydTest $ describe "wireform-avro-derive" Test.Avro.Derive.spec
