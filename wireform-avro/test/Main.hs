module Main (main) where

import Test.Syd (sydTest, describe)
import qualified Test.Avro.Derive

main :: IO ()
main = sydTest $ describe "wireform-avro-derive" Test.Avro.Derive.spec
