module Main (main) where

import Test.Syd
import qualified Test.ASN1.Derive

main :: IO ()
main = sydTest $ describe "wireform-asn1-derive" $ sequence_
  [ Test.ASN1.Derive.tests
  ]
