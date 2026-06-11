module Main (main) where

import Test.ASN1.Derive qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-asn1-derive" $
      sequence_
        [ Test.ASN1.Derive.tests
        ]
