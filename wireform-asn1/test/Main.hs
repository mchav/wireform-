module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Test.ASN1.Derive

main :: IO ()
main = defaultMain $ testGroup "wireform-asn1-derive"
  [ Test.ASN1.Derive.tests
  ]
