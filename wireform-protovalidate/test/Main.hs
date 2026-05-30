module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Protovalidate.Descriptor
import qualified Test.Protovalidate.Format
import qualified Test.Protovalidate.Schema
import qualified Test.Protovalidate.Validation

main :: IO ()
main =
  defaultMain $
    testGroup
      "wireform-protovalidate"
      [ Test.Protovalidate.Format.tests
      , Test.Protovalidate.Validation.tests
      , Test.Protovalidate.Schema.tests
      , Test.Protovalidate.Descriptor.tests
      ]
