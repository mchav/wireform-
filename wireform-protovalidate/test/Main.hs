module Main (main) where

import Test.Syd

import qualified Test.Protovalidate.Advanced
import qualified Test.Protovalidate.Descriptor
import qualified Test.Protovalidate.Format
import qualified Test.Protovalidate.Refined
import qualified Test.Protovalidate.Schema
import qualified Test.Protovalidate.TH
import qualified Test.Protovalidate.Validation

main :: IO ()
main =
  sydTest $
    describe
      "wireform-protovalidate" $ sequence_
      [ Test.Protovalidate.Format.tests
      , Test.Protovalidate.Validation.tests
      , Test.Protovalidate.Schema.tests
      , Test.Protovalidate.Descriptor.tests
      , Test.Protovalidate.Refined.tests
      , Test.Protovalidate.TH.tests
      , Test.Protovalidate.Advanced.tests
      ]
