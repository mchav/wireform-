module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.CEL.Conformance
import qualified Test.CEL.Properties
import qualified Test.CEL.TH

main :: IO ()
main =
  defaultMain $
    testGroup
      "wireform-cel"
      [ Test.CEL.Conformance.tests
      , Test.CEL.Properties.tests
      , Test.CEL.TH.tests
      ]
