module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.CEL.Conformance
import qualified Test.CEL.Properties

main :: IO ()
main =
  defaultMain $
    testGroup
      "wireform-cel"
      [ Test.CEL.Conformance.tests
      , Test.CEL.Properties.tests
      ]
