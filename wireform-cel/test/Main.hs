module Main (main) where

import Test.Syd

import qualified Test.CEL.Conformance
import qualified Test.CEL.Properties
import qualified Test.CEL.TH

main :: IO ()
main =
  sydTest $
    describe
      "wireform-cel" $ sequence_
      [ Test.CEL.Conformance.tests
      , Test.CEL.Properties.tests
      , Test.CEL.TH.tests
      ]
