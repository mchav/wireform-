module Main (main) where

import Test.CEL.Conformance qualified
import Test.CEL.Properties qualified
import Test.CEL.TH qualified
import Test.Syd


main :: IO ()
main =
  sydTest
    $ describe
      "wireform-cel"
    $ sequence_
      [ Test.CEL.Conformance.tests
      , Test.CEL.Properties.tests
      , Test.CEL.TH.tests
      ]
