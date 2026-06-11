module Main (main) where

import Test.Syd
import Test.YAML.Annotated qualified
import Test.YAML.Conformance qualified
import Test.YAML.Decode qualified
import Test.YAML.Derive qualified
import Test.YAML.Encode qualified
import Test.YAML.Roundtrip qualified
import Test.YAML.Security qualified


main :: IO ()
main = do
  conf <- Test.YAML.Conformance.tests
  sydTest $
    describe "wireform-yaml" $
      sequence_
        [ Test.YAML.Decode.tests
        , Test.YAML.Encode.tests
        , Test.YAML.Roundtrip.tests
        , Test.YAML.Annotated.tests
        , Test.YAML.Security.tests
        , conf
        , Test.YAML.Derive.tests
        ]
