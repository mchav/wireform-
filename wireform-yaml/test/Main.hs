module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.YAML.Annotated
import qualified Test.YAML.Decode
import qualified Test.YAML.Encode
import qualified Test.YAML.Roundtrip
import qualified Test.YAML.Conformance
import qualified Test.YAML.Derive
import qualified Test.YAML.Security

main :: IO ()
main = do
  conf <- Test.YAML.Conformance.tests
  defaultMain $ testGroup "wireform-yaml"
    [ Test.YAML.Decode.tests
    , Test.YAML.Encode.tests
    , Test.YAML.Roundtrip.tests
    , Test.YAML.Annotated.tests
    , Test.YAML.Security.tests
    , conf
    , Test.YAML.Derive.tests
    ]
