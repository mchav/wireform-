module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Proto.Derive
import qualified Test.Proto.Derive.Auto
import qualified Test.Proto.Derive.Golden
import qualified Test.Proto.Derive.Metadata
import qualified Test.Proto.Derive.Oneof
import qualified Test.Proto.Derive.TopEnum

main :: IO ()
main = defaultMain $
  testGroup "wireform-proto:Derive"
    [ Test.Proto.Derive.tests
    , Test.Proto.Derive.Auto.tests
    , Test.Proto.Derive.Golden.tests
    , Test.Proto.Derive.Oneof.tests
    , Test.Proto.Derive.TopEnum.tests
    , Test.Proto.Derive.Metadata.tests
    ]
