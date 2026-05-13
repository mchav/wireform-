module Main (main) where

import Test.Proto.TH.Derive qualified
import Test.Proto.TH.Derive.Auto qualified
import Test.Proto.TH.Derive.Golden qualified
import Test.Proto.TH.Derive.Metadata qualified
import Test.Proto.TH.Derive.Oneof qualified
import Test.Proto.TH.Derive.TopEnum qualified
import Test.Tasty (defaultMain, testGroup)


main :: IO ()
main =
  defaultMain $
    testGroup
      "wireform-proto:Derive"
      [ Test.Proto.TH.Derive.tests
      , Test.Proto.TH.Derive.Auto.tests
      , Test.Proto.TH.Derive.Golden.tests
      , Test.Proto.TH.Derive.Oneof.tests
      , Test.Proto.TH.Derive.TopEnum.tests
      , Test.Proto.TH.Derive.Metadata.tests
      ]
