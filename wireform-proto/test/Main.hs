module Main (main) where

import Test.Proto.Derive qualified
import Test.Proto.Derive.Auto qualified
import Test.Proto.Derive.Golden qualified
import Test.Proto.Derive.Metadata qualified
import Test.Proto.Derive.Oneof qualified
import Test.Proto.Derive.TopEnum qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe
      "wireform-proto:Derive" $ sequence_
      [ Test.Proto.Derive.tests
      , Test.Proto.Derive.Auto.tests
      , Test.Proto.Derive.Golden.tests
      , Test.Proto.Derive.Oneof.tests
      , Test.Proto.Derive.TopEnum.tests
      , Test.Proto.Derive.Metadata.tests
      ]
