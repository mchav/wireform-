module Main (main) where

import Test.Derive.Aeson qualified
import Test.Derive.Extension qualified
import Test.Derive.Fixtures qualified
import Test.Derive.Modifier qualified
import Test.Derive.NameStyle qualified
import Test.Syd


main :: IO ()
main = sydTest tests


tests :: Spec
tests =
  describe "wireform-derive" $
    sequence_
      [ Test.Derive.NameStyle.tests
      , Test.Derive.Modifier.tests
      , Test.Derive.Extension.tests
      , Test.Derive.Fixtures.tests
      , Test.Derive.Aeson.tests
      ]
