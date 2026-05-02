module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Test.Derive.Aeson
import qualified Test.Derive.Extension
import qualified Test.Derive.Fixtures
import qualified Test.Derive.Modifier
import qualified Test.Derive.NameStyle

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "wireform-derive"
  [ Test.Derive.NameStyle.tests
  , Test.Derive.Modifier.tests
  , Test.Derive.Extension.tests
  , Test.Derive.Fixtures.tests
  , Test.Derive.Aeson.tests
  ]
