module Main (main) where

import Test.Tasty

import qualified Test.Iceberg.Murmur3
import qualified Test.Iceberg.SingleValue
import qualified Test.Iceberg.Transform
import qualified Test.Iceberg.Expression
import qualified Test.Iceberg.Write
import qualified Test.Iceberg.Update
import qualified Test.Iceberg.View
import qualified Test.Iceberg.Puffin
import qualified Test.Iceberg.DeletionVector
import qualified Test.Iceberg.RESTCatalog
import qualified Test.Iceberg.NameMapping

main :: IO ()
main = defaultMain $ testGroup "wireform-iceberg"
  [ Test.Iceberg.Murmur3.tests
  , Test.Iceberg.SingleValue.tests
  , Test.Iceberg.Transform.tests
  , Test.Iceberg.Expression.tests
  , Test.Iceberg.Write.tests
  , Test.Iceberg.Update.tests
  , Test.Iceberg.View.tests
  , Test.Iceberg.Puffin.tests
  , Test.Iceberg.DeletionVector.tests
  , Test.Iceberg.RESTCatalog.tests
  , Test.Iceberg.NameMapping.tests
  ]
