module Main (main) where

import Test.Tasty

import qualified Test.Iceberg.BoundTrunc
import qualified Test.Iceberg.DeletionVector
import qualified Test.Iceberg.Expression
import qualified Test.Iceberg.ManifestMerge
import qualified Test.Iceberg.MetricsConfig
import qualified Test.Iceberg.Murmur3
import qualified Test.Iceberg.NameMapping
import qualified Test.Iceberg.Parquet
import qualified Test.Iceberg.Partition
import qualified Test.Iceberg.Puffin
import qualified Test.Iceberg.RESTCatalog
import qualified Test.Iceberg.RESTClient
import qualified Test.Iceberg.ScanPlan
import qualified Test.Iceberg.SchemaCompat
import qualified Test.Iceberg.SequenceInheritance
import qualified Test.Iceberg.SIMD
import qualified Test.Iceberg.SingleValue
import qualified Test.Iceberg.SnapshotHistory
import qualified Test.Iceberg.SnapshotSummary
import qualified Test.Iceberg.Sort
import qualified Test.Iceberg.Transform
import qualified Test.Iceberg.Update
import qualified Test.Iceberg.Validate
import qualified Test.Iceberg.View
import qualified Test.Iceberg.Write

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
  , Test.Iceberg.RESTClient.tests
  , Test.Iceberg.NameMapping.tests
  , Test.Iceberg.Parquet.tests
  , Test.Iceberg.ScanPlan.tests
  , Test.Iceberg.SequenceInheritance.tests
  , Test.Iceberg.SnapshotHistory.tests
  , Test.Iceberg.SnapshotSummary.tests
  , Test.Iceberg.Partition.tests
  , Test.Iceberg.Sort.tests
  , Test.Iceberg.ManifestMerge.tests
  , Test.Iceberg.MetricsConfig.tests
  , Test.Iceberg.BoundTrunc.tests
  , Test.Iceberg.SchemaCompat.tests
  , Test.Iceberg.SIMD.tests
  , Test.Iceberg.Validate.tests
  ]
