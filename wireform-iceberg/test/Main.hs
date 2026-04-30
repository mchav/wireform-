module Main (main) where

import Test.Tasty

import qualified Test.Iceberg.BoundTrunc
import qualified Test.Iceberg.CatalogHadoop
import qualified Test.Iceberg.CatalogSql
import qualified Test.Iceberg.Delete
import qualified Test.Iceberg.DeletionVector
import qualified Test.Iceberg.Geometry
import qualified Test.Iceberg.Expression
import qualified Test.Iceberg.Incremental
import qualified Test.Iceberg.Maintenance
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
import qualified Test.Iceberg.Hash
import qualified Test.Iceberg.SchemaCompat
import qualified Test.Iceberg.SequenceInheritance
import qualified Test.Iceberg.SingleValue
import qualified Test.Iceberg.SnapshotHistory
import qualified Test.Iceberg.SnapshotSummary
import qualified Test.Iceberg.Sort
import qualified Test.Iceberg.Transform
import qualified Test.Iceberg.Update
import qualified Test.Iceberg.Validate
import qualified Test.Iceberg.EncryptionProperty
import qualified Test.Iceberg.NestedProperty
import qualified Test.Iceberg.Variant
import qualified Test.Iceberg.VariantParquet
import qualified Test.Iceberg.VariantProperty
import qualified Test.Iceberg.VariantShredding
import qualified Test.Iceberg.View
import qualified Test.Iceberg.Write

main :: IO ()
main = defaultMain $ testGroup "wireform-iceberg"
  [ Test.Iceberg.Murmur3.tests
  , Test.Iceberg.SingleValue.tests
  , Test.Iceberg.Transform.tests
  , Test.Iceberg.Expression.tests
  , Test.Iceberg.Incremental.tests
  , Test.Iceberg.Maintenance.tests
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
  , Test.Iceberg.CatalogHadoop.tests
  , Test.Iceberg.CatalogSql.tests
  , Test.Iceberg.Delete.tests
  , Test.Iceberg.Geometry.tests
  , Test.Iceberg.Hash.tests
  , Test.Iceberg.SchemaCompat.tests
  , Test.Iceberg.Validate.tests
  , Test.Iceberg.EncryptionProperty.tests
  , Test.Iceberg.NestedProperty.tests
  , Test.Iceberg.Variant.tests
  , Test.Iceberg.VariantParquet.tests
  , Test.Iceberg.VariantProperty.tests
  , Test.Iceberg.VariantShredding.tests
  ]
