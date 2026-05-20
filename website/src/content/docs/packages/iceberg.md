---
title: wireform-iceberg
description: "Apache Iceberg table format with metadata JSON, Avro manifests, scan planning, schema evolution, partition transforms, deletion vectors, catalog clients, and time travel."
sidebar:
  order: 43
---

`wireform-iceberg` implements the Apache Iceberg open table format. Iceberg
adds ACID transactions, hidden partitioning, schema evolution, and time
travel on top of object-storage data files. Use this package when you need
to read table metadata, plan scans with predicate pushdown, or integrate
with Iceberg REST, Glue, Hadoop, or SQL catalogs from Haskell.

## Key features

- **Table metadata** as canonical JSON via `Iceberg.JSON`
- **Manifest and manifest-list** readers and writers (Avro-encoded)
- **Scan planning** with manifest pruning and file selection
- **Schema evolution** rules and compatibility checks
- **Partition transforms** (`identity`, `bucket`, `truncate`, time transforms)
- **Deletion vectors** and position/equality delete file handling
- **Puffin statistics** index format support
- **Catalog clients** for REST, AWS Glue, Hadoop filesystem, and SQL backends
- **Time travel** via snapshot refs, snapshot IDs, and timestamp lookup
- **Interop-tested** against pyiceberg

## Basic usage

Open a table by parsing its metadata JSON, then plan a scan over the
current snapshot. The scan planner resolves the manifest list, reads
each manifest, and collects the data file paths your reader should open:

```haskell
import qualified Data.Aeson              as Aeson
import qualified Data.ByteString         as BS
import qualified Data.Map.Strict         as Map
import qualified Iceberg.Expression      as Expr
import qualified Iceberg.JSON            as IJ
import qualified Iceberg.Read            as IR
import           Iceberg.Snapshot          (currentSnapshot)

openTableMetadata :: FilePath -> IO (Either String Iceberg.Types.TableMetadata)
openTableMetadata metadataPath = do
  jsonBytes <- BS.readFile metadataPath
  pure $ case Aeson.eitherDecodeStrict jsonBytes of
    Left err  -> Left err
    Right val -> IJ.metadataFromJSON val

planScanWithLocalManifests
  :: Iceberg.Types.TableMetadata
  -> ByteString
  -> Map Text ByteString
  -> Either String IR.ScanPlan
planScanWithLocalManifests tm manifestListBytes manifests =
  let filterExpr =
        Expr.and_
          (Expr.greaterThanOrEq "event_time" (Expr.LLong 1700000000000))
          (Expr.equal "region" (Expr.LString "us-west"))
      readManifest path =
        maybe (Left ("missing manifest: " ++ T.unpack path)) Right
          (Map.lookup path manifests)
  in case currentSnapshot tm of
       Nothing -> Left "table has no current snapshot"
       Just _  -> IR.planScanWithFilter tm manifestListBytes readManifest filterExpr
```

The resulting `ScanPlan` carries the resolved snapshot, schema, manifest
paths, and data file paths. Pass each data file path to `wireform-parquet`
(or another format reader) to materialize rows.

For catalog-backed tables, use `Iceberg.Catalog.REST.Client` or
`Iceberg.Catalog.Glue` to load metadata before calling the scan planner.

## Notable modules

| Module | Purpose |
|--------|---------|
| `Iceberg.Types` | `TableMetadata`, `Schema`, `Snapshot`, partition specs |
| `Iceberg.JSON` | `metadataToJSON` / `metadataFromJSON` |
| `Iceberg.Snapshot` | Snapshot lookup, refs, time travel, ancestry |
| `Iceberg.Read` | `planScan`, `planScanWithFilter`, manifest readers |
| `Iceberg.Write` | Snapshot and manifest emission |
| `Iceberg.Expression` | Predicate AST and manifest pruning evaluators |
| `Iceberg.Partition` / `Iceberg.Transform` | Partition spec evaluation |
| `Iceberg.SchemaEvolution` | Allowed schema changes |
| `Iceberg.Delete` / `Iceberg.DeletionVector` | Row-level delete handling |
| `Iceberg.Puffin` | Puffin auxiliary index files |
| `Iceberg.Catalog.*` | REST, Glue, Hadoop, and SQL catalog bindings |
| `Iceberg.Parquet` | Iceberg Parquet data file bridge |

## Interop

The probe suite round-trips table metadata, manifest lists, and manifest
files against pyiceberg and fastavro fixtures captured from real tables.
