# Package overviews

A one-page guide to every columnar-storage package in
wireform. Each entry summarises what's in the package, what it
depends on, and the entry points users typically reach for.

## `wireform-columnar`

Format-agnostic primitives shared by every columnar package.

- `Columnar.Predicate` — `Predicate` ADT, `PSkip` / `PMaybeKeep`
  decisions, `evalRange` for the per-leaf comparison loop.
  Used by Parquet's row-group / page-index / bloom evaluators
  and ORC's stripe / row-group evaluator.
- `Columnar.Stream` — pull-based `Iter` (pure, error-aware) +
  `IterIO` (IO-shaped, mirrored API) + combinators
  (`iterMap` / `iterFilter` / `iterFold` / `iterTake` /
  `iterChunk` / `iterScan` / `iterMergeBy` / `iterRowSlice` /
  `iterIOPrefetch` / `iterParallelMap`).
- `Columnar.SIMD` — bitmap popcount, LSB bit unpack, memcpy
  via SIMDe (used by Arrow validity decoding + Parquet
  optional-page interleaving).

Entry point for new code: `Columnar.Stream.Iter` is the
streaming-shape callers should reach for.

## `wireform-arrow`

Apache Arrow IPC stream + file format, plus the in-memory
columnar data model (`ColumnArray`).

- `Arrow.Types` — schema / field / type / dictionary
  metadata. `defaultSchema` / `defaultLeafField` /
  `defaultField` smart constructors. `schemaFingerprint` /
  `schemaEquivalent` for cross-file schema comparison.
- `Arrow.Column` — `ColumnArray` ADT (every Arrow physical
  type, primitive + nested + view + REE), materialiser,
  `sliceColumnArray` for row windowing,
  `validateMapKeysSorted` for the AMap invariant.
- `Arrow.IPC` / `Arrow.FlatBufferIPC` — wire codecs.
- `Arrow.Stream` — high-level reader/writer with iterator
  shape (`streamReaderIter`, `streamReaderProjectedIter`),
  body compression, dict-batch handling.
- `Arrow.Record` — record-level codecs (`RowEncoder`,
  `RowDecoder`, `Table`, `structE`/`structD`,
  `encoderFromRowEncoder`, `subsetTable`, `projectTable`,
  `columnDWithDefault`, `NameStrategy`).
- `Arrow.Record.Generic` — Generic-derivable `HasEncoder` /
  `HasDecoder` / `HasRowEncoder` / `HasRowDecoder`.
- `Arrow.Derive` — annotation-driven TH deriver for
  `HasTable` / `HasEncoder` / `HasDecoder`.

Entry point: `Arrow.Stream.encodeArrowStream` /
`decodeArrowStream` for raw IPC bytes;
`Arrow.Record.encodeTable` / `decodeTable` for record-level.

## `wireform-parquet`

Apache Parquet metadata, column pages, read + write.

- `Parquet.Types` — `FileMetadata`, `RowGroup`,
  `ColumnChunk`, `Statistics`, `OffsetIndex`, `ColumnIndex`,
  `LogicalType`, `SortingColumn`, `ColumnOrder`. Mirrors
  parquet.thrift.
- `Parquet.Footer` / `Parquet.Thrift.Schema` — the Thrift
  Compact Protocol codec used by every metadata struct.
- `Parquet.Page` — page-header decode (DATA_PAGE,
  DATA_PAGE_V2, DICTIONARY_PAGE).
- `Parquet.Read` — every encoding (PLAIN, dictionary,
  DELTA_*, BYTE_STREAM_SPLIT) for every physical type.
  Required + optional + page-pruned variants.
  Path-based + handle-based file readers
  (`loadParquetFilePath`, `openParquetReader`).
- `Parquet.Write` — column-chunk + row-group + footer
  builders. `buildParquetFileWithIndex` for the full
  page-index + bloom path; `buildParquetFileMixedWith` for
  the nullable-with-compression path.
- `Parquet.HighLevel` — top-of-stack API
  (`encodeParquet` / `decodeParquet`) with auto-populated
  bloom filters.
- `Parquet.Arrow` — Arrow ↔ Parquet bridge with column
  projection + page-pruning.
- `Parquet.Predicate` — `Pred.Predicate` evaluators against
  every metadata tier.
- `Parquet.Aggregate` — count(*), count(col), min(col),
  max(col) from stats only; no decode.
- `Parquet.BloomFilter` — Sbbf encode/decode + membership.
- `Parquet.PageIndex` — `OffsetIndex` + `ColumnIndex` codec.
- `Parquet.Encryption` — modular encryption (AesGcmV1 +
  AesGcmCtrV1).
- `Parquet.Nested` — Dremel-style shredder for nested
  struct/list/map.

Entry point: `Wireform.Columnar.{encode,decode,decodeIter}`
when you don't need format-specific knobs;
`Parquet.HighLevel` when you do.

## `wireform-orc`

Apache ORC metadata, stripes, read + write.

- `ORC.Types` — `ORCFooter`, `StripeInformation`, `ORCType`,
  `TypeKind` (incl. TKTimestampInstant), `ColumnStatistics`
  + 8 sub-statistics records.
- `ORC.Footer` — protobuf footer codec including full
  `ColumnStatistics` round-trip.
- `ORC.Read` — RLE v2 decoders, per-column readers.
  `loadORCFilePath` + `openORCReader` for handle-backed
  iteration.
- `ORC.Write` — stripe + footer builders.
- `ORC.Arrow` — Arrow ↔ ORC bridge with column projection +
  stripe-level filtering.
- `ORC.Statistics` — predicate evaluator + decimal-text
  parsing helper.
- `ORC.RowIndex` — encode + decode for the ROW_INDEX stream.
- `ORC.BloomFilter` — encode + decode + membership probes.
- `ORC.Encryption` — column-level encryption.

Entry point: `Wireform.Columnar.{encode,decode,decodeIter}`;
`ORC` for the high-level all-stripes API.

## `wireform-iceberg`

Apache Iceberg table format on top of Parquet + Avro.

- `Iceberg.Manifest` — Avro schemas + `pruneManifestFiles`
  helper for scan-planning.
- `Iceberg.Snapshot`, `Iceberg.Update`, `Iceberg.Validate` —
  table operations.
- `Iceberg.Catalog.{Glue,Hadoop,REST,Sql}` — catalog
  clients.
- `Iceberg.Variant` — semi-structured Variant column.

Entry point: `Iceberg.Read` / `Iceberg.Write` for table-level
operations; the Parquet bridge handles per-data-file
materialisation.

## `wireform-delta`, `wireform-lance`, `wireform-hudi`

Skeleton packages for Delta Lake / Lance / Hudi. Each
exposes the format's wire-level entry point + the metadata
structures that downstream tooling needs; full readers are
follow-ups.

## `Wireform.Columnar` (umbrella facade)

`facade/Wireform/Columnar.hs` re-exports every per-format
package under one set of names so callers can pick a format
at the call site.

- `encode` / `decode` — eager round-trip.
- `decodeIter` — pull-based row-group / stripe / record-batch
  iteration.
- `decodeProjectedIter` — same with column projection.
- `decodeFilteredIter` — same with `Predicate` row-group
  pushdown.
- `decodeRecordsIter` — typed records via `Arrow.Record.Table`
  (auto-projects to required columns).
- `decodeDatasetIter` / `decodeDatasetProjectedIter` /
  `decodeDatasetRowSlicedIter` — multi-file datasets.
- `decodeHeterogeneousDatasetIter` — mixed-format datasets.
- `decodePartitionedDataset` + `parsePartitionPath` — Hive-
  style directory partitioning.
