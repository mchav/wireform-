# Columnar & lake-format roadmap (wireform)

This document tracks progress toward **complete, spec-faithful** support for the
columnar and table-metadata formats exposed in wireform. “Complete” is staged:
each format has **reader** and (where applicable) **writer** milestones, with
optional spec extensions (encryption, advanced statistics, etc.) in late tiers.

---

## Shared principles

- **Tests**: golden vectors vs reference implementations (arrow-rs / pyarrow /
  parquet-cpp) where feasible; property tests for round-trips on writers.
- **Performance**: keep hot paths allocation-tight (existing wireform columnar
  guidelines in `AGENTS.md`).
- **Dependencies**: optional compression codecs via Cabal flags (`snappy`,
  `zstd`, future `lz4`).

---

## Phase A — Parquet

### Page index + bloom filter (A.7) details

* `Parquet.PageIndex` round-trips `OffsetIndex` (page locations + optional
  unencoded byte-array sizes) and `ColumnIndex` (null pages, min/max,
  `BoundaryOrder`, optional null-counts and rep/def histograms) through the
  Thrift Compact codec used by `Parquet.Footer`.
* `Parquet.BloomFilter` implements parquet-format 2.10 split-block bloom
  filters (BLOCK + XXHASH + UNCOMPRESSED). The bitset is an unboxed
  `Vector Word64` and the inner kernels do not allocate `Maybe`/`Either`
  per insert/check.
* `Wireform.Hash.xxh64` is the single canonical XXH64 entry point —
  C/SIMDe-backed and byte-exact against `xxhsum -H1`. The Parquet bloom
  filter calls it directly; the legacy `Parquet.XXH64` wrapper module
  has been removed.
* `Parquet.Types.ColumnChunk` and `ColumnMetadata` now carry the page-index
  and bloom-filter offset/length pointers from the parquet.thrift spec
  (fields 4-7 on `ColumnChunk`, fields 14-15 on `ColumnMetaData`).


| Milestone | Reader | Writer | Status |
|-----------|--------|--------|--------|
| A.1 | Footer + Thrift metadata (done) | Footer round-trip (done) | Done |
| A.2 | DATA_PAGE v1: **def/rep levels** + PLAIN **optional** primitives | — | **Done** |
| A.3 | PLAIN_DICTIONARY + levels; dictionary optional columns | — | **Done** |
| A.4 | DATA_PAGE v2 + DELTA_BINARY_PACKED encoding | — | **Done** |
| A.5 | All compression codecs used in the wild (incl. LZ4 / LZ4_RAW / Brotli) | — | **Done** (`Parquet.Compress`: GZip built-in; Snappy/ZSTD/LZ4_RAW/Brotli behind `-fsnappy`/`-fzstd`/`-flz4`/`-fbrotli`; LZO explicitly unsupported as a legacy Hadoop codec) |
| A.6 | Column writer + file assembly + reference-file tests | Writer | **Done** |
| A.7 | Statistics, Bloom filters, page indexes, encryption | Optional tier | **Done** (page index + bloom filter + AES-GCM/CTR modular encryption) |
| A.8 | Remaining encodings (DELTA_LENGTH_BYTE_ARRAY, DELTA_BYTE_ARRAY, BYTE_STREAM_SPLIT, RLE_DICTIONARY) | — | **Done** |
| A.9 | Repetition level semantics for repeated/nested columns | — | **Partial** (`materializeRepeated*`) |
| A.10 | Heterogeneous-primitive Parquet writer (all primitive types via `ColumnData`) | Writer | **Done** (`buildParquetFile` / `buildParquetFileWithIndex`) |
| A.11 | Per-column compression on the writer (Uncompressed / GZip / Snappy / ZSTD / LZ4_RAW) | Writer | **Done** (`Parquet.Compress`, `ColumnAux.caCodec`) |
| A.12 | Writer-side definition levels + nullable PLAIN data pages | Writer | **Done** (`Parquet.LevelsEncode`, `OptionalColumn`) |
| A.13 | Writer-side dictionary encoding | Writer | **Done** (`buildDictionary`, `encodeDictPage`, `encodeDictDataPage`) |
| A.14 | Writer-side DELTA_BINARY_PACKED | Writer | **Done** (`Parquet.DeltaEncode`) |
| A.15 | Spec-correct field IDs in footer Thrift (verified via pyarrow golden round-trip) | Read+Write | **Done** |
| A.16 | SIMD-backed `null_pages` bitmap helpers | — | **Done** (`Parquet.NullPagesBitmap`) |
| A.17 | DATA_PAGE_V2 writer (separate def/rep/data, header-only compression flag) | Writer | **Done** (`encodeColumnDataPageV2` / `encodeOptionalColumnPageV2`, `ColumnAux.caPageVersion`) |
| A.18 | DELTA_LENGTH_BYTE_ARRAY writer | Writer | **Done** (`Parquet.DeltaEncode.encodeDeltaLengthByteArray`) |
| A.19 | DELTA_BYTE_ARRAY writer (incremental string encoding) | Writer | **Done** (`Parquet.DeltaEncode.encodeDeltaByteArray`) |
| A.20 | BYTE_STREAM_SPLIT writer (FLOAT + DOUBLE) | Writer | **Done** (`Parquet.ByteStreamSplit`) |
| A.21 | SchemaElement.field_id (Iceberg leaf identification) | Read+Write | **Done** (`seFieldId`) |
| A.22 | PageType discriminated-union refactor (one sum, no parallel `Maybe`s) | Internal | **Done** (`Parquet.Page.PageType`) |

---

## Phase B — Apache Arrow IPC

| Milestone | Reader | Writer | Status |
|-----------|--------|--------|--------|
| B.1 | Flat record batch materialization | — | **Done** |
| B.2 | Nested types (struct, list), dictionaries | Symmetric | **Done** |
| B.3 | Stream + file IPC + writer | Writer | **Done** (writer covers every `ColumnArray` constructor) |
| B.4 | Golden IPC interop with pyarrow | Symmetric | **Done** — `Arrow.FlatBufferIPC` emits standards-compliant FlatBuffers metadata; `writeArrowStreamFBFromColumns` produces bytes that `pyarrow.ipc.open_stream` decodes end-to-end (schema + record batches, primitive + variable-length + nullable columns) |
| B.5 | f16, unsigned ints, unions, map, large binary/utf8, interval, fixed-size list, dictionary | Read + Write | **Done** (`Arrow.Column` materializers + `Arrow.Write.encodeCol` cover every `ArrowType`; `ADecimal256` is distinct from `ADecimal` to round-trip both widths) |
| B.6 | wireform-arrow internal round-trip test suite | — | **Done** (`wireform-arrow/test/Main.hs`: every `ColumnArray` constructor survives a `writeArrowStream` + `readArrowStream` + `materializeRecordBatch` cycle) |

---

## Phase C — Apache ORC

| Milestone | Reader | Writer | Status |
|-----------|--------|--------|--------|
| C.1 | Footer + stripe slice + stream bytes | — | **Done** |
| C.2 | Integer RLE v1/v2 + present stream + boolean RLE | — | **Done** |
| C.3 | Column decoders (int, bool, string, float, double) + compression | — | **Done** |
| C.4 | End-to-end `readColumn` | — | **Done** |
| C.5 | Remaining types (timestamp, date, decimal) + RLE v2 Patched Base | — | **Done** (`decodeTimestampColumn`, `decodeDateColumn`, `decodeDecimalColumn`, `decodeShortColumn`, `decodeTinyIntColumn`, `decodeBinaryColumn`, RLE v2 Patched Base) |
| C.6 | Writer + ORC file assembly | Writer | **Done** (`buildORCFile`, integer/string/float/double/bool/date/timestamp/decimal encoders) |
| C.7 | Per-stripe bloom filter (`BLOOM_FILTER_UTF8`) | Writer | **Done** (`ORC.BloomFilter`) |
| C.8 | Per-stripe row index (`ROW_INDEX`) | Writer | **Done** (`ORC.RowIndex`) |
| C.9 | DECIMAL128 stream decoder (LEB128 zig-zag, full Integer precision) | Reader | **Done** (`ORC.Read.decodeDecimal128Stream`) |
| C.10 | `DICTIONARY_V2` string writer + reader (auto-dispatch on dict-non-empty) | Read + Write | **Done** (`ORC.Write.encodeStringDictColumn`, `ORC.Read.decodeStringColumn` dispatches on `dictBs`; `ORC.RLE.decodeRLEv2IntAll` decodes the unknown-count dictionary length stream) |
| C.11 | Whole-file column encryption integration (AES-CTR stripe streams + `Footer.encryption` round-trip) | Read + Write | **Done** (`ORC.Write.buildEncryptedORCFile`, `encryptStripeStreams`, `decryptStripeStream`; `ORCFooter.orcEncryption :: Maybe FooterEncryption`) |

---

## Phase D — Apache Iceberg

| Milestone | Scope | Status |
|-----------|--------|--------|
| D.1 | Manifest / manifest-list Avro; path helpers | **Done** |
| D.2 | Snapshot selection, schema evolution, partition specs, scan planning | **Done** |
| D.3 | Delete files (position / equality), sequence numbers | **Done** (`PositionDelete`, `applyPositionDeletes`, `planScanWithDeletes`, sequence/file-sequence numbers) |
| D.4 | REST catalog client | **Done** (`Iceberg.Catalog.REST` request/response types + JSON) |
| D.5 | Bucket / Truncate / Year / Month / Day / Hour transforms (Murmur3) | **Done** (`Iceberg.Murmur3`, `Iceberg.Transform`) |
| D.6 | Predicate AST + manifest pruning (inclusive + strict) | **Done** (`Iceberg.Expression`, `planScanWithFilter`) |
| D.7 | Manifest / manifest-list / table-metadata writers | **Done** (`Iceberg.Write`) |
| D.8 | Table commit operations (Append, Overwrite, RowDelta, branches, tags) | **Done** (`Iceberg.Update`) |
| D.9 | Iceberg View spec | **Done** (types + JSON + `Iceberg.View` updates) |
| D.10 | Statistics / partition-statistics file references | **Done** |
| D.11 | V3 deletion vectors (Puffin Roaring64) | **Done** (`Iceberg.Puffin`, `Iceberg.DeletionVector`) |
| D.12 | V3 type extensions (nanos, unknown, variant, geometry, geography) | **Done** (types + JSON) |
| D.13 | V3 default values, row lineage, encryption keys | **Done** (types + JSON) |
| D.14 | Schema name mapping (`schema.name-mapping.default`) | **Done** |
| D.15 | Canonical Iceberg field-id annotations on writer Avro schemas | **Done** (`Iceberg.Write`) |
| D.16 | Sequence-number inheritance on manifest read | **Done** (`Iceberg.Read.inheritSequenceNumbers`) |
| D.17 | Snapshot history helpers + rollback ancestor validation | **Done** (`Iceberg.Snapshot`, `Iceberg.Update`) |
| D.18 | Auto-computed snapshot summary keys | **Done** (`Iceberg.Update` `SnapshotStats` / `autoSummary`) |
| D.19 | Time-travel scans (`planScanAtSnapshot` / `planScanAsOfTime`) | **Done** (`Iceberg.Read`) |
| D.20 | Predicate IN / NOT_IN / STARTS_WITH / NOT_STARTS_WITH evaluators | **Done** (`Iceberg.Expression`) |
| D.21 | Row -> partition tuple + predicate-through-partition projection | **Done** (`Iceberg.Partition`) |
| D.22 | Sort key evaluation | **Done** (`Iceberg.Sort`) |
| D.23 | Per-column MetricsConfig + bound truncation | **Done** (`Iceberg.MetricsConfig`, `Iceberg.BoundTrunc`) |
| D.24 | Schema evolution rule validator | **Done** (`Iceberg.SchemaCompat`) |
| D.25 | Table-metadata structural validation | **Done** (`Iceberg.Validate`) |
| D.26 | gzip metadata.json compression | **Done** (`Iceberg.Write.encodeTableMetadataCompressed`) |
| D.27 | REST catalog convenience helpers (LoadTableResult builder, exception type) | **Done** (`Iceberg.Catalog.REST`) |
| D.28 | Manifest-list partition summary aggregation | **Done** (`Iceberg.Write.buildManifestSummary`) |
| D.29 | Fast-append / merge-append / rewrite-manifests commit planner | **Done** (`Iceberg.ManifestMerge`) |
| D.30 | C/SIMDe kernels for Murmur3, `bucket[N]`, XXH64, Roaring 32-bit, deletion-vector membership | **Done** (`Wireform.Hash` is the single canonical home; benchmarks in `wireform-iceberg/bench/RESULTS.md`) |
| D.31 | Iceberg ↔ Parquet bridge (`DataFile` derived from Parquet footer; `OffsetIndex` × `DeletionVector` page selection) | **Done** (`Iceberg.Parquet`) |
| D.32 | Iceberg REST catalog HTTP client | **Done** (`Iceberg.Catalog.REST.Client`, behind `-frest-client`) |
| D.33 | Iceberg encryption-keys wiring to Parquet `EncryptionConfig` | **Done** (`Iceberg.Parquet.encryptionConfigFromTable`, `withEncryptionKeyMetadata`) |
| D.34 | Incremental scans (`planIncrementalAppend`, `planIncrementalChangelog`) | **Done** (`Iceberg.Read`) |
| D.35 | Snapshot expiration + orphan-file detection | **Done** (`Iceberg.Maintenance`) |
| D.36 | End-to-end Iceberg + Parquet pipeline example | **Done** (`examples/IcebergPipeline.hs`) |
| D.37 | `iceberg` CLI (metadata-show / manifest-show / expire / orphans / REST) | **Done** (`wireform-iceberg/app/Main.hs`) |
| D.38 | pyarrow golden round-trip (proves byte-compat with arrow-cpp) | **Done** (`wireform-parquet/test/fixtures`) |
| D.39 | Iceberg position-delete + equality-delete file writers (compose Parquet writer + Iceberg.Parquet bridge) | **Done** (`Iceberg.Delete`) |
| D.40 | REST catalog write-side: rename / register / view CRUD / namespace property updates | **Done** (`Iceberg.Catalog.REST.Client`) |
| D.41 | V3 multi-source-ids partition fields (multi-arg `bucket[N]` / `truncate[W]`) | **Done** (`PartitionField.pfSourceIds`, V1/V2 single + V3 multi unified) |
| D.42 | V3 geometry / geography column bounds: WKB POINT codec | **Done** (`Iceberg.Geometry`) |
| D.43 | Hadoop file-based catalog (FS-agnostic via `FileSystem` record; optimistic concurrency on `version-hint.text`) | **Done** (`Iceberg.Catalog.Hadoop`) |
| D.44 | SQL ("JDBC") catalog (backend-agnostic via `SqlBackend` record; CAS-on-`metadata_location` commits) | **Done** (`Iceberg.Catalog.Sql`) |
| D.45 | V3 Variant binary encoding + Variant ↔ JSON bridge | **Done** (`Iceberg.Variant`) |
| D.46 | Parquet writer: per-column modular encryption (AES-GCM-V1 + AES-GCM-CTR-V1, deterministic-nonce GCM, V1 + V2 + aux modules) | **Done** (`Parquet.Write.ColumnEncryption`, `encryptPageBytes`, `encryptPageBytesV2`, `encryptAuxModule`) |
| D.47 | Parquet writer: encrypted-footer mode (PARE trailing magic) | **Done** (`Parquet.Write.FooterEncryption`, `buildParquetFileWithIndexEncryptedFooter`) |
| D.48 | Parquet writer: arbitrary nested shredding (struct / list / map / list-of-struct / list-of-list); pyarrow byte-compat | **Done** (`Parquet.Nested.shred`, `buildNestedFile`) |
| D.49 | Iceberg V3 Variant: full primitive type set (decimal / date / time / timestamp variants / uuid) | **Done** (`Iceberg.Variant`) |
| D.50 | Parquet encrypted-file reader (PARE detection + Footer module decryption + spec-compliant §5.1 length-prefixed module framing) | **Done** (`Parquet.Read.loadParquetFileEncrypted`, `Parquet.Encryption.{encryptGcmModuleFramed,readFramedModule}`) |
| D.51 | Variant column in Parquet writer (NSVariant + Iceberg.Variant.Parquet, pyarrow-verified 2-leaf binary group) | **Done** (`Iceberg.Variant.Parquet`) |
| D.52 | Iceberg V3 Variant shredding (primitive case): `routeRow` + `buildShreddedVariantParquetFile` | **Done** (`Iceberg.Variant.Shredding`) |
| D.53 | Iceberg AWS Glue catalog dialect (backend-agnostic via `GlueBackend` record; CAS-on-`metadata_location` commits via Glue UpdateTable VersionId) | **Done** (`Iceberg.Catalog.Glue`) |
| D.54 | ORC column encryption building blocks (AES-CTR stream cipher + per-stripe key derivation + protobuf encoders for `Encryption`/`EncryptionKey`/`EncryptionVariant`/`DataMask`) | **Done** (`ORC.Encryption`) |
| D.55 | Hedgehog property tests (Variant codec / Parquet encryption AAD framing / Dremel shredder invariants) | **Done** (`Test.Iceberg.{VariantProperty,EncryptionProperty,NestedProperty}`) |
| D.56 | Iceberg V3 Variant: partially-shredded object reader (union of typed + fallthrough fields) | **Done** (`Iceberg.Variant.Shredding.reconstructObjectVariant` handles the `(non-null value, non-null typed_value)` case; the primitive-shredding `reconstructVariant` deliberately rejects it per spec) |

Iceberg builds on **Parquet** (and optional other file formats); Phases A–C feed D.

---

## Execution order

1. **Parquet A.2–A.4** (correct nested + dictionary + encodings) — unblocks real
   files.
2. **Arrow B.2–B.3** — shared test vectors and column materialization.
3. **ORC C.2+** — column decoders on top of existing stripe layout.
4. **Iceberg D.2+** — table semantics on top of Parquet.

This file should be updated when a milestone lands (tests + exports + README
synopsis if user-visible).
