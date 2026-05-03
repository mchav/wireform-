# Feature Gap Analysis: wireform vs Other Protobuf Libraries

Analysis comparing wireform against proto-lens (Haskell), prost (Rust), and the
official Google protobuf runtimes (Java, Python, Go, C++).

## Implementation Status

| # | Feature | Status | Module(s) |
|---|---------|--------|-----------|
| 1 | gRPC Service Stub Generation | **Implemented** | `Proto.CodeGen.Service` |
| 2 | protoc Plugin Mode | **Implemented** | `Proto.Google.Protobuf.Compiler.Plugin`, `protoc-gen-wireform` exe |
| 3 | Protobuf Editions Support | **Implemented** | `Proto.AST` (Editions, FeatureSet), `Proto.Parser` |
| 4 | Full descriptor.proto Types | **Implemented** | `Proto.Google.Protobuf.Descriptor` |
| 5 | Text Format (pbtxt) | **Implemented** | `Proto.TextFormat` |
| 6 | Dynamic Messages | **Implemented** | `Proto.Dynamic` |
| 7 | Conformance Test Harness | **Implemented** | `Proto.Conformance`, `wireform-conformance` exe |
| 8 | Streaming/Incremental Decode | **Implemented** | `Proto.Decode.Stream`, `Proto.Encode.Lazy` |
| 9 | Well-Known Type JSON | **Implemented** | `Proto.JSON.WellKnown` |
| 10 | aeson Integration | **Implemented** | `Proto.JSON.Aeson` |
| 11 | Group Field Handling | **Already implemented** | `Proto.Wire.Decode.skipGroup` |
| 12 | Custom Option Extensions | **Implemented** | `Proto.Options.Custom` |
| 13 | Map/Oneof in TH Codegen | **Implemented** | `Proto.TH` |
| 14 | Enum Alias Codegen | **Implemented** | `Proto.CodeGen` |

### Details

**1. gRPC Service Stub Generation** — The code generator now emits service
declarations instead of discarding them. `Proto.CodeGen.Service` generates:
- Server handler record types (one method handler field per RPC)
- Client stub record types
- Method metadata types with names and streaming modes
- Service metadata (fully-qualified names, method paths)

**2. protoc Plugin Mode** — `Proto.Google.Protobuf.Compiler.Plugin` provides
`CodeGeneratorRequest`/`CodeGeneratorResponse` types with full encode/decode,
plus a `pluginMain` entry point. The `protoc-gen-wireform` executable
implements the protoc plugin protocol.

**3. Protobuf Editions** — The AST now has `Editions Edition` as a `Syntax`
variant. The parser handles `edition = "2023"` syntax. `FeatureSet` and all
feature enum types are defined with `featuresForEdition` defaults.

**4. descriptor.proto Types** — Hand-written Haskell types for the core
descriptor hierarchy: `FileDescriptorSet`, `FileDescriptorProto`,
`DescriptorProto`, `FieldDescriptorProto`, `EnumDescriptorProto`,
`ServiceDescriptorProto`, `MethodDescriptorProto`, `OneofDescriptorProto`.

**5. Text Format** — `Proto.TextFormat` provides rendering and parsing of
the protobuf text format (pbtxt) for dynamic messages, with both compact
and pretty-printed output modes.

**6. Dynamic Messages** — `Proto.Dynamic` provides `DynamicMessage` backed
by `Map Int DynamicValue`, with wire format encode/decode and JSON conversion.

**7. Conformance Test Harness** — `Proto.Conformance` implements the
length-prefixed stdin/stdout protocol for the official conformance runner.

**9. Well-Known Type JSON** — Canonical JSON conversions:
- Timestamp ↔ RFC 3339 strings
- Duration ↔ "3.5s" format
- FieldMask ↔ comma-separated paths
- Struct/Value/ListValue ↔ native JSON

**10. aeson Integration** — Full aeson integration. Generated types get
`ToJSON`/`FromJSON` instances directly. The library depends on aeson.

**12. Custom Option Extensions** — `CustomOptionRegistry` for tracking and
resolving custom option extensions, with extraction from parsed proto files.

**13. Map/Oneof in TH** — The TH code generator now handles `MEMapField`
(generating `Map` fields) and `MEOneof` (generating `Maybe`-wrapped sum types).

**14. Enum Alias** — The code generator detects `allow_alias` enums, generates
only primary constructors with pattern synonyms for aliases, and skips
deriving `Enum` for non-sequential numbering.

### All features implemented

All originally-identified feature gaps have been closed, including:

**8. Streaming/Incremental Decode** — Implemented in `Proto.Decode.Stream`
(incremental/resumable decoder) and `Proto.Encode.Lazy` (push-based encoder).

## Columnar / lake-format gap closures

These items were tracked in [`docs/columnar-roadmap.md`](docs/columnar-roadmap.md):

| # | Feature | Module(s) |
|---|---------|-----------|
| P-A.7a | Parquet page index (`OffsetIndex` / `ColumnIndex`) | `Parquet.PageIndex` |
| P-A.7b | Parquet bloom filter (split-block, XXH64) | `Parquet.BloomFilter`, `Parquet.XXH64` |
| P-A.8  | DELTA_LENGTH_BYTE_ARRAY / DELTA_BYTE_ARRAY / BYTE_STREAM_SPLIT / RLE_DICTIONARY decoders | `Parquet.Delta`, `Parquet.Read` |

Page-index pointers (offset, length) and bloom-filter pointers
(`bloom_filter_offset`, `bloom_filter_length`) are round-tripped through
`ColumnChunk` / `ColumnMetadata` so existing Parquet files written by
arrow-cpp, parquet-mr, and pyarrow are recognised on read.

### Still planned

(Nothing on the original gap list - all entries below now have working
implementations.)

- ~~Parquet encryption (modular encryption, footer key wrapping).~~ **Done**
  (`Parquet.Encryption` AES-GCM-V1 + AES-GCM-CTR-V1 with full AAD framing;
  Iceberg `tmEncryptionKeys` wires through `Iceberg.Parquet.encryptionConfigFromTable`).
- ~~Parquet writer integration that emits page-index + bloom-filter footers
  alongside row groups.~~ **Done** (`buildParquetFileWithIndex` +
  `ColumnAux`).
- ~~ORC writer (C.6) and timestamp/decimal/date column write path (C.5).~~
  **Done** (`encodeDateColumn` / `encodeTimestampColumn` /
  `encodeDecimalColumn` / `encodeDecimalRawColumn`).

### Iceberg parity status (updated)

`wireform-iceberg` is feature-complete vs. the Java/PyIceberg/iceberg-rust/
iceberg-go SDKs across all three Iceberg spec versions:

| Capability | Coverage | Module(s) |
|---|---|---|
| Table metadata read/write (v1/v2/v3 fields) | full | `Iceberg.Types`, `Iceberg.JSON`, `Iceberg.Write` |
| Manifest / manifest-list read | full incl. partition summaries, key_metadata, first_row_id, sequence-number inheritance | `Iceberg.Read` |
| Manifest / manifest-list write (with canonical Iceberg field-id annotations) | full incl. column stats + delete-vector pointers + per-spec partition summaries | `Iceberg.Write` |
| Schema evolution (lookups, projection, name mapping, valid-promotion rules) | full | `Iceberg.SchemaEvolution`, `Iceberg.JSON`, `Iceberg.SchemaCompat` |
| Partition / sort transforms (Identity, Bucket, Truncate, Y/M/D/H, Void) | full + Murmur3 | `Iceberg.Murmur3`, `Iceberg.Transform` |
| Row -> partition tuple, predicate -> partition predicate projection | full | `Iceberg.Partition` |
| Sort key evaluation (per-row sort key with NullsFirst/Last + Asc/Desc) | full | `Iceberg.Sort` |
| Per-column metrics modes (none / counts / truncate(N) / full) | full | `Iceberg.MetricsConfig` |
| Lower/upper bound truncation (string + binary) | full | `Iceberg.BoundTrunc` |
| Predicate AST + manifest pruning (inclusive + strict) incl. IN, NOT IN, STARTS_WITH, NOT_STARTS_WITH | full | `Iceberg.Expression`, `Iceberg.Read.planScanWithFilter` |
| Time-travel scan: planScanAtSnapshot, planScanAsOfTime | full | `Iceberg.Read` |
| Snapshot history helpers: ancestorsOf, currentAncestors, snapshotsBetween, isAncestor, snapshotByRef, snapshotAsOfTime | full | `Iceberg.Snapshot` |
| Position + equality delete files; sequence numbers (incl. inheritance) | full | `Iceberg.Read`, `Iceberg.Snapshot` |
| Branch / tag refs, fast-forward, rollback (with ancestor validation) | full | `Iceberg.Update` |
| AppendFiles / OverwriteFiles / RowDelta commit semantics | full | `Iceberg.Update` |
| Snapshot summary auto-computation (added-/total- data files / records / sizes / position+equality deletes) | full | `Iceberg.Update` (`SnapshotStats`, `autoSummary`) |
| Iceberg View spec | full read/write | `Iceberg.Types`, `Iceberg.JSON`, `Iceberg.View`, `Iceberg.Write` |
| Statistics / partition-statistics file references | full | `Iceberg.Types`, `Iceberg.JSON` |
| V3 deletion vectors (Puffin Roaring64) | full | `Iceberg.Puffin`, `Iceberg.DeletionVector` |
| V3 nanosecond / variant / geometry / geography / unknown types | full (JSON only) | `Iceberg.Types`, `Iceberg.JSON` |
| V3 default values, row lineage, encryption keys | full (types + JSON) | `Iceberg.Types`, `Iceberg.JSON` |
| Identifier-field-ids validation; v1/v2/v3 table-metadata constraint validation | full | `Iceberg.Validate` |
| REST catalog request/response shapes and JSON + exception type | full | `Iceberg.Catalog.REST` |
| `WRITE_METADATA_COMPRESSION` (gzip metadata.json.gz) | full | `Iceberg.Write` (`encodeTableMetadataCompressed`) |
| Fast-append / merge-append / rewrite-manifests planner (bin-packed by `commit.manifest.target-size-bytes` / `min-count-to-merge`) | full | `Iceberg.ManifestMerge` |
| C/SIMDe kernels for hot paths (Murmur3-32 / `bucket[N]`, XXH64, Roaring 32-bit decode/encode/contains, V3 deletion-vector membership) | 3×–53× faster than the pure references on the bench (`bench/RESULTS.md`) | `Wireform.Hash` (single canonical home), `wireform-core/cbits/wireform_hash_simd.c` |
| Parquet bloom filter on the SIMD XXH64 | 2.6×–7.1× faster than the pure path on 64 B–64 KiB inputs (`wireform-parquet/bench`) | `Parquet.BloomFilter` (uses `Wireform.Hash.xxh64` directly) |
| Iceberg ↔ Parquet bridge (writer + scan side) | full | `Iceberg.Parquet` |
| Iceberg REST catalog HTTP client | full (behind `-frest-client` flag) | `Iceberg.Catalog.REST.Client` |
| Parquet writer with page-index + bloom-filter + column-index footers | full | `Parquet.Write.buildParquetFileWithIndex` + `ColumnAux` |
| Parquet modular encryption (AES-GCM-V1 + AES-GCM-CTR-V1, full AAD framing) | full | `Parquet.Encryption` |
| Iceberg `tmEncryptionKeys` -> Parquet `EncryptionConfig` wiring | full | `Iceberg.Parquet.encryptionConfigFromTable` / `withEncryptionKeyMetadata` |
| ORC date / timestamp / decimal writers | full | `ORC.Write.encodeDateColumn` / `encodeTimestampColumn` / `encodeDecimalColumn` / `encodeDecimalRawColumn` |
| Parquet writer: all primitive types (Int32 / Int64 / Float / Double / Bool / ByteArray) | full | `Parquet.Write.buildParquetFile` + `ColumnData` |
| Parquet writer: per-column compression (Uncompressed / GZip / Snappy / ZSTD / LZ4_RAW) | full | `Parquet.Compress`, `ColumnAux.caCodec` |
| Parquet writer: nullable columns via definition levels | full | `Parquet.LevelsEncode`, `OptionalColumn`, `encodeOptionalColumnPage` |
| Parquet writer: dictionary encoding (PLAIN_DICTIONARY + RLE_DICTIONARY) | full | `Parquet.Write.buildDictionary` / `encodeDictPage` / `encodeDictDataPage` |
| Parquet writer: DELTA_BINARY_PACKED | full | `Parquet.DeltaEncode` |
| Parquet writer: DELTA_LENGTH_BYTE_ARRAY (encoding 6) | full | `Parquet.DeltaEncode.encodeDeltaLengthByteArray` |
| Parquet writer: DELTA_BYTE_ARRAY / incremental string encoding (encoding 7) | full | `Parquet.DeltaEncode.encodeDeltaByteArray` |
| Parquet writer: BYTE_STREAM_SPLIT (encoding 9, FLOAT + DOUBLE) | full | `Parquet.ByteStreamSplit` |
| Parquet writer: DATA_PAGE_V2 (separate def/rep/data sections, header-only compression flag) | full | `Parquet.Write.encodeColumnDataPageV2` / `encodeOptionalColumnPageV2`, `ColumnAux.caPageVersion` |
| Parquet writer: SchemaElement.field_id (Iceberg leaf identification) | full | `Parquet.Types.seFieldId`, footer encode/decode |
| Iceberg position-delete file writer (fixed `file_path` + `pos` columns with reserved field-ids) | full | `Iceberg.Delete.writePositionDeleteFile` |
| Iceberg equality-delete file writer (one column per equality-id) | full | `Iceberg.Delete.writeEqualityDeleteFile` |
| Iceberg V3 multi-source-ids partition fields (multi-arg `bucket[N]` / `truncate[W]`) | full | `Iceberg.Types.PartitionField.pfSourceIds` (V1/V2 single + V3 multi unified) |
| Iceberg V3 geometry / geography column bounds (WKB POINT, 21 bytes) | full | `Iceberg.Geometry` |
| REST catalog write-side: rename, register, view CRUD, namespace property updates | full | `Iceberg.Catalog.REST.Client.renameTable` / `registerTable` / `loadView` / `createView` / `dropView` / `updateNamespaceProperties` |
| Iceberg Hadoop file-based catalog (FS-agnostic via `FileSystem` record; optimistic concurrency on `version-hint.text`) | full | `Iceberg.Catalog.Hadoop` |
| Iceberg SQL ("JDBC") catalog (backend-agnostic via `SqlBackend` record; CAS-on-`metadata_location` commits; standard `iceberg_tables` / `iceberg_namespace_properties` schema) | full | `Iceberg.Catalog.Sql` |
| Iceberg V3 Variant binary encoding (header + dictionary + value tree) + Variant ↔ JSON bridge | full (JSON-equivalent type set; decimal / temporal / UUID surface as `VUnsupportedPrimitive`) | `Iceberg.Variant` |
| Parquet writer: per-column modular encryption (AES-GCM-V1 + AES-GCM-CTR-V1 with deterministic-nonce GCM, AAD per page module) | full | `Parquet.Write.ColumnEncryption`, `columnEncryptionFor`, `encryptPageBytes`, `encryptPageBytesV2`, `encryptAuxModule` |
| Parquet writer: encrypted-footer mode (PARE trailing magic; Footer module encrypted under ModuleFooter AAD) | full | `Parquet.Write.FooterEncryption`, `buildParquetFileWithIndexEncryptedFooter` |
| Parquet writer: arbitrary nested column shredding (struct / list / map / list-of-struct / list-of-list); Dremel rep+def encoding with `NestedSchema` + `NestedRow` | full; pyarrow byte-compat verified for `list<int>`, `list<struct>`, `list<list>`, `map<string,int>` | `Parquet.Nested` |
| Iceberg.Variant: full V3 primitive type set (decimal4/8/16, date, time, timestamp(Ntz)(Nanos), uuid) | full encode/decode + canonical JSON projection | `Iceberg.Variant` |
| Parquet encrypted-file reader (PARE detection + decrypt + spec §5.1 framing) | full | `Parquet.Read.loadParquetFileEncrypted`, `Parquet.Encryption.{encryptGcmModuleFramed,readFramedModule}` |
| Variant column in Parquet writer (`{metadata, value}` 2-leaf group; pyarrow-verified) | full | `Parquet.Nested.NSVariant`, `Iceberg.Variant.Parquet.buildVariantParquetFile` |
| Iceberg V3 Variant shredding (primitive / object / array) | full per spec §Primitive/Objects/Arrays; the primitive `reconstructVariant` deliberately rejects `(non-null value, non-null typed_value)` because that combination is spec-reserved for object shredding and is handled by `reconstructObjectVariant` | `Iceberg.Variant.Shredding.{routeRow,routeObjectRow,routeArrayRow,reconstructVariant,reconstructObjectVariant,reconstructArrayVariant}` |
| Iceberg AWS Glue catalog dialect (backend-agnostic via `GlueBackend`; CAS commits) | full | `Iceberg.Catalog.Glue` |
| ORC column encryption: whole-file integration | full (AES-CTR stripe streams + `Footer.encryption` round-trip via `buildEncryptedORCFile` / `encryptStripeStreams` / `decryptStripeStream`; `ORCFooter.orcEncryption` preserves the serialized `Encryption` protobuf across read/write) | `ORC.Write`, `ORC.Footer`, `ORC.Types` |
| ORC column encryption building blocks (AES-CTR stream cipher + per-stripe key + protobuf encoders) | full (used by the whole-file integration) | `ORC.Encryption` |
| Hedgehog property tests (Variant codec / Parquet encryption AAD / nested shredder invariants) | full | `Test.Iceberg.{VariantProperty,EncryptionProperty,NestedProperty}` |
| ORC writer: per-stripe bloom filter (`BLOOM_FILTER_UTF8` stream + legacy `BLOOM_FILTER` stream) | full (selectable via `BloomFilterKind`; `BloomFilterUtf8` writes proto field 3 `utf8bitset`, `BloomFilterLegacy` writes proto field 2 unpacked `repeated fixed64`) | `ORC.BloomFilter` |
| ORC writer: per-stripe row index (`ROW_INDEX` stream) | full | `ORC.RowIndex` |
| ORC reader: DECIMAL128 stream (LEB128 zig-zag, full Integer precision) | full | `ORC.Read.decodeDecimal128Stream` |
| ORC reader + writer: DICTIONARY_V2 strings (auto-dispatches on non-empty dictionary stream; writer deduplicates to first-occurrence order) | full | `ORC.Read.decodeStringColumn`, `ORC.Write.encodeStringDictColumn`, `ORC.RLE.decodeRLEv2IntAll` |
| Parquet Brotli codec (flag-guarded under `-fbrotli`) | full (read + write) | `Parquet.Compress`, `Parquet.Read` |
| Arrow IPC writer covers every `ColumnArray` constructor (primitives + nullable primitives + struct + list + large-list + fixed-size-list + map + dense/sparse union + dictionary + interval + decimal128/256 + large-utf8/binary + fixed-size-binary) | full | `Arrow.Write.encodeCol` |
| Arrow IPC internal round-trip test suite (writer bytes → reader materializer for every `ColumnArray` shape) | full | `wireform-arrow/test/Main.hs` |
| Iceberg incremental scans (CDC / append) | full | `Iceberg.Read.planIncrementalAppend`, `planIncrementalChangelog` |
| Iceberg snapshot expiration + orphan file detection | full | `Iceberg.Maintenance.expireSnapshots` / `orphanFileCandidates` |
| End-to-end Iceberg + Parquet pipeline example | available | `examples/IcebergPipeline.hs` |
| `iceberg` CLI (metadata-show / manifest-show / expire / orphans / REST) | full | `wireform-iceberg/app/Main.hs` |
| pyarrow golden interop: writer-side bytes pyarrow can read; reader-side parses pyarrow output | proven | `wireform-parquet/test/fixtures` + `validate_writer.py` |
| Parquet `ColumnIndex.null_pages` SIMD-backed bitmap | full | `Parquet.NullPagesBitmap` |
