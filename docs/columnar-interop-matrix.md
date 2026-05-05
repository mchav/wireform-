# Columnar interop matrix

What's tested against what, as of this PR. Each row is one
direction (wireform → engine, or engine → wireform); a "✓ N/M"
cell means "N of M test files in this matrix pass through the
listed engine without modification".

## Apache Parquet

|                                  | pyarrow 24 | duckdb 1.5 | polars 1.40 | arrow-rs 53 |
| -------------------------------- | :--------: | :--------: | :---------: | :---------: |
| **wireform → engine** (10 files) |  ✓ 10/10   |  ✓ 10/10   |   ✓ 10/10   |   ✓ 10/10   |
| **engine → wireform** (14 files) |  ✓ 14/14   |   ✓ 3/3    |    ✓ 3/3    |     n/a     |

The wireform → engine side covers: required Int32 / Int64 /
Float / Double / Bool / ByteArray / UTF-8, ZSTD-compressed,
two-row-group, mixed required + nullable. The engine → wireform
side covers each engine's defaults plus their preferred
codecs (Snappy / ZSTD / GZip / LZ4_RAW / dictionary / V1 page /
V2 page / multi-row-group).

Drivers:

- `wireform-parquet/scripts/parquet_interop.py` (forward)
- `wireform-parquet/scripts/parquet_reverse_interop.py` (reverse)
- `interop/arrow-rs/target/release/read_parquet` (Rust forward)

## Apache ORC

|                              | pyarrow 24 | duckdb 1.5 |
| ---------------------------- | :--------: | :--------: |
| **wireform → engine** (5)    |    ✓ 5/5   |  ⚠ skipped |
| **engine → wireform** (8)    |    ✓ 8/8   |     n/a    |

DuckDB's ORC support is a community extension not bundled by
default in 1.5.x; polars doesn't ship a stable ORC reader.
pyarrow.orc wraps the official Apache C++ ORC reader so it
exercises the spec faithfully.

The reverse-direction matrix covers Snappy / ZSTD / Zlib
compression plus every primitive type our writer emits.

Drivers:

- `wireform-orc/scripts/orc_interop.py` (forward)
- `wireform-orc/scripts/orc_reverse_interop.py` (reverse)

## Apache Arrow IPC

|                              | pyarrow 24 | arrow-rs 53 |
| ---------------------------- | :--------: | :---------: |
| **wireform → engine** (26)   |   ✓ 26/26  |   ✓ 25/26   |

The 26 files cover every primitive integer width
(int8/16/32/64, uint8/16/32/64), Float / Double, Binary /
FixedSizeBinary, Date / Time / Timestamp / Duration,
Decimal128, List<int32>, Struct, dictionary-encoded UTF-8,
view types (Utf8View / ListView), RunEndEncoded, ZSTD body
compression, and the file-format (.arrow) variant.

The 1 arrow-rs failure is its own missing `Type ListView not
supported` for IPC schemas. arrow-rs reads everything else,
including the recently-added utf8view (which had a wireform
alignment bug arrow-rs caught — see commit
"FlatBuffers / Arrow IPC: fix i64 vector alignment for arrow-rs").

Drivers:

- `wireform-arrow/scripts/pyarrow_interop.py`
- `interop/arrow-rs/target/release/read_arrow_ipc`

## Replication

```bash
# system deps
apt install liblz4-dev libsnappy-dev libzstd-dev   # ubuntu
brew install lz4 snappy zstd                       # macos
pip install pyarrow duckdb polars

# build
cabal build all
( cd interop/arrow-rs && cargo build --release )

# Parquet (forward + reverse)
python3 wireform-parquet/scripts/parquet_interop.py
python3 wireform-parquet/scripts/parquet_reverse_interop.py

# ORC (forward + reverse)
python3 wireform-orc/scripts/orc_interop.py
python3 wireform-orc/scripts/orc_reverse_interop.py

# Arrow IPC
python3 wireform-arrow/scripts/pyarrow_interop.py

# Rust side: feed the wireform probe outputs to arrow-rs / parquet-rs
mkdir -p /tmp/wf-pq /tmp/wf-arrow
cabal run wireform-parquet:wireform-parquet-interop-probe -- /tmp/wf-pq
cabal run wireform-arrow:wireform-arrow-pyarrow-probe   -- /tmp/wf-arrow
./interop/arrow-rs/target/release/read_parquet /tmp/wf-pq
./interop/arrow-rs/target/release/read_arrow_ipc /tmp/wf-arrow
```

## Bugs surfaced (and fixed) by the probes

Each one was a real bug in wireform that no internal test
exercised; the probes caught them by feeding our output into
the Apache reference readers.

1. **`utf8_required.parquet` was rejected by every reader**.
   Cause: the probe used a bare `ByteString` literal for the
   row containing `"Δοε"`. ByteString's `IsString` truncates
   each `Char` to its low 8 bits, silently mangling
   non-ASCII strings. (Probe-side bug.)
2. **Default builds couldn't read what every other engine
   writes by default.** The `snappy` / `zstd` / `lz4` Cabal
   flags were `default: False`. pyarrow defaults to Snappy,
   duckdb to Snappy, polars to ZSTD. Flipped to `default:
   True` on Parquet; added the same flags to ORC (which was
   missing snappy + zstd entirely).
3. **`ColumnMetaData.dictionary_page_offset` was wired to
   the wrong thrift field number** (10 instead of 11; field
   10 is `index_page_offset`). Result: the offset was always
   `Nothing` after decode, so `columnChunkSlice` missed the
   dictionary page and any RLE_DICTIONARY data page failed
   with "RLE_DICTIONARY page before dictionary page".
4. **Dictionary page header rejected legacy
   `PLAIN_DICTIONARY` encoding.** pyarrow's V1 writer emits
   dictionary pages with `PLAIN_DICTIONARY = 2`; both `PLAIN`
   and `PLAIN_DICTIONARY` are spec-valid for the dict page
   itself.
5. **BOOLEAN with `RLE` encoding (encoding 3) wasn't
   supported.** V2 BOOLEAN columns use this encoding;
   previously the dispatcher returned "unsupported encoding
   3".
6. **LZ4_RAW + ORC LZ4 went through a wire-incompatible
   `lz4` Hackage package.** That package framed every block
   in an 8-byte size header that no other Parquet / ORC
   reader expects. Replaced with direct FFI bindings to
   `liblz4` (`Columnar.LZ4`).
7. **ORC `StripeFooter.columns` field was always empty.**
   Every official ORC reader rejects this with "bad number
   of ColumnEncodings in StripeFooter: expected=N, actual=0".
   Fixed to emit one `ColumnEncoding` per column (DIRECT for
   Struct / Map / List / Union; DIRECT_V2 otherwise).
8. **ORC postscript magic was required to be `"ORC"`.** The
   field is optional in the spec; pyarrow / arrow-cpp omit
   it because the leading "ORC" file header is canonical.
9. **ORC footer + stripe footers weren't decompressed.** The
   spec wraps both in the file's compression envelope; we
   were reading the raw bytes and the protobuf decoder
   choked on the high-bit byte from the compressed stream.
10. **Arrow IPC `[long]` vectors had wrong element
    alignment.** The variadic-buffer-counts vector for view
    types put i64 elements at file position `≡ 4 mod 8` —
    arrow-rs's verifier rejected it. Generic `prepForObject`
    accounted for the u32 length prefix incorrectly. Fixed
    with a vector-aware `prepForVector` helper.
