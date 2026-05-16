# wireform-orc

[![BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)


> [!CAUTION]
> wireform is in heavy development and has not been published to Hackage yet. APIs may change.

[Apache ORC](https://orc.apache.org/) for Haskell. The postscript and
footer ([`ORC.Footer`](src/ORC/Footer.hs),
[`ORC.Types`](src/ORC/Types.hs),
[`ORC.Proto.Schema`](src/ORC/Proto/Schema.hs)), the per-stripe
columnar reader / writer ([`ORC.Stripe`](src/ORC/Stripe.hs),
[`ORC.RowIndex`](src/ORC/RowIndex.hs),
[`ORC.BloomFilter`](src/ORC/BloomFilter.hs)),
the run-length encoding family in `ORC.RLE`,
the columnar entry points
([`ORC.Read`](src/ORC/Read.hs),
[`ORC.Write`](src/ORC/Write.hs),
[`ORC`](src/ORC.hs)), an Arrow bridge
([`ORC.Arrow`](src/ORC/Arrow.hs)), aggregate and predicate pushdown
([`ORC.Aggregate`](src/ORC/Aggregate.hs),
[`ORC.Statistics`](src/ORC/Statistics.hs)), column-level encryption
([`ORC.Encryption`](src/ORC/Encryption.hs)), and an annotation-driven
deriver ([`ORC.Derive`](src/ORC/Derive.hs)).

ORC is the columnar file format that came out of Hortonworks' work
on Hive, sized for the same workloads Parquet targets. The on-disk
layout is similar in spirit (column-by-column storage with stripe-
level statistics, lightweight encodings, optional compression on top)
but differs in detail: ORC predates Parquet's RLE family, has its
own RLE v2 encoding optimised for integer columns, supports a
per-row-index granularity for finer-grained pushdown, and uses
protobuf for its metadata instead of Thrift. Hive, Trino, and Spark
read both formats; ORC tends to win on integer-heavy workloads,
Parquet on text-heavy ones.

This package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo and shares its allocation primitives, annotation deriver, and
testing discipline with every other format.

## Install

```cabal
build-depends:
  base,
  wireform-orc,
  wireform-arrow,       -- for the Arrow bridge / typed record surface
  wireform-columnar,    -- iterator surface + predicate vocabulary
  wireform-derive,      -- only if you want the cross-format annotation deriver
```

The package supports three compression flags. Snappy, Zstandard, and
LZ4 (block format) are all on by default because virtually every ORC
file in the wild uses one of them; the only common compression an
ORC file in the wild has *not* seen is "no compression":

```cabal
flags: +snappy +zstd +lz4
```

Disable individually with `-f-snappy`, `-f-zstd`, `-f-lz4` if you
want a smaller dep tree.

The package is part of the [wireform](https://github.com/iand675/wireform-)
monorepo. Clone the repo and `cabal build wireform-orc` to compile
locally.

## Hello world

```haskell
import qualified Data.ByteString as BS
import qualified ORC

main :: IO ()
main = do
  bytes <- BS.readFile "data.orc"
  case ORC.decodeORC bytes of
    Right footer -> putStrLn $ "rows=" ++ show (ORC.footerNumRows footer)
                            ++ " stripes=" ++ show (length (ORC.footerStripes footer))
    Left  err    -> putStrLn err
```

For end-to-end column reads, the `Wireform.Columnar` facade in the
umbrella package layers a uniform Arrow-shaped surface over ORC,
Parquet, and Arrow IPC.

## What's in here

| Module                | Role                                                      |
|-----------------------|-----------------------------------------------------------|
| `ORC.Types`           | ORC schema and metadata AST (`ORCFooter`, `Postscript`, `StripeInformation`, `ORCType`, `ColumnEncoding`) |
| `ORC.Proto.Schema`    | Protobuf binding for the ORC metadata schema (ORC's footer is protobuf-encoded, in contrast to Parquet's Thrift-encoded footer) |
| `ORC.Footer`          | `readFooter` / `writeFooter`: parse and emit the postscript-prefixed protobuf-encoded footer |
| `ORC.Stripe`          | Per-stripe reader: per-column index streams, data streams, dictionary streams |
| `ORC.RowIndex`        | Row-index reader (per-row-group statistics inside a stripe; ORC's sub-stripe pushdown granularity) |
| `ORC.BloomFilter`     | Per-column bloom filter (`decodeBloomFilter`, `bfCheckBytes`, `bfCheckLong`) |
| `ORC.Compress`        | Per-stream chunk-header decoder, codec dispatch (each compression behind its own Cabal flag) |
| `ORC.Encryption`      | Column-level encryption (per the ORC encryption spec) |
| `ORC.Read`            | High-level reader: path + handle helpers, `decompressORCStreamSized` |
| `ORC.Write`           | Stripe writer + footer serializer |
| `ORC`                 | Top-level entry: `decodeORC`, `defaultWriteOptions` |
| `ORC.Aggregate`       | Statistics-driven aggregate pushdown: `count` / `min` / `max` / `sum` directly from stripe and row-index statistics |
| `ORC.Statistics`      | Predicate evaluator over column / stripe / row-index statistics + bloom filters |
| `ORC.Arrow`           | Bridge between ORC columns and Arrow `ColumnArray`. `streamStripesFilteredIter`, `streamStripesProjectedFilteredIter`. |
| `ORC.Derive`          | `deriveORC` Template Haskell entry point |

## RLE

`ORC.RLE` (an internal module exposed indirectly through the column
readers) covers all three of ORC's run-length encoding variants:

- v1: simple run / literal alternation, used in older files.
- v2: the four-mode encoding (Short Repeat / Direct / Patched Base
  / Delta) optimised for integer columns. Most ORC files in the
  wild use it.
- Boolean RLE: the bit-packed encoding for boolean columns and the
  present streams that carry validity bitmaps.

## Predicate pushdown

`ORC.Statistics` evaluates a `Columnar.Predicate.Predicate` against:

- per-stripe column statistics from the footer,
- per-row-index column statistics inside each stripe,
- per-column bloom filters where present.

Combined with `ORC.Arrow.streamStripesFilteredIter` /
`streamStripesProjectedFilteredIter` (and the
`Wireform.Columnar.decodeFilteredIter` / `decodeProjectedFilteredIter`
facade in the umbrella package) this lets a query skip whole stripes
or sub-stripe row ranges without reading their column data.

`ORC.Aggregate` short-circuits the entire scan for `count` / `min` /
`max` / `sum` queries that can be answered from stripe statistics
alone.

## Arrow bridge

`ORC.Arrow` converts ORC column reads to Arrow `ColumnArray`s. The
`Wireform.Columnar` facade in the umbrella package layers a uniform
Arrow-shaped API on top of all three columnar formats, so the same
caller code can switch between Parquet, ORC, and Arrow IPC by
changing the `Format` argument.

## Annotation-driven deriving

`ORC.Derive` consumes the cross-format `Wireform.Derive.Modifier`
vocabulary from [`wireform-derive`](../wireform-derive/README.md):

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified ORC.Derive as DOrc
import Wireform.Derive (renameStyle, SnakeCase)

data Event = Event
  { eventId        :: !Int64
  , eventTimestamp :: !Int64
  , eventPayload   :: !Text
  } deriving stock (Show, Eq, Generic)

{-# ANN type Event ("Event" :: String) #-}
{-# ANN eventId        (renameStyle SnakeCase) #-}
{-# ANN eventTimestamp (renameStyle SnakeCase) #-}
{-# ANN eventPayload   (renameStyle SnakeCase) #-}

DOrc.deriveORC ''Event
```

## Testing

The per-format Hedgehog + HUnit suites cover footer round-trips,
every RLE variant, every supported compression codec (when its flag
is enabled), the row-index reader, the bloom filter, the predicate
evaluator, the aggregate short-circuit, and the Arrow bridge:

```bash
cabal test wireform-orc:wireform-orc-test
cabal test wireform-orc:wireform-orc-derive-test
```

## Benchmarks

No per-package criterion harness in tree yet. Planned comparisons:

- Haskell: no comparable ORC reader on Hackage; the natural
  baseline is the `wireform-arrow` IPC reader and the
  `wireform-parquet` reader on equivalent inputs.
- Java: the [Apache ORC reference Java library](https://github.com/apache/orc),
  used by Hive / Spark.
- C++: the [Apache ORC reference C++ library](https://github.com/apache/orc/tree/main/c%2B%2B).
- Rust: [`orc-rust`](https://github.com/datafusion-contrib/orc-rust),
  used by datafusion-orc.

> Numbers TBD: harness pending.

## License

BSD-3-Clause.

## References

- [Apache ORC specification](https://orc.apache.org/specification/)
- [Apache ORC project](https://orc.apache.org/)
- [ORC encryption specification](https://orc.apache.org/specification/ORCv2/#encryption)
