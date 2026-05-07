# Migration: lazy lists → `Iter`

The original streaming surface for the columnar formats
returned `[Either String (V.Vector ColumnArray)]` — a lazy
list with errors threaded through every element. That shape
is gone; everything now uses `Columnar.Stream.Iter`.

This guide shows the mechanical translation.

## The old shape

```haskell
import qualified Parquet.Arrow as PArrow

-- old
let results = PArrow.streamRowGroups arrowSchema pf
forM_ results $ \r -> case r of
  Right cols -> consume cols
  Left  err  -> log err
```

Issues:
- A decode error in row group 50 forces the consumer to
  pattern-match every cell.
- Lazy-list spine retention pinned every later row group in
  memory until garbage-collected.
- Consumers couldn't easily short-circuit (take 5; foldM with
  early exit).

## The new shape

```haskell
import qualified Parquet.Arrow as PArrow
import qualified Columnar.Stream as IS

-- new
let it = PArrow.streamRowGroupsIter arrowSchema pf

-- option A: drain to a list (errors halt at the failing step)
case IS.iterToList it of
  Right batches -> mapM_ consume batches
  Left  err     -> log err

-- option B: pull one at a time
let go i = case IS.iterStep i of
      Right (IS.IterYield cols rest) -> consume cols >> go rest
      Right IS.IterDone              -> done
      Left e                         -> log e
go it

-- option C: short-circuit fold
case IS.iterFold (+) 0 (IS.iterMap (\cs -> AC.columnLength (V.head cs)) it) of
  Right total -> putStrLn $ "rows: " ++ show total
  Left  e     -> log e
```

## Translation table

| Old                                           | New                                     |
|-----------------------------------------------|-----------------------------------------|
| `streamRowGroups`                             | `streamRowGroupsIter`                   |
| `Arrow.Stream.streamReaderToList`             | `streamReaderIter` + `iterToList`       |
| `forM_ rs (\r -> case r of …)`                | `iterToList` + `mapM_`                  |
| `take n (lefts ++ rights)`                    | `iterTake n` (errors halt naturally)    |
| `foldr (\r acc -> …) seed`                    | `iterFold` (strict left fold)           |
| `[Either String a]` from a generator          | `iterFromIndexed n decodeIdx`           |
| Schema projection: external filter then list  | `streamRowGroupsProjectedIter`          |
| Predicate pushdown                            | `streamRowGroupsFilteredIter`           |

## What's gone

- `Parquet.Arrow.streamRowGroups` still exists (returns the
  old lazy-list shape) for callers mid-migration; new code
  should reach for the `Iter` variant.
- `Arrow.Stream.streamReaderToList` likewise stays as a
  drain helper; it now returns a strict list under the hood.

## Common patterns

### Decoding records

```haskell
import qualified Wireform.Columnar as Col

case Col.decodeRecordsIter Col.Parquet Col.defaultReadOptions
       tradeTable bytes of
  Right it -> IS.iterForM_ it consumeBatch
  Left  e  -> log e
```

This auto-projects the source file to just the columns the
`Table`'s `RowDecoder` consults — wide files become column-
count linear instead of row-count linear.

### Streaming from disk

```haskell
import qualified Parquet.Read as PR

bracket (PR.openParquetReader path) (\_ -> pure ()) $ \res ->
  case res of
    Right (pf, rgIter) ->
      let !sch  = PArrow.parquetFileArrowSchema pf
          decode i = PArrow.parquetRowGroupToArrow sch pf i
                       & either (Left . show) Right
          batches = IS.iterMapM decode (iterIOToIter rgIter)
      in IS.iterForM_ batches consume
    Left e -> log e
```

`openParquetReader` reads the footer once, then yields row-
group indices on demand from a handle-backed `IterIO`. The
per-group decode happens lazily inside `iterMapM`.

### Concurrent prefetch

```haskell
prefetched <- IS.iterIOPrefetch 4 rgIter
```

Pulls the next 4 row groups in a worker thread; consumer
back-pressure caps in-flight work. Typical 2-4× throughput
on multi-row-group reads.
