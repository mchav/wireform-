{-# LANGUAGE LambdaCase #-}

{- | Memory-mapped file loading for columnar formats.

The Parquet and ORC readers consume a 'ByteString' for the
whole file. For multi-GB files that's wasteful: every byte
gets paged into the GC heap even if the reader only touches
the footer + a few row groups. Memory-mapping defers the
page-in to the kernel: the 'ByteString' is backed by the
mmap region, slices into it are pointer-arithmetic, and
only the bytes the reader actually touches are paged in.

This module wraps "System.IO.MMap" so callers don't need to
decide between @Data.ByteString.readFile@ (eager,
heap-allocated) and @mmapFileByteString@ (lazy, kernel-paged).
The choice happens in 'loadFile' based on file size.

== Caveats

* The returned 'ByteString' is only valid until its
  finalizer runs (which is when the @ForeignPtr@ behind it
  is no longer referenced); be careful about retaining
  slices longer than the parent 'ByteString'. The Parquet
  and ORC readers naturally hold the parent for the
  duration of any decode.

* Truncating the underlying file while the mapping exists
  makes accesses past the new EOF raise SIGBUS. Read paths
  shouldn't observe this in practice.

* mmap of files >2GB on a 32-bit system fails; we run on
  64-bit only.
-}
module Columnar.IO (
  loadFile,
  loadFileMmap,
  loadFileEager,
  LoadStrategy (..),
  defaultLoadStrategy,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import System.Directory (getFileSize)
import System.IO.MMap (mmapFileByteString)


-- | When to use mmap vs an eager read.
data LoadStrategy
  = -- | Always memory-map. Cheapest for any size; slightly
    -- slower than 'AlwaysEager' for very small files (<1KB)
    -- because of the syscall + mapping overhead.
    AlwaysMmap
  | -- | Always 'BS.readFile'. Predictable behaviour, no
    -- mapping lifetime to think about; suboptimal for large
    -- files because the entire file gets pulled into the GC
    -- heap up front.
    AlwaysEager
  | -- | mmap files larger than the given threshold (bytes),
    -- 'BS.readFile' below. The default 'defaultLoadStrategy'
    -- threshold is 64KiB — large enough that the mapping
    -- amortises but small enough that any \"real\" Parquet
    -- file qualifies.
    MmapAbove !Int
  deriving (Show, Eq)


{- | Default strategy: mmap files \>= 64 KiB. Picked so any
footer-bearing columnar file (every Parquet file is at
least a few hundred bytes; real ones are at least kilobytes)
gets the mmap path while ad-hoc test fixtures stay eager.
-}
defaultLoadStrategy :: LoadStrategy
defaultLoadStrategy = MmapAbove 65536


{- | Load a file as a 'ByteString' using 'defaultLoadStrategy'.
Equivalent to @loadFileWith defaultLoadStrategy@.
-}
loadFile :: FilePath -> IO ByteString
loadFile = loadFileWith defaultLoadStrategy


{- | Always-mmap convenience wrapper. Use when you know you'll
only touch a small portion of a large file (e.g. just the
footer).
-}
loadFileMmap :: FilePath -> IO ByteString
loadFileMmap path = mmapFileByteString path Nothing


{- | Always-eager convenience wrapper. Use when you know
you're going to consume the entire file anyway (e.g.
decoding a small Arrow IPC message into a single record
batch you'll keep around).
-}
loadFileEager :: FilePath -> IO ByteString
loadFileEager = BS.readFile


loadFileWith :: LoadStrategy -> FilePath -> IO ByteString
loadFileWith strat path = case strat of
  AlwaysMmap -> loadFileMmap path
  AlwaysEager -> loadFileEager path
  MmapAbove n -> do
    -- Stat the file once to decide; the syscall is cheap
    -- compared to the eager read it lets us skip.
    sz <- getFileSize path
    if sz >= fromIntegral n
      then loadFileMmap path
      else loadFileEager path
