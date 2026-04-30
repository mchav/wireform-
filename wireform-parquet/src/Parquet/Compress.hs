{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Page-data compression for the Parquet writer.
--
-- The reader's 'Parquet.Read.decompressChunk' supports @Uncompressed@,
-- @GZip@, @Snappy@ (with @-fsnappy@), @ZSTD@ (with @-fzstd@), and
-- @LZ4_RAW@ (with @-flz4@). This module exposes the symmetric writer
-- side: 'compressPageBytes' takes a 'Compression' codec and a page body
-- and returns the compressed bytes.
--
-- The codec set behind cabal flags is identical to the reader's; if the
-- user requests a codec that wasn't enabled at build time, the writer
-- returns @Left@ rather than silently emitting uncompressed data.
module Parquet.Compress
  ( compressPageBytes
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZip

#ifdef HAVE_SNAPPY
import qualified Codec.Compression.Snappy as Snappy
#endif

#ifdef HAVE_ZSTD
import qualified Codec.Compression.Zstd as Zstd
#endif

#ifdef HAVE_LZ4
import qualified Codec.Compression.LZ4 as LZ4
#endif

import Parquet.Types (Compression (..))

-- | Compress a page-body byte string with the given codec.
--
-- @Uncompressed@ is always available; @GZip@ uses the built-in @zlib@
-- dependency; @Snappy@\/@ZSTD@\/@LZ4@ require the matching @-f@ flag at
-- build time. Returns @Left@ for codecs we cannot encode (e.g. the
-- deprecated Hadoop-flavoured @LZ4@).
compressPageBytes :: Compression -> ByteString -> Either String ByteString
compressPageBytes Uncompressed bs = Right bs
compressPageBytes GZip bs =
  Right (BL.toStrict (GZip.compress (BL.fromStrict bs)))

#ifdef HAVE_SNAPPY
compressPageBytes Snappy bs = Right (Snappy.compress bs)
#else
compressPageBytes Snappy _ =
  Left "Parquet.Compress: Snappy requires building wireform with -fsnappy"
#endif

#ifdef HAVE_ZSTD
compressPageBytes ZSTD bs =
  Right (Zstd.compress 3 bs)  -- level 3 is the parquet-mr default.
#else
compressPageBytes ZSTD _ =
  Left "Parquet.Compress: ZSTD requires building wireform with -fzstd"
#endif

#ifdef HAVE_LZ4
compressPageBytes LZ4Raw bs =
  case LZ4.compress bs of
    Just out -> Right out
    Nothing  -> Left "Parquet.Compress: LZ4_RAW compression failed"
#else
compressPageBytes LZ4Raw _ =
  Left "Parquet.Compress: LZ4_RAW requires building wireform with -flz4"
#endif

compressPageBytes LZ4 _ =
  Left $
    "Parquet.Compress: deprecated Hadoop LZ4 (codec 5) is not supported; "
    ++ "use LZ4_RAW (codec 7) with -flz4"

compressPageBytes Brotli _ =
  Left "Parquet.Compress: Brotli is not yet implemented"

compressPageBytes LZO _ =
  Left "Parquet.Compress: LZO is not yet implemented"
