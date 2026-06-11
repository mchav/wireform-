{- |
Module      : Kafka.Compression.Gzip
Description : Gzip compression implementation for Kafka
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

Gzip compression implementation using the zlib library.
Gzip provides good compression ratios with moderate CPU usage.

Kafka uses standard RFC 1952 gzip format for compression.
-}
module Kafka.Compression.Gzip (
  compressGzip,
  compressGzipWithLevel,
  decompressGzip,
  defaultGzipLevel,
) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL


{- | Default gzip compression level (6 = balanced).
Levels range from 0 (no compression) to 9 (maximum compression).
-}
defaultGzipLevel :: Int
defaultGzipLevel = 6


-- | Compress data using gzip with default compression level.
compressGzip :: ByteString -> Either String ByteString
compressGzip = compressGzipWithLevel defaultGzipLevel


{- | Compress data using gzip with specified compression level.
Level must be 0-9 (0=no compression, 1=fastest, 6=default, 9=best compression).
-}
compressGzipWithLevel :: Int -> ByteString -> Either String ByteString
compressGzipWithLevel level bs =
  Right $ BL.toStrict $ GZip.compressWith params $ BL.fromStrict bs
  where
    params =
      GZip.defaultCompressParams
        { GZip.compressLevel = GZip.compressionLevel level
        }


-- | Decompress gzip-compressed data.
decompressGzip :: ByteString -> Either String ByteString
decompressGzip bs =
  Right $ BL.toStrict $ GZip.decompress $ BL.fromStrict bs
