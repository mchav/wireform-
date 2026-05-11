{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Kafka.Compression.Types
Description : Compression codec types and utilities
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module defines the compression codec types supported by Kafka and provides
a unified interface for compressing and decompressing record batches.

Kafka supports several compression codecs:

* None (no compression)
* Gzip (RFC 1952)
* Zstd (RFC 8878) - Best compression ratio, recommended for new deployments
* Lz4 (frame format) - Fast compression/decompression
* Snappy - Fast compression (legacy, less robust Haskell support)

Each codec has different trade-offs between compression ratio, CPU usage,
and speed. Compression is applied to entire record batches, not individual records.

-}
module Kafka.Compression.Types
  ( -- * Compression Codec Type
    CompressionCodec(..)
  , codecId
  , codecName
    -- * Compression Level
  , CompressionLevel
  , defaultLevel
  , validateLevel
  , gzipLevel
  , zstdLevel
  , lz4Level
  , snappyLevel
    -- * Compression Interface
  , compress
  , compressWithLevel
  , decompress
    -- * Codec Selection
  , parseCompressionCodec
  , defaultCodec
  ) where

import Data.ByteString (ByteString)
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Kafka.Compression.Gzip as Gzip
import qualified Kafka.Compression.Zstd as Zstd
import qualified Kafka.Compression.Lz4 as Lz4
import qualified Kafka.Compression.Snappy as Snappy

-- | Kafka compression codec identifier.
-- Each codec has a numeric ID used in the wire protocol.
data CompressionCodec
  = NoCompression  -- ^ No compression (codec ID 0)
  | Gzip           -- ^ Gzip compression (codec ID 1)
  | Snappy         -- ^ Snappy compression (codec ID 2)
  | Lz4            -- ^ LZ4 compression (codec ID 3)
  | Zstd           -- ^ Zstandard compression (codec ID 4)
  deriving (Eq, Show, Ord, Generic)

-- | Get the numeric codec ID for use in the wire protocol.
codecId :: CompressionCodec -> Int8
codecId NoCompression = 0
codecId Gzip = 1
codecId Snappy = 2
codecId Lz4 = 3
codecId Zstd = 4

-- | Get the human-readable name of a codec.
codecName :: CompressionCodec -> Text
codecName NoCompression = "none"
codecName Gzip = "gzip"
codecName Snappy = "snappy"
codecName Lz4 = "lz4"
codecName Zstd = "zstd"

-- | Compression level for controlling speed vs ratio tradeoff.
-- Different codecs support different level ranges:
--
-- * Gzip: 0-9 (0=no compression, 1=fastest, 6=default, 9=best compression)
-- * Zstd: 1-22 (1=fastest, 3=default, 22=best compression)
-- * LZ4: 0-16 (0=default fast, higher=better compression but slower)
-- * Snappy: 0-9 (placeholder only - Haskell bindings don't support levels)
-- * NoCompression: level ignored
type CompressionLevel = Int

-- | Get the default compression level for a codec.
-- These defaults balance compression ratio and speed.
defaultLevel :: CompressionCodec -> CompressionLevel
defaultLevel NoCompression = 0
defaultLevel Gzip = 6   -- Gzip default (balanced)
defaultLevel Zstd = 3   -- Zstd default (fast but good compression)
defaultLevel Lz4 = 0    -- LZ4 default (fast mode)
defaultLevel Snappy = 0 -- Snappy (level not used, but placeholder)

-- | Validate compression level for a codec.
-- Returns 'Nothing' if level is valid, or 'Just' error message if invalid.
validateLevel :: CompressionCodec -> CompressionLevel -> Maybe String
validateLevel NoCompression _ = Nothing  -- Any level OK for no compression
validateLevel Gzip level
  | level < 0 || level > 9 = Just "Gzip compression level must be 0-9"
  | otherwise = Nothing
validateLevel Zstd level
  | level < 1 || level > 22 = Just "Zstd compression level must be 1-22"
  | otherwise = Nothing
validateLevel Lz4 level
  | level < 0 || level > 16 = Just "LZ4 compression level must be 0-16"
  | otherwise = Nothing
validateLevel Snappy level
  | level < 0 || level > 9 = Just "Snappy compression level must be 0-9"
  | otherwise = Nothing  -- Note: level is validated but ignored in actual compression

-- | Smart constructor for Gzip compression level (0-9).
gzipLevel :: Int -> Either String CompressionLevel
gzipLevel level = case validateLevel Gzip level of
  Just err -> Left err
  Nothing -> Right level

-- | Smart constructor for Zstd compression level (1-22).
zstdLevel :: Int -> Either String CompressionLevel
zstdLevel level = case validateLevel Zstd level of
  Just err -> Left err
  Nothing -> Right level

-- | Smart constructor for LZ4 compression level (0-16).
lz4Level :: Int -> Either String CompressionLevel
lz4Level level = case validateLevel Lz4 level of
  Just err -> Left err
  Nothing -> Right level

-- | Smart constructor for Snappy compression level (0-9).
-- Note: Snappy level is a placeholder for API consistency.
-- The Haskell snappy bindings don't support configurable compression levels,
-- so the level parameter is validated but not used in actual compression.
snappyLevel :: Int -> Either String CompressionLevel
snappyLevel level = case validateLevel Snappy level of
  Just err -> Left err
  Nothing -> Right level

-- | Compress data using the specified codec with default compression level.
-- Returns 'Left' with an error message if compression fails.
--
-- This is a convenience function that uses the default compression level
-- for each codec. For more control, use 'compressWithLevel'.
--
-- Example:
--
-- > result <- compress Gzip myData
-- > case result of
-- >   Left err -> putStrLn $ "Compression failed: " ++ err
-- >   Right compressed -> sendToKafka compressed
compress :: CompressionCodec -> ByteString -> IO (Either String ByteString)
compress codec bs = compressWithLevel codec (defaultLevel codec) bs

-- | Compress data using the specified codec and compression level.
-- Returns 'Left' with an error message if compression fails or level is invalid.
--
-- Example:
--
-- > result <- compressWithLevel Zstd 10 myData
-- > case result of
-- >   Left err -> putStrLn $ "Compression failed: " ++ err
-- >   Right compressed -> sendToKafka compressed
compressWithLevel :: CompressionCodec -> CompressionLevel -> ByteString -> IO (Either String ByteString)
compressWithLevel codec level bs =
  case validateLevel codec level of
    Just err -> return $ Left err
    Nothing -> case codec of
      NoCompression -> return $ Right bs
      Gzip -> return $ Gzip.compressGzipWithLevel level bs
      Zstd -> Zstd.compressZstdWithLevel level bs
      Lz4 -> Lz4.compressLz4WithLevel level bs
      Snappy -> Snappy.compressSnappyWithLevel level bs  -- Level ignored, placeholder

-- | Decompress data using the specified codec.
-- Returns 'Left' with an error message if decompression fails.
--
-- Example:
--
-- > result <- decompress Gzip compressedData
-- > case result of
-- >   Left err -> putStrLn $ "Decompression failed: " ++ err
-- >   Right decompressed -> processRecords decompressed
decompress :: CompressionCodec -> ByteString -> IO (Either String ByteString)
decompress NoCompression bs = return $ Right bs
decompress Gzip bs = return $ Gzip.decompressGzip bs
decompress Zstd bs = Zstd.decompressZstd bs
decompress Lz4 bs = Lz4.decompressLz4 bs
decompress Snappy bs = Snappy.decompressSnappy bs

-- | Parse a compression codec from its name.
-- Names are case-insensitive.
--
-- Example:
--
-- > parseCompressionCodec "gzip"  == Just Gzip
-- > parseCompressionCodec "ZSTD"  == Just Zstd
-- > parseCompressionCodec "none"  == Just NoCompression
parseCompressionCodec :: Text -> Maybe CompressionCodec
parseCompressionCodec t = case T.toLower t of
  "none" -> Just NoCompression
  "gzip" -> Just Gzip
  "snappy" -> Just Snappy
  "lz4" -> Just Lz4
  "zstd" -> Just Zstd
  _ -> Nothing

-- | Default compression codec (no compression).
-- Applications should choose based on their requirements.
defaultCodec :: CompressionCodec
defaultCodec = NoCompression

