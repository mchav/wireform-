{- |
Module      : Kafka.Compression.Zstd
Description : Zstandard compression implementation for Kafka
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

Zstandard (zstd) compression implementation.
Zstd provides excellent compression ratios with good speed,
making it the recommended codec for most Kafka deployments.

Kafka uses standard zstd frame format.
-}
module Kafka.Compression.Zstd (
  compressZstd,
  compressZstdWithLevel,
  decompressZstd,
  defaultZstdLevel,
) where

import Codec.Compression.Zstd (Decompress (..))
import Codec.Compression.Zstd qualified as Zstd
import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS


{- | Default zstd compression level (3 = fast compression).
Levels range from 1 (fastest) to 22 (maximum compression).
Level 3 provides a good balance of speed and compression.
-}
defaultZstdLevel :: Int
defaultZstdLevel = 3


-- | Compress data using zstd with default compression level.
compressZstd :: ByteString -> IO (Either String ByteString)
compressZstd = compressZstdWithLevel defaultZstdLevel


{- | Compress data using zstd with specified compression level.
Level must be 1-22 (1=fastest, 3=default, 22=best compression).
-}
compressZstdWithLevel :: Int -> ByteString -> IO (Either String ByteString)
compressZstdWithLevel level bs
  | BS.null bs = return $ Right BS.empty -- Handle empty input
  | otherwise = do
      result <- try $ return $ Zstd.compress level bs
      return $ case result of
        Left e -> Left $ "Zstd compression failed: " ++ show (e :: SomeException)
        Right compressed -> Right compressed


-- | Decompress zstd-compressed data.
decompressZstd :: ByteString -> IO (Either String ByteString)
decompressZstd bs
  | BS.null bs = return $ Right BS.empty -- Handle empty input
  | otherwise = do
      result <- try $ pure $ Zstd.decompress bs
      return $ case result of
        Left e -> Left $ "Zstd decompression failed: " ++ show (e :: SomeException)
        Right (Decompress decompressed) -> Right decompressed
        Right (Error err) -> Left $ "Zstd decompression error: " ++ err
        Right Skip -> Left "Zstd decompression returned Skip"
