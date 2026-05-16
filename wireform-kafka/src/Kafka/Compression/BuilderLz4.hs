{-# LANGUAGE BangPatterns #-}

{- | LZ4 compression from a 'Builder' via 'Wireform.Builder.StreamSink'.

LZ4 requires contiguous input, so chunks are accumulated and
compressed in 'ssFinish'. The uncompressed payload still avoids
the main builder's contiguous allocation — chunks are ~bufSize
each and collected in a list.
-}
module Kafka.Compression.BuilderLz4 (
  compressBuilder,
  compressBuilderWithLevel,
  lz4StreamSink,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import Kafka.Compression.Lz4 qualified as Lz4
import Wireform.Builder qualified as WB


defaultBufSize :: Int
defaultBufSize = 32768


-- | Compress a 'WB.Builder' using LZ4 at the default level (0).
compressBuilder :: WB.Builder -> IO (Either String ByteString)
compressBuilder = compressBuilderWithLevel Lz4.defaultLz4Level


-- | Compress a 'WB.Builder' using LZ4 at the given level.
compressBuilderWithLevel :: Int -> WB.Builder -> IO (Either String ByteString)
compressBuilderWithLevel !level builder = do
  sink <- lz4StreamSink level
  outBuilder <- WB.runBuilderStreaming sink defaultBufSize builder
  let compressed = WB.toStrictByteString outBuilder
  if BS.null compressed
    then pure (Right BS.empty)
    else pure (Right compressed)


{- | Create a 'WB.StreamSink' for LZ4 compression.
Accumulates chunks, compresses in 'ssFinish'.
-}
lz4StreamSink :: Int -> IO WB.StreamSink
lz4StreamSink !level = do
  chunksRef <- newIORef ([] :: [ByteString])
  pure
    WB.StreamSink
      { WB.ssFeedRaw = \ptr len -> do
          bs <- BS.packCStringLen (castPtr ptr, len)
          modifyIORef' chunksRef (bs :)
      , WB.ssFinish = do
          chunks <- readIORef chunksRef
          let !input = BS.concat (reverse chunks)
          if BS.null input
            then pure mempty
            else do
              result <- Lz4.compressLz4WithLevel level input
              case result of
                Right compressed -> pure (WB.byteStringCopy compressed)
                Left err -> error $ "lz4 compression failed: " ++ err
      }
