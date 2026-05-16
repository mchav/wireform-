{-# LANGUAGE BangPatterns #-}

{- | Snappy compression from a 'Builder' via 'Wireform.Builder.StreamSink'.

Snappy requires contiguous input, so chunks are accumulated and
compressed in 'ssFinish'.
-}
module Kafka.Compression.BuilderSnappy (
  compressBuilder,
  snappyStreamSink,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import Kafka.Compression.Snappy qualified as Snappy
import Wireform.Builder qualified as WB


defaultBufSize :: Int
defaultBufSize = 32768


-- | Compress a 'WB.Builder' using Snappy.
compressBuilder :: WB.Builder -> IO (Either String ByteString)
compressBuilder builder = do
  sink <- snappyStreamSink
  outBuilder <- WB.runBuilderStreaming sink defaultBufSize builder
  let compressed = WB.toStrictByteString outBuilder
  if BS.null compressed
    then pure (Right BS.empty)
    else pure (Right compressed)


{- | Create a 'WB.StreamSink' for Snappy compression.
Accumulates chunks, compresses in 'ssFinish'.
-}
snappyStreamSink :: IO WB.StreamSink
snappyStreamSink = do
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
              result <- Snappy.compressSnappy input
              case result of
                Right compressed -> pure (WB.byteStringCopy compressed)
                Left err -> error $ "snappy compression failed: " ++ err
      }
