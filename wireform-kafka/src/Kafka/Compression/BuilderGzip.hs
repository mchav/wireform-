{-# LANGUAGE BangPatterns #-}

{- | Streaming gzip compression directly from a 'Builder' via
'Wireform.Builder.StreamSink'.
-}
module Kafka.Compression.BuilderGzip (
  compressBuilder,
  compressBuilderWithLevel,
  gzipStreamSink,
) where

import Codec.Compression.GZip qualified as GZip
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import Wireform.Builder qualified as WB


defaultBufSize :: Int
defaultBufSize = 32768


-- | Compress a 'WB.Builder' using gzip at the default level (6).
compressBuilder :: WB.Builder -> Either String ByteString
compressBuilder = compressBuilderWithLevel 6


-- | Compress a 'WB.Builder' using gzip at the given level (0-9).
compressBuilderWithLevel :: Int -> WB.Builder -> Either String ByteString
compressBuilderWithLevel !level builder =
  Right $ BL.toStrict $ GZip.compressWith params $ WB.toLazyByteString builder
  where
    params =
      GZip.defaultCompressParams
        { GZip.compressLevel = GZip.compressionLevel level
        }


{- | Create a 'WB.StreamSink' for gzip compression.

Accumulates raw pointer regions as ByteStrings (must copy since
the buffer is reused), then compresses in 'ssFinish'. The result
is returned as a Builder.
-}
gzipStreamSink :: Int -> IO WB.StreamSink
gzipStreamSink !level = do
  chunksRef <- newIORef ([] :: [ByteString])
  pure
    WB.StreamSink
      { WB.ssFeedRaw = \ptr len -> do
          -- Must copy since the streaming sink reuses the buffer
          bs <- BS.packCStringLen (castPtr ptr, len)
          modifyIORef' chunksRef (bs :)
      , WB.ssFinish = do
          chunks <- readIORef chunksRef
          let !input = BL.fromChunks (reverse chunks)
              params =
                GZip.defaultCompressParams
                  { GZip.compressLevel = GZip.compressionLevel level
                  }
              !compressed = BL.toStrict $ GZip.compressWith params input
          pure (WB.byteStringCopy compressed)
      }
