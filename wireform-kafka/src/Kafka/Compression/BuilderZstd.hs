{-# LANGUAGE BangPatterns #-}

{- | Streaming zstd compression directly from a 'Builder' via
'Wireform.Builder.StreamSink'.

Uses direct FFI calls to @ZSTD_compressStream2@ so the builder's
raw buffer pointer is handed straight to zstd — zero intermediate
'ByteString' for the uncompressed input.
-}
module Kafka.Compression.BuilderZstd (
  compressBuilder,
  compressBuilderWithLevel,
  zstdStreamSink,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.IORef
import Data.Word (Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (peekByteOff, pokeByteOff)
import Wireform.Builder qualified as WB


defaultBufSize :: Int
defaultBufSize = 32768


-- | Compress a 'WB.Builder' using zstd at the default level (3).
compressBuilder :: WB.Builder -> IO (Either String ByteString)
compressBuilder = compressBuilderWithLevel 3


-- | Compress a 'WB.Builder' using zstd at the given level.
compressBuilderWithLevel :: Int -> WB.Builder -> IO (Either String ByteString)
compressBuilderWithLevel !level builder = do
  eSink <- zstdStreamSink level
  case eSink of
    Left err -> pure (Left err)
    Right sink -> do
      outBuilder <- WB.runBuilderStreaming sink defaultBufSize builder
      pure (Right (WB.toStrictByteString outBuilder))


{- | Create a 'WB.StreamSink' for zstd streaming compression.

Input: raw pointers from the builder buffer (zero ByteString alloc).
Output: 'ssFinish' returns a 'Builder' of the compressed bytes.
-}
zstdStreamSink :: Int -> IO (Either String WB.StreamSink)
zstdStreamSink !level = do
  cctx <- c_ZSTD_createCCtx
  if cctx == nullPtr
    then pure $ Left "zstd: failed to create CCtx"
    else do
      _ <- c_ZSTD_CCtx_setParameter cctx 100 (fromIntegral level)
      outBufSize <- fromIntegral <$> c_ZSTD_CStreamOutSize
      outFptr <- mallocForeignPtrBytes outBufSize
      chunksRef <- newIORef ([] :: [ByteString])

      let
        -- Feed uncompressed bytes to zstd. Loops until all input consumed.
        feedRaw :: Ptr Word8 -> Int -> IO ()
        feedRaw !srcPtr !srcLen =
          withForeignPtr outFptr $ \outPtr ->
            allocaBytes 24 $ \inBuf -> allocaBytes 24 $ \outBuf -> do
              -- ZSTD_inBuffer { src, size, pos }
              pokeByteOff inBuf (0 :: Int) srcPtr
              pokeByteOff inBuf 8 (fromIntegral srcLen :: CSize)
              pokeByteOff inBuf 16 (0 :: CSize)
              feedLoop inBuf outBuf outPtr 0 {- ZSTD_e_continue -}

        -- Loop: call compressStream2 until all input is consumed.
        -- Each iteration may produce output which we collect.
        feedLoop :: Ptr () -> Ptr () -> Ptr Word8 -> CInt -> IO ()
        feedLoop !inBuf !outBuf !outPtr !directive = do
          -- Reset output buffer for this iteration
          pokeByteOff outBuf (0 :: Int) outPtr
          pokeByteOff outBuf 8 (fromIntegral outBufSize :: CSize)
          pokeByteOff outBuf 16 (0 :: CSize)

          _ <- c_ZSTD_compressStream2 cctx outBuf inBuf directive

          -- Collect any output produced
          outPos <- peekByteOff outBuf 16 :: IO CSize
          when (outPos > 0) $ do
            chunk <- BSI.create (fromIntegral outPos) $ \dst ->
              BSI.memcpy dst outPtr (fromIntegral outPos)
            modifyIORef' chunksRef (chunk :)

          -- Check if all input was consumed
          inPos <- peekByteOff inBuf 16 :: IO CSize
          inSize <- peekByteOff inBuf 8 :: IO CSize
          when (inPos < inSize) $
            feedLoop inBuf outBuf outPtr directive

        -- Finish: call with ZSTD_e_end until remaining == 0.
        finish :: IO WB.Builder
        finish = withForeignPtr outFptr $ \outPtr ->
          allocaBytes 24 $ \inBuf -> allocaBytes 24 $ \outBuf -> do
            -- Empty input
            pokeByteOff inBuf (0 :: Int) nullPtr
            pokeByteOff inBuf 8 (0 :: CSize)
            pokeByteOff inBuf 16 (0 :: CSize)
            flushLoop inBuf outBuf outPtr
            _ <- c_ZSTD_freeCCtx cctx
            chunks <- readIORef chunksRef
            let !compressed = BS.concat (reverse chunks)
            pure (WB.byteStringCopy compressed)

        flushLoop :: Ptr () -> Ptr () -> Ptr Word8 -> IO ()
        flushLoop !inBuf !outBuf !outPtr = do
          pokeByteOff outBuf (0 :: Int) outPtr
          pokeByteOff outBuf 8 (fromIntegral outBufSize :: CSize)
          pokeByteOff outBuf 16 (0 :: CSize)

          remaining <- c_ZSTD_compressStream2 cctx outBuf inBuf 2 {- ZSTD_e_end -}
          outPos <- peekByteOff outBuf 16 :: IO CSize
          when (outPos > 0) $ do
            chunk <- BSI.create (fromIntegral outPos) $ \dst ->
              BSI.memcpy dst outPtr (fromIntegral outPos)
            modifyIORef' chunksRef (chunk :)

          when (remaining > 0) $
            flushLoop inBuf outBuf outPtr

      pure $
        Right
          WB.StreamSink
            { WB.ssFeedRaw = feedRaw
            , WB.ssFinish = finish
            }


when :: Bool -> IO () -> IO ()
when True m = m
when False _ = pure ()
{-# INLINE when #-}


foreign import ccall unsafe "zstd.h ZSTD_createCCtx"
  c_ZSTD_createCCtx :: IO (Ptr ())


foreign import ccall unsafe "zstd.h ZSTD_freeCCtx"
  c_ZSTD_freeCCtx :: Ptr () -> IO CSize


foreign import ccall unsafe "zstd.h ZSTD_CCtx_setParameter"
  c_ZSTD_CCtx_setParameter :: Ptr () -> CInt -> CInt -> IO CSize


foreign import ccall unsafe "zstd.h ZSTD_compressStream2"
  c_ZSTD_compressStream2 :: Ptr () -> Ptr () -> Ptr () -> CInt -> IO CSize


foreign import ccall unsafe "zstd.h ZSTD_CStreamOutSize"
  c_ZSTD_CStreamOutSize :: IO CSize
