{- | The universal wire representation for HTTP message bodies.

A 'BodyStream' is a popper — a pull-based chunk source. The
transport layer always speaks 'BodyStream's internally, so a
strict body is just a popper that yields one chunk then EOF.

Conventions:

* A well-behaved producer never emits a zero-length chunk before
  EOF. The pull action returns an empty 'ByteString' to signal EOF.
* 'knownSize', when present, lets the encoder set @Content-Length@
  and lets middleware make informed buffering decisions.
-}
{-# LANGUAGE BangPatterns #-}
module Network.HTTP.Client.BodyStream
  ( BodyStream (..)
  , Popper
  , emptyStream
  , popperFromStrict
  , popperFromList
  , streamFromStrict
  , streamFromList
    -- * Consuming
  , drainPopper
  , drainBodyStream
  , popperBytes
  , bodyStreamBytes
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.IORef (atomicModifyIORef', newIORef)

import qualified Wireform.Builder as WB

-- | A pull-based byte source. Returns 'BS.empty' to signal EOF.
type Popper = IO ByteString

-- | A pull-shaped byte stream.
data BodyStream = BodyStream
  { pull      :: !Popper
  , knownSize :: !(Maybe Int64)
  }

-- | A stream that is empty from the start.
emptyStream :: BodyStream
emptyStream = BodyStream
  { pull = pure BS.empty
  , knownSize = Just 0
  }

-- | Turn a strict 'ByteString' into a popper that yields it once then
-- returns 'BS.empty' forever.
popperFromStrict :: ByteString -> IO Popper
popperFromStrict bs = do
  ref <- newIORef (Just bs)
  pure $ atomicModifyIORef' ref $ \case
    Just b  -> (Nothing, b)
    Nothing -> (Nothing, BS.empty)

-- | Build a popper that yields a known sequence of non-empty chunks.
popperFromList :: [ByteString] -> IO Popper
popperFromList chunks = do
  ref <- newIORef (filter (not . BS.null) chunks)
  pure $ atomicModifyIORef' ref $ \case
    []     -> ([], BS.empty)
    (c:cs) -> (cs, c)

-- | A 'BodyStream' built from a strict 'ByteString'. 'knownSize' is
-- populated.
streamFromStrict :: ByteString -> IO BodyStream
streamFromStrict bs = do
  p <- popperFromStrict bs
  pure BodyStream
    { pull = p
    , knownSize = Just (fromIntegral (BS.length bs))
    }

-- | A 'BodyStream' built from a list of chunks. 'knownSize' is the
-- total length.
streamFromList :: [ByteString] -> IO BodyStream
streamFromList chunks = do
  let cleaned = filter (not . BS.null) chunks
  p <- popperFromList cleaned
  pure BodyStream
    { pull = p
    , knownSize = Just (fromIntegral (sum (map BS.length cleaned)))
    }

-- | Pull from a popper until EOF and __discard__ the bytes.
--
-- This is the right operation when you've decided you don't want a
-- response body — for example, when a retry middleware skips a
-- failed attempt's body so the connection can advance, or when the
-- caller chose 'DiscardBody'. Each chunk is dropped as soon as the
-- loop reads it; nothing is accumulated in memory.
--
-- If you /do/ want the bytes, use 'popperBytes' (which makes the
-- materialisation explicit at the call site).
drainPopper :: Popper -> IO ()
drainPopper p = go
  where
    go = do
      chunk <- p
      if BS.null chunk then pure () else go

-- | 'drainPopper' for a 'BodyStream'.
drainBodyStream :: BodyStream -> IO ()
drainBodyStream = drainPopper . pull

-- | Pull from a popper until EOF and collect the bytes into a single
-- strict 'ByteString'.
--
-- Chunks are accumulated through 'Wireform.Builder' so each append
-- is O(1); the final 'WB.toStrictByteString' is the only allocating
-- copy. Use this when you genuinely need the full body in memory
-- (decoding JSON, recording a VCR cassette, asserting against a
-- recorded request). For "just drain to release the connection",
-- use 'drainPopper' instead — it doesn't allocate.
popperBytes :: Popper -> IO ByteString
popperBytes p = WB.toStrictByteString <$> go mempty
  where
    go !acc = do
      chunk <- p
      if BS.null chunk
        then pure acc
        else go (acc <> WB.byteString chunk)

-- | 'popperBytes' for a 'BodyStream'.
bodyStreamBytes :: BodyStream -> IO ByteString
bodyStreamBytes = popperBytes . pull
