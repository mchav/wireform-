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
module Network.HTTP.Wire.BodyStream
  ( BodyStream (..)
  , Popper
  , emptyStream
  , popperFromStrict
  , popperFromList
  , streamFromStrict
  , streamFromList
  , drainPopper
  , drainBodyStream
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.IORef (atomicModifyIORef', newIORef)

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

-- | Drain a popper to a strict 'ByteString'. Stops at the first
-- empty-chunk return.
drainPopper :: Popper -> IO ByteString
drainPopper p = go []
  where
    go acc = do
      chunk <- p
      if BS.null chunk
        then pure $! BS.concat (reverse acc)
        else go (chunk : acc)

-- | Drain a 'BodyStream'. Equivalent to @drainPopper . pull@.
drainBodyStream :: BodyStream -> IO ByteString
drainBodyStream = drainPopper . pull
