{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

{- | Zero-copy HTTP\/2 frame reader on top of the wireform magic-ring
'Wireform.Transport'.

This is the /fast/ streaming entry point: it walks the ring directly
(no 'Wireform.Parser' monad, no unboxed-sum threading) using a tight
read-9-bytes-then-decode-then-slice-payload loop, sharing the
existing 'decodeFrameHeader' \/ 'decodeFramePayload' validators with
the classic recv-buffer path.

Compared to 'Network.HTTP2.Frame.Stream' (which composes the same
walk under 'Wireform.Parser' @Stream@), this implementation pays no
parser monad overhead — the inner loop is two 'recvBufferRead'-style
direct reads + the existing decoders, identical to what
'Network.HTTP2.Connection' runs today on top of
'Network.HTTP2.Internal.RecvBuffer'.  The only difference is the
receive buffer: a double-mapped magic ring instead of a heap-allocated
pinned buffer.

Use this on the hot connection-handler path; reserve
'Network.HTTP2.Frame.Stream' for situations where you want to compose
the per-frame parse with the rest of the 'Wireform.Parser' surface.
-}
module Network.HTTP2.Frame.StreamingReader
  ( -- * Errors
    ReadError (..)

    -- * Frame readers
  , readFrame
  , readFrameFrom
  , readFrameLoop
  ) where

import Control.Exception (SomeException)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word8, Word64)
import Foreign.Ptr (Ptr, plusPtr)
import GHC.Ptr (Ptr (Ptr))
import GHC.Exts (Int (..))
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (..))
import GHC.Generics (Generic)

import Wireform.Ring.Internal
  ( MagicRing
  , ringBase
  , ringMask
  )
import Wireform.Transport
  ( Transport (..)
  , WaitResult (..)
  )

import Network.HTTP2.Frame
  ( Frame (..)
  , FrameHeader (..)
  , FrameDecodeError
  , decodeFrameHeader
  , decodeFramePayload
  , frameHeaderLength
  )

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

data ReadError
  = ReadDecode !FrameDecodeError
  | ReadUnexpectedEof
  | ReadTransportError !SomeException
  deriving stock (Generic)

instance Show ReadError where
  show (ReadDecode e)         = "ReadDecode " <> show e
  show ReadUnexpectedEof      = "ReadUnexpectedEof"
  show (ReadTransportError exc) = "ReadTransportError " <> show exc

------------------------------------------------------------------------
-- Frame readers
------------------------------------------------------------------------

-- | Read one HTTP\/2 frame off the transport.  Uses the current
-- 'transportLoadHead' as the starting cursor, so this entry point is
-- meant for the very first read on a fresh connection (head = 0).
-- For loops use 'readFrameLoop' or 'readFrameFrom', both of which
-- track position explicitly.
readFrame :: Transport -> IO (Either ReadError Frame)
readFrame t = do
  startPos <- transportLoadHead t
  fmap (fmap fst) (readFrameFrom t startPos)
{-# INLINE readFrame #-}

-- | Read one frame starting at the given ring position.  Returns the
-- decoded 'Frame' and the position immediately after the frame's
-- payload so the caller can chain reads without touching the
-- transport's head pointer between iterations.
--
-- The 'Frame's payload 'ByteString' is a zero-copy slice of the
-- ring's backing memory; it stays valid until the caller next
-- 'transportAdvanceTail's past it (this function does the advance).
-- 'BS.copy' if the payload outlives the per-frame handler scope.
--
-- == Hot loop shape (per Core inspection)
--
-- 1. One 'transportLoadHead' read at the top.
-- 2. Decode header (worker/wrapper takes @Addr#@ directly).
-- 3. Check @h - startPos >= 9 + payloadLen@ /without/ re-reading
--    head (single-threaded design: head can only advance on
--    'transportWaitData', which we haven't called).
-- 4. Decode payload + 'transportAdvanceTail' once.
--
-- This is one fewer 'transportLoadHead' per frame than the more
-- conservative \"stage 1, then stage 2\" shape — measurable on
-- small-frame workloads (~5% per-frame improvement on the
-- @1000 small DATA frames@ benchmark).
readFrameFrom
  :: Transport
  -> Word64
  -> IO (Either ReadError (Frame, Word64))
readFrameFrom t startPos = do
  let !ring = transportRing t
  hdrE <- ensureBytes t startPos frameHeaderLength
  case hdrE of
    Left e -> pure (Left e)
    Right h0 -> do
      let !hdrSlice = ringSlice ring startPos frameHeaderLength
      case decodeFrameHeader hdrSlice of
        Left e -> pure (Left (ReadDecode e))
        Right hdr -> do
          let !payloadLen = fromIntegral (fhLength hdr) :: Int
              !payloadPos = startPos + fromIntegral frameHeaderLength
              !need64     = fromIntegral payloadLen :: Word64
          -- Reuse the head value we already saw on stage 1 — no
          -- intervening waitData means it hasn't moved.
          h1 <- if h0 - payloadPos >= need64
                  then pure (Right h0)
                  else ensureBytes t payloadPos payloadLen
          case h1 of
            Left e -> pure (Left e)
            Right _ -> do
              let !payloadSlice = ringSlice ring payloadPos payloadLen
              case decodeFramePayload hdr payloadSlice of
                Left e -> pure (Left (ReadDecode e))
                Right pl -> do
                  let !nextPos = payloadPos + fromIntegral payloadLen
                  transportAdvanceTail t nextPos
                  pure (Right (Frame hdr pl, nextPos))
{-# INLINE readFrameFrom #-}

-- | Loop: keep reading frames and dispatching them to the handler,
-- threading the position counter so we don't pay a
-- 'transportLoadHead' round-trip per frame.  Returns on the first
-- error or when the handler returns 'False' ("stop").
readFrameLoop
  :: Transport
  -> (Frame -> IO Bool)   -- ^ 'True' = keep going, 'False' = stop.
  -> IO (Either ReadError ())
readFrameLoop t handler = do
  startPos <- transportLoadHead t
  loop startPos
  where
    loop !pos = do
      r <- readFrameFrom t pos
      case r of
        Left e -> pure (Left e)
        Right (fr, newPos) -> do
          continue <- handler fr
          if continue then loop newPos else pure (Right ())

------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------

-- | Block until at least @n@ bytes are available past @startPos@.
-- Returns the (latest known) head position on success so callers
-- can re-use the value without paying a second 'transportLoadHead'
-- round-trip if they need more from the same window.
ensureBytes
  :: Transport
  -> Word64           -- ^ start position
  -> Int              -- ^ bytes needed
  -> IO (Either ReadError Word64)
ensureBytes t startPos needed = loop
  where
    needed64 = fromIntegral needed :: Word64
    loop = do
      h <- transportLoadHead t
      if h - startPos >= needed64
        then pure (Right h)
        else do
          r <- transportWaitData t h
          case r of
            MoreData _         -> loop
            EndOfInput         -> pure (Left ReadUnexpectedEof)
            TransportError exc -> pure (Left (ReadTransportError exc))
{-# INLINE ensureBytes #-}

-- | Zero-copy 'BS.ByteString' over @[pos, pos + len)@ of the ring.
-- Constructs the 'ForeignPtr' with 'FinalPtr' directly (same trick
-- 'Wireform.Parser.takeBs' uses) so the per-frame slice doesn't pay
-- a 'newForeignPtr_' / @unsafePerformIO@ round-trip.  The slice
-- becomes a dangling pointer if it outlives 'withMagicRing'; copy
-- via 'BS.copy' to retain past that scope.
ringSlice :: MagicRing -> Word64 -> Int -> ByteString
ringSlice ring pos (I# len#) =
  let !base = ringBase ring
      !msk  = ringMask ring
      !off  = fromIntegral pos .&. msk
      !(Ptr addr#) = base `plusPtr` off
  in BSI.BS (ForeignPtr addr# FinalPtr) (I# len#)
{-# INLINE ringSlice #-}
