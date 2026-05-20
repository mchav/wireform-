{- | Pinned ring buffer for zero-allocation receives.

This is a near-mirror of @Network.HTTP2.Internal.RecvBuffer@; the only
HTTP\/1-specific extension is 'recvBufferReadUntilDoubleCRLF', which
scans ahead with SIMD for the next blank line and pulls from the socket
until one is found (or the buffer fills).

When we eventually merge with @wireform-http2@ this module will move to
@wireform-core@ so both protocols share a single implementation.
-}
module Network.HTTP1.Internal.RecvBuffer
  ( RecvBuffer (..)
  , newRecvBuffer
  , newRecvBufferSized
    -- * Reads
  , recvBufferRead
  , recvBufferReadAtMost
  , recvBufferReadUntilCRLF
  , recvBufferReadUntilCRLFStrict
  , recvBufferReadUntilDoubleCRLF
    -- * State
  , recvBufferAvailable
  , recvBufferCompact
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Data.Word (Word8)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (peekByteOff)
import Network.Socket (Socket, recvBuf)

foreign import ccall unsafe "hs_http1_find_cr"
  c_find_cr :: Ptr () -> CInt -> CInt -> CInt

data RecvBuffer = RecvBuffer
  { rbBuffer   :: !(ForeignPtr Word8)
  , rbCapacity :: !Int
  , rbReadPos  :: !(IORef Int)
  , rbWritePos :: !(IORef Int)
  }

-- | h2o uses 16 KiB recv buffers; we match that.
defaultBufferCapacity :: Int
defaultBufferCapacity = 16384

newRecvBuffer :: IO RecvBuffer
newRecvBuffer = newRecvBufferSized defaultBufferCapacity

newRecvBufferSized :: Int -> IO RecvBuffer
newRecvBufferSized cap = do
  fp <- BSI.mallocByteString cap
  rp <- newIORef 0
  wp <- newIORef 0
  pure RecvBuffer { rbBuffer = fp, rbCapacity = cap, rbReadPos = rp, rbWritePos = wp }

recvBufferAvailable :: RecvBuffer -> IO Int
recvBufferAvailable rb = do
  rp <- readIORef (rbReadPos rb)
  wp <- readIORef (rbWritePos rb)
  pure (wp - rp)

-- | Compact unread data to the front. Idempotent.
recvBufferCompact :: RecvBuffer -> IO ()
recvBufferCompact rb = do
  rp <- readIORef (rbReadPos rb)
  wp <- readIORef (rbWritePos rb)
  let unread = wp - rp
  if rp > 0
    then do
      if unread > 0
        then withForeignPtr (rbBuffer rb) $ \base ->
          copyBytes base (base `plusPtr` rp) unread
        else pure ()
      writeIORef (rbReadPos rb) 0
      writeIORef (rbWritePos rb) unread
    else pure ()

-- | Read exactly @n@ bytes (blocking). Zero-copy when no wrap-around;
-- returns 'BS.empty' if the peer closes before @n@ bytes arrive.
recvBufferRead :: RecvBuffer -> Socket -> Int -> IO ByteString
recvBufferRead rb sock n = do
  rp <- readIORef (rbReadPos rb)
  wp <- readIORef (rbWritePos rb)
  let avail = wp - rp
  if avail >= n
    then sliceBuffer rb rp n
    else do
      recvBufferCompact rb
      fillUntil rb sock n

{-# INLINE sliceBuffer #-}
sliceBuffer :: RecvBuffer -> Int -> Int -> IO ByteString
sliceBuffer rb rp n = do
  writeIORef (rbReadPos rb) (rp + n)
  pure $! BSI.fromForeignPtr (rbBuffer rb) rp n

-- | Read at most @n@ bytes; returns the largest available slice up to
-- @n@. Used for body streaming where we want to forward whatever is in
-- the buffer rather than waiting for a specific amount.
recvBufferReadAtMost :: RecvBuffer -> Socket -> Int -> IO ByteString
recvBufferReadAtMost rb sock n = do
  rp <- readIORef (rbReadPos rb)
  wp <- readIORef (rbWritePos rb)
  let avail = wp - rp
  if avail > 0
    then do
      let take_ = min avail n
      writeIORef (rbReadPos rb) (rp + take_)
      pure $! BSI.fromForeignPtr (rbBuffer rb) rp take_
    else do
      recvBufferCompact rb
      got <- fillOnce rb sock n
      if got <= 0
        then pure BS.empty
        else do
          rp' <- readIORef (rbReadPos rb)
          let take_ = min got n
          writeIORef (rbReadPos rb) (rp' + take_)
          pure $! BSI.fromForeignPtr (rbBuffer rb) rp' take_

fillUntil :: RecvBuffer -> Socket -> Int -> IO ByteString
fillUntil rb sock n = do
  rp <- readIORef (rbReadPos rb)
  wp <- readIORef (rbWritePos rb)
  let avail = wp - rp
  if avail >= n
    then sliceBuffer rb rp n
    else do
      let space = rbCapacity rb - wp
      if space <= 0
        then do
          recvBufferCompact rb
          rp' <- readIORef (rbReadPos rb)
          wp' <- readIORef (rbWritePos rb)
          let space' = rbCapacity rb - wp'
          if space' <= 0
            then if wp' - rp' > 0
                   then sliceBuffer rb rp' (wp' - rp')
                   else pure BS.empty
            else fillUntil rb sock n
        else do
          let toRecv = max (n - avail) (min space 65536)
          got <- withForeignPtr (rbBuffer rb) $ \base ->
            recvBuf sock (base `plusPtr` wp) toRecv
          if got <= 0
            then if avail > 0
                   then sliceBuffer rb rp avail
                   else pure BS.empty
            else do
              writeIORef (rbWritePos rb) (wp + got)
              fillUntil rb sock n

-- | One recv into the tail of the buffer; returns bytes received
-- (0 on EOF). Never blocks beyond a single @recv()@.
fillOnce :: RecvBuffer -> Socket -> Int -> IO Int
fillOnce rb sock want = do
  wp <- readIORef (rbWritePos rb)
  let space = rbCapacity rb - wp
      toRecv = min space (max want 1)
  if toRecv <= 0
    then pure 0
    else do
      got <- withForeignPtr (rbBuffer rb) $ \base ->
        recvBuf sock (base `plusPtr` wp) toRecv
      if got <= 0
        then pure 0
        else do
          writeIORef (rbWritePos rb) (wp + got)
          pure got

------------------------------------------------------------------------
-- CRLF: single-line read (chunk-size lines, trailer-section lines, …)
------------------------------------------------------------------------

-- | Read up to (and including) the next CRLF in the recv buffer; return
-- the slice /without/ the trailing CRLF and advance the read cursor
-- past the CRLF. Returns 'Nothing' on EOF or oversize.
--
-- This is essential for chunked TE: we MUST NOT consume any bytes
-- beyond the CRLF here, because the chunk body sits immediately after
-- on the wire and is read by a separate exact-length pull.
recvBufferReadUntilCRLF :: RecvBuffer -> Socket -> Int -> IO (Maybe ByteString)
recvBufferReadUntilCRLF rb sock cap = go
  where
    go = do
      rp <- readIORef (rbReadPos rb)
      wp <- readIORef (rbWritePos rb)
      let avail = wp - rp
      if avail > cap
        then pure Nothing
        else do
          mIdx <- findCRLF rb rp wp
          case mIdx of
            Just idx -> do
              let lineLen = idx - rp
              writeIORef (rbReadPos rb) (idx + 2)
              pure $! Just (BSI.fromForeignPtr (rbBuffer rb) rp lineLen)
            Nothing -> do
              let needCompact = wp >= rbCapacity rb - 1024 && rp > 0
              if needCompact
                then recvBufferCompact rb
                else pure ()
              got <- fillOnce rb sock 4096
              if got <= 0
                then pure Nothing
                else go

findCRLF :: RecvBuffer -> Int -> Int -> IO (Maybe Int)
findCRLF rb start end = withForeignPtr (rbBuffer rb) $ \base -> go base start
  where
    go base i
      | i + 2 > end = pure Nothing
      | otherwise = do
          let crPos = fromIntegral
                (c_find_cr (castPtr base) (fromIntegral i) (fromIntegral end))
          if crPos + 2 > end
            then pure Nothing
            else do
              b1 <- peekByteOff base (crPos + 1) :: IO Word8
              if b1 == 0x0a
                then pure (Just crPos)
                else go base (crPos + 1)

------------------------------------------------------------------------
-- Strict CRLF read (rejects bare LF)
------------------------------------------------------------------------

-- | Like 'recvBufferReadUntilCRLF' but if a bare LF appears before the
-- next CRLF, returns 'Just Nothing' (signalling a protocol error in
-- the trailer / chunked-body section, where RFC 9112 § 2.2 permits
-- but does not require lenient acceptance).
--
-- Returns:
--
--   * @Just (Right slice)@ — found a proper CRLF; @slice@ is the line
--     without the trailing CRLF.
--   * @Just (Left ())@ — encountered a bare LF before any CRLF.
--   * @Nothing@ — EOF before finding either, or oversized line.
recvBufferReadUntilCRLFStrict
  :: RecvBuffer -> Socket -> Int -> IO (Maybe (Either () ByteString))
recvBufferReadUntilCRLFStrict rb sock cap = go
  where
    go = do
      rp <- readIORef (rbReadPos rb)
      wp <- readIORef (rbWritePos rb)
      let avail = wp - rp
      if avail > cap
        then pure Nothing
        else do
          outcome <- scanCrlfOrBareLf rb rp wp
          case outcome of
            ScanBareLf -> pure (Just (Left ()))
            ScanFoundCrlf idx -> do
              let lineLen = idx - rp
              writeIORef (rbReadPos rb) (idx + 2)
              pure (Just (Right (BSI.fromForeignPtr (rbBuffer rb) rp lineLen)))
            ScanNeedMore -> do
              if wp >= rbCapacity rb - 1024 && rp > 0
                then recvBufferCompact rb
                else pure ()
              got <- fillOnce rb sock 4096
              if got <= 0
                then pure Nothing
                else go

data ScanResult
  = ScanFoundCrlf !Int
  | ScanBareLf
  | ScanNeedMore

-- | Scan @[start, end)@ for either a CRLF (returns its offset) or a
-- bare LF (returns 'ScanBareLf'). Uses the SIMD CR scanner to skip
-- whole 16-byte runs but also walks single bytes when a CR is found
-- to inspect the following byte.
scanCrlfOrBareLf :: RecvBuffer -> Int -> Int -> IO ScanResult
scanCrlfOrBareLf rb start end =
  withForeignPtr (rbBuffer rb) $ \base -> go base start
  where
    go base i
      | i >= end = pure ScanNeedMore
      | otherwise = do
          -- Look for the next CR or LF, whichever comes first.
          let crPos = fromIntegral
                (c_find_cr (castPtr base) (fromIntegral i) (fromIntegral end))
              lfPos = fromIntegral
                (c_find_lf (castPtr base) (fromIntegral i) (fromIntegral end))
          if lfPos < crPos
            then pure ScanBareLf
            else if crPos >= end
              then pure ScanNeedMore
              else
                -- crPos <= lfPos, both within range (or crPos < end).
                if crPos + 1 >= end
                  then pure ScanNeedMore
                  else do
                    b1 <- peekByteOff base (crPos + 1) :: IO Word8
                    if b1 == 0x0a
                      then pure (ScanFoundCrlf crPos)
                      else go base (crPos + 1)

foreign import ccall unsafe "hs_http1_find_lf"
  c_find_lf :: Ptr () -> CInt -> CInt -> CInt

------------------------------------------------------------------------
-- CRLFCRLF: full header block in one slice
------------------------------------------------------------------------

-- | Read until (and including) the next blank line @\\r\\n\\r\\n@.
-- Returns the slice (without the trailing terminator), advances the
-- read cursor past the terminator. Returns 'Nothing' if the block grew
-- past @cap@ bytes or the peer closed before terminating it.
--
-- The zero-cost SIMD CR scanner does the heavy lifting; the LF\/CR
-- follow-up is three byte loads per CR-hit, which is amortised across
-- 16-byte scans.
recvBufferReadUntilDoubleCRLF :: RecvBuffer -> Socket -> Int -> IO (Maybe ByteString)
recvBufferReadUntilDoubleCRLF rb sock cap = go
  where
    go = do
      rp <- readIORef (rbReadPos rb)
      wp <- readIORef (rbWritePos rb)
      let avail = wp - rp
      if avail > cap
        then pure Nothing
        else do
          mIdx <- findCRLFCRLF rb rp wp
          case mIdx of
            Just idx -> do
              let blockLen = idx - rp
              writeIORef (rbReadPos rb) (idx + 4)
              pure $! Just (BSI.fromForeignPtr (rbBuffer rb) rp blockLen)
            Nothing -> do
              -- Compact opportunistically if we're crowding the tail.
              let needCompact = wp >= rbCapacity rb - 1024 && rp > 0
              if needCompact
                then recvBufferCompact rb
                else pure ()
              got <- fillOnce rb sock 4096
              if got <= 0
                then pure Nothing
                else go

-- | Linear scan for @\\r\\n\\r\\n@ in @[start, end)@ using the SIMD
-- CR finder for whole-stride skips between candidate CRs.
findCRLFCRLF :: RecvBuffer -> Int -> Int -> IO (Maybe Int)
findCRLFCRLF rb start end = withForeignPtr (rbBuffer rb) $ \base -> go base start
  where
    go base i
      | i + 4 > end = pure Nothing
      | otherwise = do
          let crPos = fromIntegral
                (c_find_cr (castPtr base) (fromIntegral i) (fromIntegral end))
          if crPos + 4 > end
            then pure Nothing
            else do
              b1 <- peekByteOff base (crPos + 1) :: IO Word8
              b2 <- peekByteOff base (crPos + 2) :: IO Word8
              b3 <- peekByteOff base (crPos + 3) :: IO Word8
              if b1 == 0x0a && b2 == 0x0d && b3 == 0x0a
                then pure (Just crPos)
                else go base (crPos + 1)
