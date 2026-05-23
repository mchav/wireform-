{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

{- | Zero-copy HTTP\/1.x head reader on top of the wireform magic-ring
'Wireform.Transport'.

This module is the /fast/ streaming entry point: it walks the ring
directly with the SIMD CR / CRLFCRLF scanners that live in
@cbits/http1_scan.c@, slices the header block as a single zero-copy
'BS.ByteString' over the ring's foreign pointer, and hands it to the
existing 'Network.HTTP1.Parser.parseRequest' / 'parseResponse' which
walk the slice with the same SIMD tchar / field-vchar primitives the
classic 'Network.HTTP1.Connection' read path uses.

Compared to 'Network.HTTP1.StreamingParser' (which composes
byte-by-byte under the 'Wireform.Parser' @Stream@ monad), this
implementation pays no monad / unboxed-sum overhead — the inner loop
is a single @hs_http1_find_cr@ call per 16-byte stride, identical to
what 'Network.HTTP1.Connection.readBody' / 'Network.HTTP1.Server' do
today on top of 'Network.HTTP1.Internal.RecvBuffer'.  The only
difference is the receive buffer: a double-mapped magic ring instead
of a heap-allocated pinned buffer.  The slice handed to 'parseRequest'
crosses the ring wrap transparently because the magic ring's second
mapping makes any read of up to @ringSize@ bytes contiguous.

Use this in preference to 'Network.HTTP1.StreamingParser' on the hot
connection-handler path; reserve 'StreamingParser' for situations
where you want incremental parsing primitives composable with the
rest of the 'Wireform.Parser' surface.
-}
module Network.HTTP1.StreamingReader
  ( -- * Errors
    ReadError (..)

    -- * Whole-request \/ whole-response readers
  , readRequestHead
  , readResponseHead
  , readRequestHeadFrom
  , readResponseHeadFrom

    -- * Building blocks
  , readHeaderBlock
  , readHeaderBlockFrom
  , readChunkSizeLine
  , readChunkSizeLineFrom
  ) where

import Control.Exception (SomeException)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word8, Word64)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GHC.Ptr (Ptr (Ptr))
import Foreign.Storable (peekByteOff)
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

import Network.HTTP1.Method (Method)
import qualified Network.HTTP1.Parser as Classic
import Network.HTTP1.Parser (Framing, ParseError)
import Network.HTTP1.Types (Request, Response)

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

-- | Why a streaming read failed.
data ReadError
  = ReadParse !ParseError
    -- ^ The header / chunk-size block was fully delivered but the
    --   structural parser rejected it.  Contains the existing
    --   'Network.HTTP1.Parser.ParseError' value so callers can map
    --   onto the corresponding HTTP response code.
  | ReadMessageTooLong !Int
    -- ^ Header / chunk-size block exceeded the supplied cap before
    --   a terminating delimiter was seen.  Maps to 431 (request) /
    --   502 (response upstream) / protocol error (chunked body).
  | ReadUnexpectedEof
    -- ^ Transport reported EOF before the terminator was seen.
  | ReadTransportError !SomeException
  deriving stock (Generic)

instance Show ReadError where
  show (ReadParse e)         = "ReadParse " <> show e
  show (ReadMessageTooLong n) = "ReadMessageTooLong " <> show n
  show ReadUnexpectedEof     = "ReadUnexpectedEof"
  show (ReadTransportError exc) = "ReadTransportError " <> show exc

------------------------------------------------------------------------
-- C imports — share the SIMD scanners with the classic recv-buffer.
------------------------------------------------------------------------

foreign import ccall unsafe "hs_http1_find_cr"
  c_find_cr :: Ptr Word8 -> CInt -> CInt -> CInt

------------------------------------------------------------------------
-- Default caps (match @Network.HTTP1.Internal.RecvBuffer@)
------------------------------------------------------------------------

-- | h2o's default request-header block cap.
defaultHeaderBlockCap :: Int
defaultHeaderBlockCap = 32 * 1024

-- | Conservative bound on a single chunk-size line.
defaultChunkLineCap :: Int
defaultChunkLineCap = 4 * 1024

------------------------------------------------------------------------
-- Header block reader
------------------------------------------------------------------------

-- | Read the full HTTP\/1.x request \/ response head — everything up
-- to and including the first @\\r\\n\\r\\n@ — as a single contiguous
-- zero-copy 'BS.ByteString' slice over the magic-ring's backing
-- memory.
--
-- Advances the transport tail past the terminator on success, so a
-- subsequent call sees the bytes starting at the body's first octet.
--
-- The returned slice is valid until the next call that advances the
-- ring tail past it (i.e. for the duration of the current
-- request-handling scope).  If the caller needs to retain the bytes
-- beyond that scope it MUST @BS.copy@.
readHeaderBlock
  :: Transport
  -> Int          -- ^ hard cap; @ReadMessageTooLong@ if exceeded.
  -> IO (Either ReadError ByteString)
readHeaderBlock t cap = do
  startPos <- transportLoadHead t
  fmap (fmap fst) (readHeaderBlockFrom t startPos cap)

-- | Like 'readHeaderBlock' but starting from an explicit ring
-- position (matches the @startPos@ argument shape of
-- 'Wireform.Parser.Driver.runParserInternal') and returns the
-- @(block, newStartPos)@ pair so callers can loop without
-- re-reading the transport head between iterations.  Used by
-- benchmark harnesses and by connection-layer pull loops that
-- track position themselves.
readHeaderBlockFrom
  :: Transport
  -> Word64
  -> Int
  -> IO (Either ReadError (ByteString, Word64))
readHeaderBlockFrom t startPos cap = do
  let !ring = transportRing t
      !base = ringBase ring
      !msk  = ringMask ring
  loop ring base msk startPos
  where
    loop !ring !base !msk !sPos = do
      -- We're free to wait for at least one byte; the scanner
      -- needs four bytes to confirm CRLFCRLF, so anything less is
      -- "keep going".
      h <- transportLoadHead t
      let !avail = fromIntegral (h - sPos) :: Int
      if avail > cap
        then pure (Left (ReadMessageTooLong cap))
        else do
          mIdx <- findCRLFCRLF base msk sPos avail
          case mIdx of
            Just idx -> do
              -- @idx@ is the offset (from sPos) of the first
              -- CR of the terminator.  Block is [sPos, sPos+idx).
              let !blockLen   = idx
                  !block      = ringSlice ring sPos blockLen
                  !nextPos    = sPos + fromIntegral (blockLen + 4)
              transportAdvanceTail t nextPos
              pure (Right (block, nextPos))
            Nothing -> do
              -- Need more bytes; suspend on the IO manager.
              r <- transportWaitData t h
              case r of
                MoreData _         -> loop ring base msk sPos
                EndOfInput         -> pure (Left ReadUnexpectedEof)
                TransportError exc -> pure (Left (ReadTransportError exc))

-- | Locate the byte offset (relative to @startPos@) of the next
-- @\\r\\n\\r\\n@ in the ring.  Returns 'Nothing' when no terminator
-- is in the @[startPos, startPos+avail)@ window.
--
-- The double-mapped ring guarantees that any read of up to
-- @ringSize@ bytes starting anywhere in @[base, base + N)@ is
-- contiguous.  We rely on that — the scanner reads up to
-- @avail@ bytes from @base + (startPos .&. mask)@ without any wrap
-- handling.
findCRLFCRLF
  :: Ptr Word8
  -> Int            -- ^ mask = ringSize - 1
  -> Word64         -- ^ start position
  -> Int            -- ^ bytes available from start
  -> IO (Maybe Int)
findCRLFCRLF base msk startPos avail
  | avail < 4 = pure Nothing
  | otherwise = do
      let !startOff = fromIntegral startPos .&. msk
          !startPtr = base `plusPtr` startOff
      go startPtr 0
  where
    -- We linear-scan for CR using the SIMD finder, then check the
    -- three follow-up bytes (LF / CR / LF).  Most CRs in a header
    -- block belong to inter-header CRLFs, so the follow-up check
    -- fails fast and we resume scanning from CR+1.
    go !ptr !off
      | off + 4 > avail = pure Nothing
      | otherwise = do
          let !crPos = fromIntegral
                (c_find_cr (castPtr ptr) (fromIntegral off) (fromIntegral avail))
          if crPos + 4 > avail
            then pure Nothing
            else do
              b1 <- peekByteOff ptr (crPos + 1) :: IO Word8
              b2 <- peekByteOff ptr (crPos + 2) :: IO Word8
              b3 <- peekByteOff ptr (crPos + 3) :: IO Word8
              if b1 == 0x0a && b2 == 0x0d && b3 == 0x0a
                then pure (Just crPos)
                else go ptr (crPos + 1)
{-# INLINE findCRLFCRLF #-}

-- | Locate the byte offset of the next @\\r\\n@ in the ring (returns
-- the offset of the CR).
findCRLF
  :: Ptr Word8
  -> Int
  -> Word64
  -> Int
  -> IO (Maybe Int)
findCRLF base msk startPos avail
  | avail < 2 = pure Nothing
  | otherwise = do
      let !startOff = fromIntegral startPos .&. msk
          !startPtr = base `plusPtr` startOff
      go startPtr 0
  where
    go !ptr !off
      | off + 2 > avail = pure Nothing
      | otherwise = do
          let !crPos = fromIntegral
                (c_find_cr (castPtr ptr) (fromIntegral off) (fromIntegral avail))
          if crPos + 2 > avail
            then pure Nothing
            else do
              b1 <- peekByteOff ptr (crPos + 1) :: IO Word8
              if b1 == 0x0a
                then pure (Just crPos)
                else go ptr (crPos + 1)
{-# INLINE findCRLF #-}

------------------------------------------------------------------------
-- Slice helper
------------------------------------------------------------------------

-- | Build a zero-copy 'BS.ByteString' slice over @[pos, pos + len)@
-- of the ring's backing memory.  Constructs the 'ForeignPtr' with
-- 'FinalPtr' (no finalizer) directly via the 'GHC.ForeignPtr'
-- constructor — this is the same convention 'Wireform.Parser.takeBs'
-- follows, and it avoids the 'newForeignPtr_' allocation overhead
-- that a more conservative @unsafePerformIO (newForeignPtr_ ptr)@
-- would otherwise pay on every per-frame slice.
--
-- The slice becomes a dangling pointer if it outlives the magic
-- ring's 'withMagicRing' bracket; callers that retain past that
-- scope MUST @BS.copy@.
ringSlice :: MagicRing -> Word64 -> Int -> ByteString
ringSlice ring pos (I# len#) =
  let !base = ringBase ring
      !msk  = ringMask ring
      !off  = fromIntegral pos .&. msk
      !(Ptr addr#) = base `plusPtr` off
  in BSI.BS (ForeignPtr addr# FinalPtr) (I# len#)
{-# INLINE ringSlice #-}

------------------------------------------------------------------------
-- High-level: full request / response head
------------------------------------------------------------------------

-- | Read a full HTTP\/1.x request head from the transport.
--
-- Composes:
--
--   1. 'readHeaderBlock' — magic-ring + SIMD CRLFCRLF.
--   2. 'Network.HTTP1.Parser.parseRequest' — SIMD tchar /
--      field-vchar scan over the resulting slice.
--   3. Smuggling-guard pipeline (validateHost, validateTarget,
--      requestFraming) shared with the classic parser.
--
-- Returns the parsed 'Request' (body slot stays @BodyEmpty@; the
-- caller wires it up using the returned 'Framing') or a 'ReadError'.
readRequestHead :: Transport -> IO (Either ReadError (Request, Framing))
readRequestHead t = do
  startPos <- transportLoadHead t
  fmap (fmap fst) (readRequestHeadFrom t startPos)
{-# INLINE readRequestHead #-}

-- | Like 'readRequestHead' but tracks position explicitly.  Returns
-- @(result, newPos)@ so callers can loop reading successive
-- requests on a keep-alive connection without re-reading
-- 'transportLoadHead' (which gives the head, not the consumed
-- position) between iterations.
readRequestHeadFrom
  :: Transport
  -> Word64
  -> IO (Either ReadError ((Request, Framing), Word64))
readRequestHeadFrom t startPos = do
  blockE <- readHeaderBlockFrom t startPos defaultHeaderBlockCap
  pure $ case blockE of
    Left e               -> Left e
    Right (block, nextPos) -> case Classic.parseRequest block of
      Right ok -> Right (ok, nextPos)
      Left  e  -> Left (ReadParse e)
{-# INLINE readRequestHeadFrom #-}

-- | Read a full HTTP\/1.x response head from the transport.
--
-- Takes the request method so 'responseFraming' can apply the
-- HEAD \/ CONNECT special cases.
readResponseHead
  :: Transport
  -> Method
  -> IO (Either ReadError (Response, Framing))
readResponseHead t reqMethod = do
  startPos <- transportLoadHead t
  fmap (fmap fst) (readResponseHeadFrom t startPos reqMethod)
{-# INLINE readResponseHead #-}

readResponseHeadFrom
  :: Transport
  -> Word64
  -> Method
  -> IO (Either ReadError ((Response, Framing), Word64))
readResponseHeadFrom t startPos reqMethod = do
  blockE <- readHeaderBlockFrom t startPos defaultHeaderBlockCap
  pure $ case blockE of
    Left e               -> Left e
    Right (block, nextPos) -> case Classic.parseResponse reqMethod block of
      Right ok -> Right (ok, nextPos)
      Left  e  -> Left (ReadParse e)
{-# INLINE readResponseHeadFrom #-}

------------------------------------------------------------------------
-- Chunk-size line
------------------------------------------------------------------------

-- | Read one chunked TE @chunk-size [ chunk-ext ] CRLF@ line and
-- return the parsed size in bytes.  Walks the ring + SIMD CR scanner
-- to find the CRLF, then defers to 'Classic.parseChunkSize' for the
-- hex / extension grammar.
readChunkSizeLine
  :: Transport
  -> IO (Either ReadError Word64)
readChunkSizeLine t = do
  startPos <- transportLoadHead t
  fmap (fmap fst) (readChunkSizeLineFrom t startPos)

readChunkSizeLineFrom
  :: Transport
  -> Word64
  -> IO (Either ReadError (Word64, Word64))
readChunkSizeLineFrom t startPos = do
  let !ring = transportRing t
      !base = ringBase ring
      !msk  = ringMask ring
  loop ring base msk startPos
  where
    loop !ring !base !msk !sPos = do
      h <- transportLoadHead t
      let !avail = fromIntegral (h - sPos) :: Int
      if avail > defaultChunkLineCap
        then pure (Left (ReadMessageTooLong defaultChunkLineCap))
        else do
          mIdx <- findCRLF base msk sPos avail
          case mIdx of
            Just idx -> do
              let !lineLen = idx
                  !line    = ringSlice ring sPos lineLen
                  !nextPos = sPos + fromIntegral (lineLen + 2)
              transportAdvanceTail t nextPos
              pure $ case Classic.parseChunkSize line of
                Right n -> Right (n, nextPos)
                Left e  -> Left (ReadParse e)
            Nothing -> do
              r <- transportWaitData t h
              case r of
                MoreData _         -> loop ring base msk sPos
                EndOfInput         -> pure (Left ReadUnexpectedEof)
                TransportError exc -> pure (Left (ReadTransportError exc))

