{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

{- | Head-to-head benchmark: classic recv-buffer based HTTP\/1.x
parsing path vs the new wireform magic-ring transport path.

The classic path is exactly what the connection layer ('Network.HTTP1.Connection.readBody' /
'Network.HTTP1.Server.runServer') runs today: pinned 'RecvBuffer'
filled by a 'RecvFn', then 'recvBufferReadUntilDoubleCRLF' to slice
out the header block, then 'parseRequest' which walks the slice with
the SIMD CR \/ tchar \/ field-value scanners under
'Network.HTTP1.Parser'.

The new path uses 'withRecvBufTransport' to fill a wireform magic
ring + 'requestHeadParser' to walk it with the 'Wireform.Parser'
@Stream@ surface.

Both paths consume the same recv chunks, parse the same request, and
return the framing.  This isolates parser+transport overhead from
everything else (sendfile, body framing, server scheduling).
-}
module Main (main) where

import Control.Exception (bracket)
import Criterion.Main
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)

import Wireform.Network
  ( chunkedRecvFn
  , withRecvBufTransport
  )
import Wireform.Parser.Driver
  ( InternalResult (..)
  , runParser
  , runParserInternal
  )
import Wireform.Ring.Internal
  ( MagicRing
  , destroyMagicRing
  , newMagicRing
  , ringBase
  , ringMask
  , ringSize
  )
import Wireform.Transport
import Wireform.Transport.Config (defaultTransportConfig, ringSizeHint)

import qualified Network.HTTP1.Internal.RecvBuffer as RB
import Network.HTTP1.Parser (parseRequest)
import Network.HTTP1.StreamingParser (requestHeadParser)
import qualified Network.HTTP1.StreamingReader as SR

------------------------------------------------------------------------
-- Sample wire payloads
------------------------------------------------------------------------

smallReq :: BS.ByteString
smallReq = BS.intercalate "\r\n"
  [ "GET / HTTP/1.1"
  , "Host: example.com"
  , "User-Agent: curl/8.4.0"
  , "Accept: */*"
  , ""
  , ""
  ]

bigReq :: BS.ByteString
bigReq = BS.intercalate "\r\n"
  [ "POST /api/v1/things HTTP/1.1"
  , "Host: example.com"
  , "User-Agent: Mozilla/5.0 (X11; Linux x86_64) wireform-http1-bench/0.1"
  , "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
  , "Accept-Language: en-US,en;q=0.5"
  , "Accept-Encoding: gzip, deflate, br"
  , "Content-Type: application/json"
  , "Content-Length: 0"
  , "Cache-Control: no-cache"
  , "Pragma: no-cache"
  , "Authorization: Bearer eyJhbGciOiJSUzI1NiJ9.eyJzdWIiOiJ1c2VyMSJ9.signature"
  , "Cookie: sessionId=abc123; trackingId=xyz789"
  , "X-Forwarded-For: 1.2.3.4"
  , "X-Forwarded-Proto: https"
  , "X-Real-IP: 1.2.3.4"
  , "X-Request-Id: 0123456789abcdef"
  , "If-None-Match: \"deadbeef\""
  , "If-Modified-Since: Wed, 21 Oct 2015 07:28:00 GMT"
  , ""
  , ""
  ]

------------------------------------------------------------------------
-- Classic recv path: RecvBuffer + parseRequest
------------------------------------------------------------------------

-- | One iteration: fresh recv buffer, fresh RecvFn that delivers the
-- payload, pull header block via 'recvBufferReadUntilDoubleCRLF',
-- parse with 'parseRequest'.
classicParse :: BS.ByteString -> IO ()
classicParse payload = do
  rb <- RB.newRecvBuffer
  recvFn <- mkRecvFn [payload]
  Just block <- RB.recvBufferReadUntilDoubleCRLF rb recvFn (32 * 1024)
  case parseRequest block of
    Right _ -> pure ()
    Left e  -> error ("classic parse failed: " <> show e)

-- | Same but with the payload split into many small chunks
-- (mimicking a slow client / TLS record boundaries).
classicParseChunked :: BS.ByteString -> Int -> IO ()
classicParseChunked payload chunkSize = do
  rb <- RB.newRecvBuffer
  recvFn <- mkRecvFn (chunksOf chunkSize payload)
  Just block <- RB.recvBufferReadUntilDoubleCRLF rb recvFn (32 * 1024)
  case parseRequest block of
    Right _ -> pure ()
    Left e  -> error ("classic parse failed: " <> show e)

------------------------------------------------------------------------
-- New transport path: withRecvBufTransport + requestHeadParser
------------------------------------------------------------------------

-- | One iteration with a fresh magic ring + 'withRecvBufTransport'.
-- This is what a *connection setup* pays — useful for measuring the
-- per-connection overhead, but not what a per-request hot loop sees.
transportParse :: BS.ByteString -> IO ()
transportParse payload = do
  recvFn <- chunkedRecvFn [payload]
  r <- withRecvBufTransport defaultTransportConfig recvFn $ \t ->
    runParser t requestHeadParser
  case r of
    Right _ -> pure ()
    Left e  -> error ("transport parse failed: " <> show e)

-- | One iteration using a pre-allocated ring + a freshly-constructed
-- in-memory 'Transport' that pretends the payload is already in the
-- ring.  This matches the per-request cost on a long-lived connection
-- where the ring was set up once at connect time and is reused for
-- every subsequent request — the apples-to-apples comparison against
-- the recv-buffer classic path (which pays a similar
-- 'recvBufferReadUntilDoubleCRLF' cost per request).
transportParseReuse :: MagicRing -> BS.ByteString -> IO ()
transportParseReuse ring payload = do
  prefillRing ring payload
  t <- mkPrefilledTransport ring (BS.length payload)
  -- 'runParserInternal startPos=0' lets us pretend the ring is fresh
  -- on every iteration without paying a real recv cost.
  r <- runParserInternal t requestHeadParser 0
  case r of
    IRDone _ _ -> pure ()
    IRFail pos -> error ("transport (reuse) parse failed at " <> show pos)
    IRErr  pos e -> error ("transport (reuse) parse error at " <> show pos
                            <> ": " <> show e)
    IRUnexpectedEof pos n -> error ("transport (reuse) unexpected EOF at "
                                    <> show pos <> " need " <> show n)
    IRTransportError exc  -> error ("transport (reuse) IO error: " <> show exc)
    IRCleanEof            -> error "transport (reuse): clean EOF"

transportParseReuseChunked :: MagicRing -> BS.ByteString -> Int -> IO ()
transportParseReuseChunked ring payload chunkSize = do
  let chunks = chunksOf chunkSize payload
  recvFn <- chunkedRecvFn chunks
  t      <- mkRingTransport ring recvFn
  r <- runParserInternal t requestHeadParser 0
  case r of
    IRDone _ _ -> pure ()
    IRFail pos -> error ("transport (reuse, chunked) parse failed at "
                          <> show pos)
    IRErr  pos e -> error ("transport (reuse, chunked) parse error at "
                            <> show pos <> ": " <> show e)
    IRUnexpectedEof pos n -> error ("transport (reuse, chunked) unexpected EOF at "
                                     <> show pos <> " need " <> show n)
    IRTransportError exc  -> error ("transport (reuse, chunked) IO error: " <> show exc)
    IRCleanEof            -> error "transport (reuse, chunked): clean EOF"

------------------------------------------------------------------------
-- Fast streaming reader: SIMD CRLFCRLF on the ring + classic parser
------------------------------------------------------------------------

readerParseReuse :: MagicRing -> BS.ByteString -> IO ()
readerParseReuse ring payload = do
  prefillRing ring payload
  t <- mkPrefilledTransport ring (BS.length payload)
  r <- SR.readRequestHeadFrom t 0
  case r of
    Right _ -> pure ()
    Left e  -> error ("reader (reuse) failed: " <> show e)

readerParseReuseChunked :: MagicRing -> BS.ByteString -> Int -> IO ()
readerParseReuseChunked ring payload chunkSize = do
  let chunks = chunksOf chunkSize payload
  recvFn <- chunkedRecvFn chunks
  t      <- mkRingTransport ring recvFn
  r <- SR.readRequestHeadFrom t 0
  case r of
    Right _ -> pure ()
    Left e  -> error ("reader (reuse, chunked) failed: " <> show e)

------------------------------------------------------------------------
-- In-memory transports that reuse a pre-allocated ring
------------------------------------------------------------------------

-- | Memcpy the payload into the start of the ring, no I/O on the
-- benchmark hot path.
prefillRing :: MagicRing -> BS.ByteString -> IO ()
prefillRing ring payload =
  BSU.unsafeUseAsCStringLen payload $ \(src, len) ->
    copyBytes (ringBase ring) (castPtr src) len

mkPrefilledTransport :: MagicRing -> Int -> IO Transport
mkPrefilledTransport ring payloadLen = do
  let !headPos = fromIntegral payloadLen :: Word64
  pure Transport
    { transportRing        = ring
    , transportLoadHead    = pure headPos
    , transportAdvanceTail = \_ -> pure ()
    , transportWaitData    = \_ -> pure EndOfInput
    , transportClose       = pure ()
    }

-- | Reuse an existing ring with the supplied recv callback.  Mirrors
-- the shape of 'withRecvBufTransport' (per-iteration cost is one
-- IORef allocation + a few writeIORefs, NOT a magic-ring mmap).
mkRingTransport :: MagicRing -> (Ptr Word8 -> Int -> IO Int) -> IO Transport
mkRingTransport ring recvIntoBuf = do
  let !base = ringBase ring
      !msk  = ringMask ring
      !sz   = ringSize ring
  headRef <- newIORef (0 :: Word64)
  tailRef <- newIORef (0 :: Word64)
  eofRef  <- newIORef False
  let waitData _ = do
        isEof <- readIORef eofRef
        if isEof then pure EndOfInput
        else do
          h <- readIORef headRef
          t <- readIORef tailRef
          let !writeOff  = fromIntegral h .&. msk
              !writePtr  = base `plusPtr` writeOff
              !available = sz - fromIntegral (h - t)
              !maxRecv   = min available (sz - writeOff)
          if maxRecv <= 0 then pure (MoreData h)
          else do
            n <- recvIntoBuf writePtr maxRecv
            if n == 0
              then do
                writeIORef eofRef True
                pure EndOfInput
              else do
                let !newHead = h + fromIntegral n
                writeIORef headRef newHead
                pure (MoreData newHead)
  pure Transport
    { transportRing        = ring
    , transportLoadHead    = readIORef headRef
    , transportAdvanceTail = writeIORef tailRef
    , transportWaitData    = waitData
    , transportClose       = writeIORef eofRef True
    }

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | A 'RecvFn' that delivers a fixed list of chunks, then EOF.
-- (Local copy because 'RB.RecvFn' is the same shape as the
-- transport-side 'RecvFn' but a different module-level type
-- synonym, so we can't reuse 'chunkedRecvFn' directly here.)
mkRecvFn :: [BS.ByteString] -> IO RB.RecvFn
mkRecvFn chunks0 = do
  ref <- newIORef chunks0
  pure $ \dst want -> do
    cs <- readIORef ref
    case cs of
      [] -> pure 0
      c : rest -> do
        let !take_    = min want (BS.length c)
            !taken    = BS.take take_ c
            !leftover = BS.drop take_ c
        writeIORef ref (if BS.null leftover then rest else leftover : rest)
        copyBSInto dst taken
        pure take_
  where
    copyBSInto :: Ptr Word8 -> BS.ByteString -> IO ()
    copyBSInto dst bs =
      let (fp, off, len) = BSI.toForeignPtr bs
      in withForeignPtr fp $ \src ->
           copyBytes dst (src `plusPtr` off) len

chunksOf :: Int -> BS.ByteString -> [BS.ByteString]
chunksOf n bs
  | BS.null bs = []
  | otherwise  = let (h, t) = BS.splitAt n bs in h : chunksOf n t

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main =
  -- Pre-allocate one magic ring shared across all "reuse" benchmarks
  -- so the per-iteration cost is parser+transport-overhead only,
  -- matching what a long-lived HTTP/1.x keep-alive connection sees.
  bracket
    (newMagicRing (ringSizeHint defaultTransportConfig))
    destroyMagicRing $ \ring ->
  defaultMain
    [ bgroup "small request (whole chunk)"
        [ bench "classic (RecvBuffer + parseRequest)" $
            nfIO (classicParse smallReq)
        , bench "reader reuse (SIMD CRLFCRLF on ring + parseRequest)" $
            nfIO (readerParseReuse ring smallReq)
        , bench "wireform-parser reuse (byte-by-byte)" $
            nfIO (transportParseReuse ring smallReq)
        ]
    , bgroup "big request (whole chunk)"
        [ bench "classic (RecvBuffer + parseRequest)" $
            nfIO (classicParse bigReq)
        , bench "reader reuse (SIMD CRLFCRLF on ring + parseRequest)" $
            nfIO (readerParseReuse ring bigReq)
        , bench "wireform-parser reuse (byte-by-byte)" $
            nfIO (transportParseReuse ring bigReq)
        ]
    , bgroup "big request (64 byte chunks)"
        [ bench "classic (RecvBuffer + parseRequest)" $
            nfIO (classicParseChunked bigReq 64)
        , bench "reader reuse (SIMD CRLFCRLF on ring + parseRequest)" $
            nfIO (readerParseReuseChunked ring bigReq 64)
        , bench "wireform-parser reuse" $
            nfIO (transportParseReuseChunked ring bigReq 64)
        ]
    , bgroup "big request (4 byte chunks)"
        [ bench "classic (RecvBuffer + parseRequest)" $
            nfIO (classicParseChunked bigReq 4)
        , bench "reader reuse (SIMD CRLFCRLF on ring + parseRequest)" $
            nfIO (readerParseReuseChunked ring bigReq 4)
        , bench "wireform-parser reuse" $
            nfIO (transportParseReuseChunked ring bigReq 4)
        ]
    ]
