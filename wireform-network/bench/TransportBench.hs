{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Benchmark: magic ring transport vs standard network recv.
--
-- == Architecture
--
-- The magic ring is pre-allocated once (one ring per connection lifetime).
-- Per-benchmark-iteration cost is the lightweight recv+parse work only.
--
-- == What's measured
--
-- Five scenarios are compared for each workload:
--
--   * @stream (ring, no I\/O)@: data pre-filled in ring, no suspension.
--     Isolates the streaming parser's inner-loop overhead — should match
--     the pure parse exactly (and does).
--
--   * @stream (ring, 1 suspend)@: data in ring but head starts at 0,
--     forcing one control0#\/prompt# suspension round-trip before parsing.
--     Measures the GHC delimited-continuation constant.
--
--   * @transport+ring (net)@: full transport path over a loopback socket
--     using recvBuf directly into the ring.
--
--   * @recv+concat+parse@: standard Haskell network pattern — recv loop,
--     BS.concat, then parseByteString.
--
--   * @recvBuf+pinned@: recv+memcpy into a pre-allocated pinned buffer,
--     then parseByteString (removes concat overhead).
module Main where

import Criterion.Main
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, finally, toException)
import qualified Control.Exception as E
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.ByteString.Internal (ByteString (..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BSU
import Data.IORef
import Data.Word
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Network.Socket hiding (close)
import qualified Network.Socket as S
import Network.Socket.ByteString (sendAll, recv)

import Wireform.Parser
import Wireform.Parser.Internal (Stream, Pure, ParserMode)
import Wireform.Parser.Driver
import Wireform.Ring.Internal (MagicRing, ringBase, ringSize, ringMask, withMagicRing)
import Wireform.Transport
import Wireform.Transport.Config (defaultTransportConfig, ringSizeHint)

------------------------------------------------------------------------
-- Parsers
------------------------------------------------------------------------

word32sP :: ParserMode m => Int -> Parser m () ()
word32sP 0 = pure ()
word32sP n = do
  !_ <- anyWord32be
  word32sP (n - 1)
{-# INLINE word32sP #-}

lenPrefixedP :: ParserMode m => Int -> Parser m () ()
lenPrefixedP 0 = pure ()
lenPrefixedP n = do
  !len <- anyWord8
  !_ <- takeBs (fromIntegral len)
  lenPrefixedP (n - 1)
{-# INLINE lenPrefixedP #-}

singleMsgP :: ParserMode m => Parser m () ByteString
singleMsgP = do
  len <- anyWord8
  takeBs (fromIntegral len)
{-# INLINE singleMsgP #-}

------------------------------------------------------------------------
-- Input generation
------------------------------------------------------------------------

mkWord32Input :: Int -> ByteString
mkWord32Input n = LBS.toStrict . BSB.toLazyByteString $
  mconcat [ BSB.word32BE (fromIntegral i) | i <- [0 .. n - 1] ]

mkLenPrefixedInput :: Int -> ByteString
mkLenPrefixedInput n = LBS.toStrict . BSB.toLazyByteString $
  mconcat [ BSB.word8 32 <> BSB.byteString (BS.replicate 32 (fromIntegral i))
          | i <- [0 .. n - 1]
          ]

------------------------------------------------------------------------
-- Socket helpers
------------------------------------------------------------------------

-- | Unix domain socket pair — no TIME_WAIT, no port exhaustion,
-- no listener socket. Much cheaper per iteration than TCP loopback.
connectedPair :: IO (Socket, Socket)
connectedPair = socketPair AF_UNIX Stream defaultProtocol

recvAll :: Socket -> Int -> IO ByteString
recvAll sock total = go [] total
  where
    go acc 0 = pure $! BS.concat (reverse acc)
    go acc left = do
      chunk <- recv sock (min left (64 * 1024))
      if BS.null chunk
        then pure $! BS.concat (reverse acc)
        else go (chunk : acc) (left - BS.length chunk)

------------------------------------------------------------------------
-- Pre-fill ring with a ByteString
------------------------------------------------------------------------

prefillRing :: MagicRing s -> ByteString -> IO ()
prefillRing ring payload =
  BSU.unsafeUseAsCStringLen payload \(src, len) ->
    copyBytes (ringBase ring) (castPtr src) len

------------------------------------------------------------------------
-- Transports
------------------------------------------------------------------------

-- | In-memory transport: all data visible immediately (no suspension).
mkPrefilledTransport :: MagicRing s -> Int -> IO Transport
mkPrefilledTransport ring payloadLen = do
  let !headPos = fromIntegral payloadLen :: Word64
  pure Transport
    { transportRingBaseField = ringBase ring
    , transportRingSizeField = ringSize ring
    , transportRingMaskField = ringMask ring
    , transportLoadHead      = pure headPos
    , transportAdvanceTail   = \_ -> pure ()
    , transportWaitData      = \_ -> pure EndOfInput
    , transportClose         = pure ()
    }

-- | In-memory transport: head starts at 0, first waitData delivers data.
-- Forces exactly one suspension/resume cycle.
mkSuspendOnceTransport :: MagicRing s -> Int -> IO Transport
mkSuspendOnceTransport ring payloadLen = do
  let !headPos = fromIntegral payloadLen :: Word64
  headRef <- newIORef (0 :: Word64)
  pure Transport
    { transportRingBaseField = ringBase ring
    , transportRingSizeField = ringSize ring
    , transportRingMaskField = ringMask ring
    , transportLoadHead      = readIORef headRef
    , transportAdvanceTail   = \_ -> pure ()
    , transportWaitData      = \_ -> do
        writeIORef headRef headPos
        pure (MoreData headPos)
    , transportClose         = pure ()
    }

-- | Network recv transport reusing a pre-allocated ring.
withRecvTransportReuse :: MagicRing s -> Socket -> (Transport -> IO a) -> IO a
withRecvTransportReuse ring sock action = do
  let !base = ringBase ring
      !msk  = ringMask ring
      !sz   = ringSize ring
  headRef  <- newIORef (0 :: Word64)
  tailRef  <- newIORef (0 :: Word64)
  eofRef   <- newIORef False
  errRef   <- newIORef (Nothing :: Maybe SomeException)
  let loadHead = readIORef headRef
      advanceTail pos = writeIORef tailRef pos
      waitData pos = do
        isEof <- readIORef eofRef
        if isEof then pure EndOfInput
        else do
          mbErr <- readIORef errRef
          case mbErr of
            Just e  -> pure (TransportError e)
            Nothing -> doRecv pos
      doRecv pos = do
        h <- readIORef headRef
        if h > pos then pure (MoreData h)
        else do
          t <- readIORef tailRef
          let !writeOff  = fromIntegral h .&. msk
              !writePtr  = base `plusPtr` writeOff
              !available = sz - fromIntegral (h - t)
              !maxRecv   = min available (sz - writeOff)
          if maxRecv <= 0 then pure (MoreData h)
          else do
            result <- E.try @IOException (S.recvBuf sock writePtr maxRecv)
            case result of
              Left exc -> do
                writeIORef errRef (Just (toException exc))
                pure (TransportError (toException exc))
              Right n
                | n == 0 -> do
                    writeIORef eofRef True
                    pure EndOfInput
                | otherwise -> do
                    let !newHead = h + fromIntegral n
                    writeIORef headRef newHead
                    pure (MoreData newHead)
      transport = Transport
        { transportRingBaseField = base
        , transportRingSizeField = sz
        , transportRingMaskField = msk
        , transportLoadHead      = loadHead
        , transportAdvanceTail   = advanceTail
        , transportWaitData      = waitData
        , transportClose         = writeIORef eofRef True
        }
  action transport

------------------------------------------------------------------------
-- Benchmark functions
------------------------------------------------------------------------

benchStreamNoSuspend :: MagicRing s -> ByteString -> Parser Stream () () -> IO ()
benchStreamNoSuspend ring payload parser = do
  prefillRing ring payload
  t <- mkPrefilledTransport ring (BS.length payload)
  r <- runParserInternal t parser 0
  case r of
    IRDone _ () -> pure ()
    _           -> error "stream-nosuspend failed"

benchStreamOneSuspend :: MagicRing s -> ByteString -> Parser Stream () () -> IO ()
benchStreamOneSuspend ring payload parser = do
  prefillRing ring payload
  t <- mkSuspendOnceTransport ring (BS.length payload)
  r <- runParser t parser
  case r of
    Right () -> pure ()
    Left e   -> error ("stream-1suspend failed: " <> show e)

benchTransport :: MagicRing s -> ByteString -> Parser Stream () () -> IO ()
benchTransport ring payload parser = do
  (sender, receiver) <- connectedPair
  senderDone <- newEmptyMVar
  _ <- forkIO $ sendAll sender payload `finally` putMVar senderDone ()
  withRecvTransportReuse ring receiver \t -> do
    r <- runParser t parser
    case r of
      Right () -> pure ()
      Left e   -> error ("transport parse failed: " <> show e)
  takeMVar senderDone
  S.close sender
  S.close receiver

benchTransportLoop :: MagicRing s -> ByteString -> Int -> IO ()
benchTransportLoop ring payload expectedMsgs = do
  (sender, receiver) <- connectedPair
  senderDone <- newEmptyMVar
  _ <- forkIO $ sendAll sender payload `finally` putMVar senderDone ()
  countRef <- newIORef (0 :: Int)
  withRecvTransportReuse ring receiver \t -> do
    r <- runParserLoop t (singleMsgP @Stream) \(!_msg) -> do
      n <- readIORef countRef
      let !n' = n + 1
      writeIORef countRef n'
      pure $ if n' >= expectedMsgs then Stop else Continue
    case r of
      Right () -> pure ()
      Left e   -> error ("transport loop failed: " <> show e)
  takeMVar senderDone
  S.close sender
  S.close receiver

benchRecvConcat :: ByteString -> Parser Pure () () -> IO ()
benchRecvConcat payload parser = do
  (sender, receiver) <- connectedPair
  senderDone <- newEmptyMVar
  _ <- forkIO $ sendAll sender payload `finally` putMVar senderDone ()
  allData <- recvAll receiver (BS.length payload)
  case parseByteString parser allData of
    Right () -> pure ()
    Left e   -> error ("recv+concat parse failed: " <> show e)
  takeMVar senderDone
  S.close sender
  S.close receiver

benchRecvBuf :: ByteString -> Parser Pure () () -> IO ()
benchRecvBuf payload parser = do
  let !payloadLen = BS.length payload
  (sender, receiver) <- connectedPair
  senderDone <- newEmptyMVar
  _ <- forkIO $ sendAll sender payload `finally` putMVar senderDone ()
  fp <- mallocForeignPtrBytes payloadLen
  withForeignPtr fp \buf -> do
    recvIntoBuf receiver buf payloadLen
    let !bs = BS fp payloadLen
    case parseByteString parser bs of
      Right () -> pure ()
      Left e   -> error ("recvBuf parse failed: " <> show e)
  takeMVar senderDone
  S.close sender
  S.close receiver

recvIntoBuf :: Socket -> Ptr Word8 -> Int -> IO ()
recvIntoBuf sock base total = go 0
  where
    go !off
      | off >= total = pure ()
      | otherwise = do
          let !maxChunk = min (total - off) (64 * 1024)
          bs <- recv sock maxChunk
          let !n = BS.length bs
          if n == 0
            then pure ()
            else do
              BSU.unsafeUseAsCStringLen bs \(src, len) ->
                copyBytes (base `plusPtr` off) (castPtr src) len
              go (off + n)

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = withMagicRing (ringSizeHint defaultTransportConfig) \ring -> do
  let !n10k  = 10000
      !n100k = 100000

  let !w32_10k  = mkWord32Input n10k
      !w32_100k = mkWord32Input n100k
      !lp_10k   = mkLenPrefixedInput n10k
      !lp_100k  = mkLenPrefixedInput n100k

  putStrLn   "Payload sizes:"
  putStrLn $ "  word32 x10K:   " <> show (BS.length w32_10k) <> " bytes"
  putStrLn $ "  word32 x100K:  " <> show (BS.length w32_100k) <> " bytes"
  putStrLn $ "  len-pfx x10K:  " <> show (BS.length lp_10k) <> " bytes"
  putStrLn $ "  len-pfx x100K: " <> show (BS.length lp_100k) <> " bytes"

  defaultMain
    [ bgroup "word32be x10K (40 KB)"
        [ bench "stream (no suspend)"   $ nfIO (benchStreamNoSuspend ring w32_10k (word32sP @Stream n10k))
        , bench "stream (1 suspend)"    $ nfIO (benchStreamOneSuspend ring w32_10k (word32sP @Stream n10k))
        , bench "transport+ring (net)"  $ nfIO (benchTransport ring w32_10k (word32sP @Stream n10k))
        , bench "recv+concat+parse"     $ nfIO (benchRecvConcat w32_10k (word32sP @Pure n10k))
        , bench "recvBuf+pinned"        $ nfIO (benchRecvBuf w32_10k (word32sP @Pure n10k))
        ]
    , bgroup "word32be x100K (400 KB)"
        [ bench "stream (no suspend)"   $ nfIO (benchStreamNoSuspend ring w32_100k (word32sP @Stream n100k))
        , bench "stream (1 suspend)"    $ nfIO (benchStreamOneSuspend ring w32_100k (word32sP @Stream n100k))
        , bench "transport+ring (net)"  $ nfIO (benchTransport ring w32_100k (word32sP @Stream n100k))
        , bench "recv+concat+parse"     $ nfIO (benchRecvConcat w32_100k (word32sP @Pure n100k))
        , bench "recvBuf+pinned"        $ nfIO (benchRecvBuf w32_100k (word32sP @Pure n100k))
        ]
    , bgroup "length-prefixed x10K (330 KB)"
        [ bench "stream (no suspend)"        $ nfIO (benchStreamNoSuspend ring lp_10k (lenPrefixedP @Stream n10k))
        , bench "stream (1 suspend)"         $ nfIO (benchStreamOneSuspend ring lp_10k (lenPrefixedP @Stream n10k))
        , bench "transport+ring (net)"       $ nfIO (benchTransport ring lp_10k (lenPrefixedP @Stream n10k))
        , bench "transport+ring (loop, net)" $ nfIO (benchTransportLoop ring lp_10k n10k)
        , bench "recv+concat+parse"          $ nfIO (benchRecvConcat lp_10k (lenPrefixedP @Pure n10k))
        , bench "recvBuf+pinned"             $ nfIO (benchRecvBuf lp_10k (lenPrefixedP @Pure n10k))
        ]
    , bgroup "length-prefixed x100K (3.3 MB)"
        [ bench "stream (no suspend)"        $ nfIO (benchStreamNoSuspend ring lp_100k (lenPrefixedP @Stream n100k))
        , bench "stream (1 suspend)"         $ nfIO (benchStreamOneSuspend ring lp_100k (lenPrefixedP @Stream n100k))
        , bench "transport+ring (net)"       $ nfIO (benchTransport ring lp_100k (lenPrefixedP @Stream n100k))
        , bench "transport+ring (loop, net)" $ nfIO (benchTransportLoop ring lp_100k n100k)
        , bench "recv+concat+parse"          $ nfIO (benchRecvConcat lp_100k (lenPrefixedP @Pure n100k))
        , bench "recvBuf+pinned"             $ nfIO (benchRecvBuf lp_100k (lenPrefixedP @Pure n100k))
        ]
    ]
