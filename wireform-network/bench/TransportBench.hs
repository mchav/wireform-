{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Benchmark: magic ring transport vs standard network recv.
--
-- Three approaches per workload:
--
--   * @transport+ring@: recv into magic ring → streaming parser
--     (pipelining recv and parse, zero-copy takeBs slices from ring)
--
--   * @recv+concat+parse@: recv loop → BS.concat → parseByteString
--     (typical Haskell network code; extra allocation per recv chunk,
--     final O(n) concat copy)
--
--   * @recvBuf+pinned+parse@: recvBuf directly into pre-allocated
--     pinned buffer → parseByteString from that buffer
--     (avoids per-recv ByteString allocation; fairest comparison
--     to the ring since both write recv data to a fixed buffer)
--
-- Each iteration creates a fresh loopback TCP socket pair, so socket
-- setup cost is included.  Payload sizes are chosen to keep this
-- overhead negligible relative to the parsing work.
module Main where

import Criterion.Main
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.ByteString.Internal (ByteString(..))
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
import Wireform.Network.Transport.Recv
import Wireform.Transport.Config

------------------------------------------------------------------------
-- Parsers (polymorphic via ParserMode so they specialize at each site)
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

-- 1-byte length (32) + 32 bytes payload per message
mkLenPrefixedInput :: Int -> ByteString
mkLenPrefixedInput n = LBS.toStrict . BSB.toLazyByteString $
  mconcat [ BSB.word8 32 <> BSB.byteString (BS.replicate 32 (fromIntegral i))
          | i <- [0 .. n - 1]
          ]

------------------------------------------------------------------------
-- Socket helpers
------------------------------------------------------------------------

-- | Create a connected TCP loopback pair. Returns (sender, receiver).
connectedPair :: IO (Socket, Socket)
connectedPair = do
  listener <- socket AF_INET Stream defaultProtocol
  setSocketOption listener ReuseAddr 1
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listener 1
  boundAddr <- getSocketName listener
  accepted <- newEmptyMVar
  _ <- forkIO do
    (server, _) <- accept listener
    putMVar accepted server
  client <- socket AF_INET Stream defaultProtocol
  connect client boundAddr
  server <- takeMVar accepted
  S.close listener
  pure (client, server)

-- | Recv exactly @total@ bytes by accumulating chunks, then BS.concat.
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
-- Benchmark: transport + magic ring + streaming parse
------------------------------------------------------------------------

benchTransport :: ByteString -> Parser Stream () () -> IO ()
benchTransport payload parser = do
  (sender, receiver) <- connectedPair
  senderDone <- newEmptyMVar
  _ <- forkIO $ sendAll sender payload `finally` putMVar senderDone ()
  withRecvTransport defaultTransportConfig receiver \t -> do
    r <- runParser t parser
    case r of
      Right () -> pure ()
      Left e   -> error ("transport parse failed: " <> show e)
  takeMVar senderDone
  S.close sender
  S.close receiver

------------------------------------------------------------------------
-- Benchmark: transport + magic ring + runParserLoop (per-message)
------------------------------------------------------------------------

benchTransportLoop :: ByteString -> Int -> IO ()
benchTransportLoop payload expectedMsgs = do
  (sender, receiver) <- connectedPair
  senderDone <- newEmptyMVar
  _ <- forkIO $ sendAll sender payload `finally` putMVar senderDone ()
  countRef <- newIORef (0 :: Int)
  withRecvTransport defaultTransportConfig receiver \t -> do
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

------------------------------------------------------------------------
-- Benchmark: standard recv + concat + pure parse
------------------------------------------------------------------------

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

------------------------------------------------------------------------
-- Benchmark: recvBuf into pre-allocated pinned buffer + parse
------------------------------------------------------------------------

benchRecvBuf :: ByteString -> Parser Pure () () -> IO ()
benchRecvBuf payload parser = do
  let !payloadLen = BS.length payload
  (sender, receiver) <- connectedPair
  senderDone <- newEmptyMVar
  _ <- forkIO $ sendAll sender payload `finally` putMVar senderDone ()
  fp <- mallocForeignPtrBytes payloadLen
  withForeignPtr fp \buf -> do
    recvIntoBuf' receiver buf payloadLen
    let !bs = BS fp payloadLen
    case parseByteString parser bs of
      Right () -> pure ()
      Left e   -> error ("recvBuf parse failed: " <> show e)
  takeMVar senderDone
  S.close sender
  S.close receiver

-- | Recv into a Ptr using recv + memcpy (same technique as the recv
-- transport). Avoids the accumulate-then-concat pattern, removing
-- the O(n) concat copy and per-chunk list-cons allocation.
recvIntoBuf' :: Socket -> Ptr Word8 -> Int -> IO ()
recvIntoBuf' sock base total = go 0
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
main = do
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
    --
    -- word32be: pure throughput (minimal per-element work)
    --
    [ bgroup "word32be x10K (40 KB)"
        [ bench "transport+ring"     $ nfIO (benchTransport w32_10k (word32sP @Stream n10k))
        , bench "recv+concat+parse"  $ nfIO (benchRecvConcat w32_10k (word32sP @Pure n10k))
        , bench "recvBuf+pinned"     $ nfIO (benchRecvBuf w32_10k (word32sP @Pure n10k))
        ]
    , bgroup "word32be x100K (400 KB)"
        [ bench "transport+ring"     $ nfIO (benchTransport w32_100k (word32sP @Stream n100k))
        , bench "recv+concat+parse"  $ nfIO (benchRecvConcat w32_100k (word32sP @Pure n100k))
        , bench "recvBuf+pinned"     $ nfIO (benchRecvBuf w32_100k (word32sP @Pure n100k))
        ]

    --
    -- Length-prefixed messages: framing + bulk read (realistic protocol pattern)
    --
    , bgroup "length-prefixed x10K (330 KB)"
        [ bench "transport+ring"          $ nfIO (benchTransport lp_10k (lenPrefixedP @Stream n10k))
        , bench "transport+ring (loop)"   $ nfIO (benchTransportLoop lp_10k n10k)
        , bench "recv+concat+parse"       $ nfIO (benchRecvConcat lp_10k (lenPrefixedP @Pure n10k))
        , bench "recvBuf+pinned"          $ nfIO (benchRecvBuf lp_10k (lenPrefixedP @Pure n10k))
        ]
    , bgroup "length-prefixed x100K (3.3 MB)"
        [ bench "transport+ring"          $ nfIO (benchTransport lp_100k (lenPrefixedP @Stream n100k))
        , bench "transport+ring (loop)"   $ nfIO (benchTransportLoop lp_100k n100k)
        , bench "recv+concat+parse"       $ nfIO (benchRecvConcat lp_100k (lenPrefixedP @Pure n100k))
        , bench "recvBuf+pinned"          $ nfIO (benchRecvBuf lp_100k (lenPrefixedP @Pure n100k))
        ]
    ]
