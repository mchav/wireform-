{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Benchmark: magic ring transport vs standard network recv.
--
-- The magic ring is pre-allocated once (as it would be in production —
-- one ring per connection lifetime), so per-iteration cost is only the
-- lightweight recv+parse work, not the mmap setup.
--
-- Three approaches compared per workload:
--
--   * @transport+ring@: recv into magic ring, streaming parser
--     (pipelining, zero-copy takeBs slices from ring memory)
--
--   * @recv+concat+parse@: recv loop → BS.concat → parseByteString
--     (typical Haskell network code; per-recv ByteString alloc,
--     final O(n) concat copy)
--
--   * @recvBuf+pinned@: recv+memcpy into pre-allocated pinned buffer
--     → parseByteString (no concat, isolates the allocation difference)
--
-- Socket pair creation is included in all approaches for fairness.
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

mkLenPrefixedInput :: Int -> ByteString
mkLenPrefixedInput n = LBS.toStrict . BSB.toLazyByteString $
  mconcat [ BSB.word8 32 <> BSB.byteString (BS.replicate 32 (fromIntegral i))
          | i <- [0 .. n - 1]
          ]

------------------------------------------------------------------------
-- Socket helpers
------------------------------------------------------------------------

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
-- Lightweight recv transport from a pre-existing ring
--
-- Mirrors Wireform.Network.Transport.Recv but takes an existing
-- MagicRing instead of creating one. This is what production code
-- effectively does: one ring per connection, many parses.
------------------------------------------------------------------------

withRecvTransportReuse :: MagicRing -> Socket -> (Transport -> IO a) -> IO a
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
        if isEof
          then pure EndOfInput
          else do
            mbErr <- readIORef errRef
            case mbErr of
              Just e  -> pure (TransportError e)
              Nothing -> doRecv pos

      doRecv pos = do
        h <- readIORef headRef
        if h > pos
          then pure (MoreData h)
          else do
            t <- readIORef tailRef
            let !writeOff  = fromIntegral h .&. msk
                !writePtr  = base `plusPtr` writeOff
                !available = sz - fromIntegral (h - t)
                !maxRecv   = min available (sz - writeOff)
            if maxRecv <= 0
              then pure (MoreData h)
              else do
                result <- E.try @IOException (doRawRecv sock writePtr maxRecv)
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
        { transportRing        = ring
        , transportLoadHead    = loadHead
        , transportAdvanceTail = advanceTail
        , transportWaitData    = waitData
        , transportClose       = writeIORef eofRef True
        }

  action transport

doRawRecv :: Socket -> Ptr Word8 -> Int -> IO Int
doRawRecv sock ptr maxLen = recvBuf sock ptr maxLen

------------------------------------------------------------------------
-- Benchmark: transport + magic ring (ring pre-allocated)
------------------------------------------------------------------------

benchTransport :: MagicRing -> ByteString -> Parser Stream () () -> IO ()
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

------------------------------------------------------------------------
-- Benchmark: transport + magic ring + runParserLoop (per-message)
------------------------------------------------------------------------

benchTransportLoop :: MagicRing -> ByteString -> Int -> IO ()
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
-- Benchmark: recv into pre-allocated pinned buffer + parse
------------------------------------------------------------------------

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
        [ bench "transport+ring"     $ nfIO (benchTransport ring w32_10k (word32sP @Stream n10k))
        , bench "recv+concat+parse"  $ nfIO (benchRecvConcat w32_10k (word32sP @Pure n10k))
        , bench "recvBuf+pinned"     $ nfIO (benchRecvBuf w32_10k (word32sP @Pure n10k))
        ]
    , bgroup "word32be x100K (400 KB)"
        [ bench "transport+ring"     $ nfIO (benchTransport ring w32_100k (word32sP @Stream n100k))
        , bench "recv+concat+parse"  $ nfIO (benchRecvConcat w32_100k (word32sP @Pure n100k))
        , bench "recvBuf+pinned"     $ nfIO (benchRecvBuf w32_100k (word32sP @Pure n100k))
        ]
    , bgroup "length-prefixed x10K (330 KB)"
        [ bench "transport+ring"          $ nfIO (benchTransport ring lp_10k (lenPrefixedP @Stream n10k))
        , bench "transport+ring (loop)"   $ nfIO (benchTransportLoop ring lp_10k n10k)
        , bench "recv+concat+parse"       $ nfIO (benchRecvConcat lp_10k (lenPrefixedP @Pure n10k))
        , bench "recvBuf+pinned"          $ nfIO (benchRecvBuf lp_10k (lenPrefixedP @Pure n10k))
        ]
    , bgroup "length-prefixed x100K (3.3 MB)"
        [ bench "transport+ring"          $ nfIO (benchTransport ring lp_100k (lenPrefixedP @Stream n100k))
        , bench "transport+ring (loop)"   $ nfIO (benchTransportLoop ring lp_100k n100k)
        , bench "recv+concat+parse"       $ nfIO (benchRecvConcat lp_100k (lenPrefixedP @Pure n100k))
        , bench "recvBuf+pinned"          $ nfIO (benchRecvBuf lp_100k (lenPrefixedP @Pure n100k))
        ]
    ]
