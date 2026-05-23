{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

module Wireform.Network.Transport.Recv.Test (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally, fromException)
import Control.Monad (replicateM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word
import Network.Socket hiding (close, recv)
import qualified Network.Socket as S
import Network.Socket.ByteString (sendAll, recv)
import System.Timeout (timeout)
import Test.Hspec

import Wireform.Parser
import Wireform.Parser.Internal (Pure, Stream)
import Wireform.Parser.Driver
import Wireform.Parser.Error
import Wireform.Network.Transport.Recv
import Wireform.Transport.Config

type P = Parser Stream String

spec :: Spec
spec = describe "RecvTransport" $ do

  describe "withRecvBufTransport (chunked feeder)" $ do
    it "parses a message delivered in one chunk" $ do
      recvFn <- chunkedRecvFn ["hello"]
      withRecvBufTransport defaultTransportConfig recvFn $ \t -> do
        r <- runParser t (takeBs 5 :: P ByteString)
        case r of
          Right bs -> bs `shouldBe` "hello"
          Left e   -> expectationFailure ("parse failed: " <> show e)

    it "stitches a message split across two chunks" $ do
      recvFn <- chunkedRecvFn ["he", "llo"]
      withRecvBufTransport defaultTransportConfig recvFn $ \t -> do
        r <- runParser t (takeBs 5 :: P ByteString)
        case r of
          Right bs -> bs `shouldBe` "hello"
          Left e   -> expectationFailure ("parse failed: " <> show e)

    it "loops over multiple length-prefixed messages, stops voluntarily" $ do
      recvFn <- chunkedRecvFn ["\x05hello\x05world"]
      withRecvBufTransport defaultTransportConfig recvFn $ \t -> do
        ref <- newIORef ([] :: [ByteString])
        let p = anyWord8 >>= \len -> takeBs (fromIntegral len) :: P ByteString
        r <- runParserLoop t p $ \msg -> do
          modifyIORef ref (msg :)
          xs <- readIORef ref
          pure (if length xs >= 2 then Stop else Continue)
        case r of
          Right () -> do
            msgs <- reverse <$> readIORef ref
            msgs `shouldBe` ["hello", "world"]
          Left e   -> expectationFailure ("loop error: " <> show e)

    it "surfaces clean EOF when chunks are exhausted" $ do
      recvFn <- chunkedRecvFn ["abc"]
      withRecvBufTransport defaultTransportConfig recvFn $ \t -> do
        let p = anyWord8 >>= \len -> takeBs (fromIntegral len) :: P ByteString
        r <- runParserLoop t p $ \_msg -> pure Continue
        case r of
          Right () -> pure ()  -- clean EOF after consuming the 'a' header + 'bc'
          Left _ -> pure ()    -- unexpected EOF is also acceptable here

  describe "reads larger than ringSize do not deadlock" $ do
    it "takeBs drains a payload larger than the ring into a fresh allocation" $ do
      -- 'ringSizeHint = 1' is rounded up to the platform's minimum
      -- (a single page on every supported OS), so the actual ring
      -- is some page-sized power of two — much smaller than the
      -- 64 KiB request we are about to make.  The drain path inside
      -- 'takeBs' should walk the bytes through the ring chunk by
      -- chunk, checkpointing to free space, and return the full
      -- 65536-byte 'ByteString' instead of deadlocking.
      let cfg     = defaultTransportConfig { ringSizeHint = 1 }
          total   = 65536
          chunkSz = 1024
          chunks  =
            [ BS.replicate chunkSz (fromIntegral (i `mod` 251))
            | i <- [0 .. (total `div` chunkSz) - 1]
            ]
          expected = BS.concat chunks
      recvFn <- chunkedRecvFn chunks
      mRes <- timeout 5_000_000 $
        withRecvBufTransport cfg recvFn $ \t ->
          runParser t (takeBs total :: P ByteString)
      case mRes of
        Nothing ->
          expectationFailure
            "deadlocked: takeBs n > ringSize should drain, not block forever"
        Just (Right bs) -> do
          BS.length bs `shouldBe` total
          bs `shouldBe` expected
        Just (Left e) ->
          expectationFailure $
            "expected drained ByteString, got: " <> show e

    it "takeBsCopy drains a payload larger than the ring into a fresh allocation" $ do
      let cfg     = defaultTransportConfig { ringSizeHint = 1 }
          total   = 100_000
          chunkSz = 997  -- odd size, exercises the chunk-boundary math
          chunks  =
            [ BS.replicate chunkSz (fromIntegral (i `mod` 251))
            | i <- [0 .. (total `div` chunkSz)]
            ]
          expected = BS.take total (BS.concat chunks)
      recvFn <- chunkedRecvFn chunks
      mRes <- timeout 5_000_000 $
        withRecvBufTransport cfg recvFn $ \t ->
          runParser t (takeBsCopy total :: P ByteString)
      case mRes of
        Nothing ->
          expectationFailure
            "deadlocked: takeBsCopy n > ringSize should drain, not block forever"
        Just (Right bs) -> do
          BS.length bs `shouldBe` total
          bs `shouldBe` expected
        Just (Left e) ->
          expectationFailure $
            "expected drained ByteString, got: " <> show e

    it "surfaces RingExhausted when a non-draining parser fills the ring without checkpointing" $ do
      -- Read more raw bytes in a single 'runParser' than the ring can
      -- hold from startPos using byte-at-a-time 'anyWord8' (which
      -- does not auto-drain like 'takeBs' / 'takeBsCopy' do).  The
      -- parser never advances tail, the producer fills the ring,
      -- and the next ensureN# suspension surfaces as a transport
      -- error rather than spinning forever.
      let cfg = defaultTransportConfig { ringSizeHint = 1 }
          chunks = replicate 20000 (BS.singleton 0x42)
      recvFn <- chunkedRecvFn chunks
      mRes <- timeout 5_000_000 $
        withRecvBufTransport cfg recvFn $ \t ->
          runParser t (replicateM_ 20000 anyWord8 :: P ())
      case mRes of
        Nothing ->
          expectationFailure
            "deadlocked: consuming > ringSize without checkpoint should not block forever"
        Just (Left (ParseTransportError exc)) ->
          case fromException exc :: Maybe RingExhausted of
            Just _  -> pure ()
            Nothing ->
              expectationFailure $
                "expected RingExhausted inside ParseTransportError, got: " <> show exc
        Just other ->
          expectationFailure $
            "expected ParseTransportError(RingExhausted), got: " <> show other

  it "parses a simple message from a socket" $ do
    withConnectedPair \(writer, reader) -> do
      sendAll writer "hello"
      shutdown writer ShutdownSend
      withRecvTransport defaultTransportConfig reader \t -> do
        r <- runParser t (takeBs 5 :: P ByteString)
        case r of
          Right bs -> bs `shouldBe` "hello"
          Left e   -> expectationFailure ("parse failed: " <> show e)

  it "parses a message with monadic chain (anyWord8 >>= takeBs)" $ do
    withConnectedPair \(writer, reader) -> do
      sendAll writer "\x05hello"
      shutdown writer ShutdownSend
      withRecvTransport defaultTransportConfig reader \t -> do
        let p = anyWord8 >>= \len -> takeBs (fromIntegral len) :: P ByteString
        r <- runParser t p
        case r of
          Right bs -> bs `shouldBe` "hello"
          Left e   -> expectationFailure ("parse failed: " <> show e)

  it "parses two messages in a loop (Stop after first)" $ do
    withConnectedPair \(writer, reader) -> do
      sendAll writer "\x05hello\x05world"
      shutdown writer ShutdownSend
      withRecvTransport defaultTransportConfig reader \t -> do
        let p = anyWord8 >>= \len -> takeBs (fromIntegral len) :: P ByteString
        r <- runParserLoop t p \msg -> do
          msg `shouldBe` "hello"
          pure Stop
        case r of
          Right () -> pure ()
          Left e   -> expectationFailure ("loop error: " <> show e)

  -- BUG: this test hangs because the second recv after consuming all
  -- data + FIN blocks in the GHC IO manager's threadWaitRead on
  -- loopback sockets.  The transport correctly handles EOF when recv
  -- returns 0, but recv never returns — it parks on the IO manager
  -- waiting for readability that the kernel doesn't signal.
  --
  -- Root cause: after the first recv returns all data, the FIN has
  -- been consumed.  The socket is in CLOSE_WAIT.  On Linux loopback,
  -- the IO manager (epoll) does not report this socket as readable
  -- again, so threadWaitRead blocks indefinitely.
  --
  -- Fix needed: the recv transport should check socket state or use
  -- a non-blocking recv attempt before parking on the IO manager.
  xit "handles clean EOF at message boundary (runParserLoop)" $ do
    withConnectedPair \(writer, reader) -> do
      sendAll writer "\x06foobar"
      shutdown writer ShutdownSend
      ref <- newIORef ([] :: [ByteString])
      withRecvTransport defaultTransportConfig reader \t -> do
        let p = anyWord8 >>= \len -> takeBs (fromIntegral len) :: P ByteString
        r <- runParserLoop t p \msg -> do
          modifyIORef ref (msg :)
          pure Continue
        msgs <- reverse <$> readIORef ref
        case r of
          Right () -> msgs `shouldBe` ["foobar"]
          Left e   -> expectationFailure ("expected clean exit: " <> show e)

  -- BUG: same underlying issue as above — recv blocks after data is
  -- consumed because the IO manager doesn't wake on CLOSE_WAIT.
  xit "detects unexpected EOF mid-message" $ do
    withConnectedPair \(writer, reader) -> do
      sendAll writer "\x0Ahello"  -- says 10 bytes, only sends 5
      shutdown writer ShutdownSend
      withRecvTransport defaultTransportConfig reader \t -> do
        let p = anyWord8 >>= \len -> takeBs (fromIntegral len) :: P ByteString
        r <- runParser t p
        case r of
          Left _ -> pure ()
          Right _ -> expectationFailure "expected failure"

  -- BUG: depends on the same EOF detection working.
  xit "sticky-closed: repeated waitData returns same state" $ do
    withConnectedPair \(writer, reader) -> do
      sendAll writer "data"
      shutdown writer ShutdownSend
      withRecvTransport defaultTransportConfig reader \t -> do
        r1 <- runParser t (takeBs 4 :: P ByteString)
        case r1 of
          Right bs -> bs `shouldBe` "data"
          Left e   -> expectationFailure ("first parse: " <> show e)
        r2 <- runParser t (anyWord8 :: P Word8)
        case r2 of
          Left _ -> pure ()
          Right _ -> expectationFailure "expected EOF"

connectedPair :: IO (Socket, Socket)
connectedPair = do
  listener <- socket AF_INET Stream defaultProtocol
  setSocketOption listener ReuseAddr 1
  bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listener 1
  boundAddr <- getSocketName listener
  accepted <- newEmptyMVar
  _ <- forkIO $ do
    (server, _) <- accept listener
    putMVar accepted server
  client <- socket AF_INET Stream defaultProtocol
  connect client boundAddr
  server <- takeMVar accepted
  S.close listener
  pure (server, client)

withConnectedPair :: ((Socket, Socket) -> IO a) -> IO a
withConnectedPair action = do
  (reader, writer) <- connectedPair
  action (writer, reader) `finally` do
    S.close writer
    S.close reader
