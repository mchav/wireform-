{-# LANGUAGE OverloadedStrings #-}

module Wireform.Network.Transport.Recv.Test (spec) where

import Control.Concurrent.Async (withAsync)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Test.Hspec

import Wireform.Parser
import Wireform.Parser.Driver
import Wireform.Network.Transport.Recv
import Wireform.Transport.Config

spec :: Spec
spec = describe "RecvTransport" $ do
  it "parses a simple message from a socket" $ do
    (result, _) <- withSocketPair $ \(client, server) -> do
      sendAll client "hello"
      close client
      withRecvTransport defaultTransportConfig server $ \t ->
        runParser t (takeBs 5)
    result `shouldBe` Right "hello"

  it "parses a sequence of messages in a loop" $ do
    (result, _) <- withSocketPair $ \(client, server) -> do
      sendAll client "\x05hello\x05world"
      close client
      ref <- newIORef []
      withRecvTransport defaultTransportConfig server $ \t -> do
        r <- runParserLoop t (anyWord8 >>= \len -> takeBs (fromIntegral len)) $ \msg -> do
          modifyIORef ref (msg :)
          pure Continue
        msgs <- reverse <$> readIORef ref
        pure (r, msgs)
    let (r, msgs) = result
    r `shouldBe` Right ()
    msgs `shouldBe` ["hello", "world"]

  it "handles EOF at message boundary cleanly" $ do
    (result, _) <- withSocketPair $ \(client, server) -> do
      sendAll client "\x03abc"
      close client
      withRecvTransport defaultTransportConfig server $ \t ->
        runParserLoop t (anyWord8 >>= \len -> takeBs (fromIntegral len)) $ \_ ->
          pure Continue
    result `shouldBe` Right ()

  it "reports unexpected EOF mid-message" $ do
    (result, _) <- withSocketPair $ \(client, server) -> do
      sendAll client "\x0A"  -- says 10 bytes follow, but nothing does
      close client
      withRecvTransport defaultTransportConfig server $ \t ->
        runParser t (anyWord8 >>= \len -> takeBs (fromIntegral len))
    case result of
      Left (ParseUnexpectedEof _ _) -> pure ()
      Left (ParseFail _) -> pure ()  -- also acceptable
      other -> expectationFailure ("unexpected: " <> show other)

------------------------------------------------------------------------
-- Test helpers
------------------------------------------------------------------------

-- | Create a connected socket pair for testing.
-- Returns (client, server) where client writes and server reads.
withSocketPair :: ((Socket, Socket) -> IO a) -> IO (a, ())
withSocketPair action = do
  ready <- newEmptyMVar
  let addr = SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1))
  listener <- socket AF_INET Stream defaultProtocol
  setSocketOption listener ReuseAddr 1
  bind listener addr
  listen listener 1
  boundAddr <- getSocketName listener
  result <- newIORef undefined
  withAsync (do
    (server, _) <- accept listener
    putMVar ready server
    ) $ \_ -> do
      client <- socket AF_INET Stream defaultProtocol
      connect client boundAddr
      server <- takeMVar ready
      r <- action (client, server)
      writeIORef result r
      close server
  close listener
  r <- readIORef result
  pure (r, ())
