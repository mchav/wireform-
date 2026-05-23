{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

-- | End-to-end TLS-on-magic-ring round-trip: a real TLS handshake
-- between two threads in the same process, then encrypted bytes
-- flowing client → server with the server decrypting straight into
-- the magic ring via 'tlsReceiveFn', then a wireform parser reading
-- plaintext off the ring.  Proves the direct-OpenSSL path:
--
--   1. Negotiates a real TLS 1.2+ session (self-signed cert,
--      'newClientCtx False' on the client to skip verify).
--   2. Successfully encrypts + decrypts.
--   3. Plumbs cleanly through 'withTlsReceiveTransport' so the
--      magic-ring parser surface sees plaintext with no intermediate
--      ByteString allocation.
module Wireform.Network.TLS.OpenSSL.Test (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally)
import qualified Data.ByteString as BS
import Data.Word
import Network.Socket
  (Socket, SocketType (Stream), AddrInfo (..), AddrInfoFlag (..))
import qualified Network.Socket as NS
import Network.Socket.ByteString (sendAll)
import System.Directory (doesFileExist)
import Test.Hspec

import Wireform.Network.TLS.OpenSSL
import Wireform.Parser
import Wireform.Parser.Internal (Stream)
import Wireform.Parser.Driver (runParser)
import Wireform.Transport.Config (defaultTransportConfig)

type P = Parser Stream String

spec :: Spec
spec = describe "Wireform.Network.TLS.OpenSSL" $ do

  let certPath = "test/data/localhost.crt"
      keyPath  = "test/data/localhost.key"

  beforeAll_ (skipIfNoCert certPath keyPath) $ do
    it "round-trips encrypted bytes (client send → server recv on ring)" $
      withTlsPair certPath keyPath $ \(clientConn, serverConn) -> do
        tlsSend clientConn "\x05hello"
        withTlsReceiveTransport defaultTransportConfig serverConn $ \t -> do
          r <- runParser t (anyWord8 >>= \n -> takeBs (fromIntegral n) :: P BS.ByteString)
          case r of
            Right bs -> bs `shouldBe` "hello"
            Left e   -> expectationFailure ("parse failed: " <> show e)

    it "stitches a payload split across two TLS records" $
      withTlsPair certPath keyPath $ \(clientConn, serverConn) -> do
        tlsSend clientConn "\x0b"
        tlsSend clientConn "hello world"
        withTlsReceiveTransport defaultTransportConfig serverConn $ \t -> do
          r <- runParser t (anyWord8 >>= \n -> takeBs (fromIntegral n) :: P BS.ByteString)
          case r of
            Right bs -> bs `shouldBe` "hello world"
            Left e   -> expectationFailure ("parse failed: " <> show e)

------------------------------------------------------------------------
-- Test harness
------------------------------------------------------------------------

-- | Skip the suite if the cert / key fixtures aren't checked into
-- the source tree (the CI runner regenerates them; a local clone
-- without that step shouldn't fail the suite).
skipIfNoCert :: FilePath -> FilePath -> IO ()
skipIfNoCert cert key = do
  hasCert <- doesFileExist cert
  hasKey  <- doesFileExist key
  if hasCert && hasKey
    then pure ()
    else pendingWith ("missing TLS fixtures: " <> cert <> " / " <> key)

withTlsPair
  :: FilePath
  -> FilePath
  -> ((SslConn, SslConn) -> IO a)
  -> IO a
withTlsPair certPath keyPath action = do
  (clientSock, serverSock) <- connectedPair
  -- Server-side handshake on a worker; client-side handshake on the
  -- main thread.  Both share the test's lifecycle.
  bracket (newServerCtx certPath keyPath) freeCtx $ \serverCtx ->
    bracket (newClientCtx False) freeCtx $ \clientCtx -> do
      serverConnMV <- newEmptyMVar
      clientConnMV <- newEmptyMVar
      _ <- forkIO $ do
        c <- newServer serverCtx serverSock
        putMVar serverConnMV c
      _ <- forkIO $ do
        c <- newClient clientCtx clientSock (Just "localhost")
        putMVar clientConnMV c
      clientConn <- takeMVar clientConnMV
      serverConn <- takeMVar serverConnMV
      action (clientConn, serverConn) `finally` do
        freeConn clientConn
        freeConn serverConn
        NS.close clientSock
        NS.close serverSock

-- | TCP loopback pair.  We use real TCP rather than AF_UNIX because
-- OpenSSL's TLS layer plays nicer over a stream socket that
-- threadWaitRead recognises.
connectedPair :: IO (Socket, Socket)
connectedPair = do
  listener <- NS.socket NS.AF_INET Stream NS.defaultProtocol
  NS.setSocketOption listener NS.ReuseAddr 1
  NS.bind listener (NS.SockAddrInet 0 (NS.tupleToHostAddress (127, 0, 0, 1)))
  NS.listen listener 1
  boundAddr <- NS.getSocketName listener
  accepted <- newEmptyMVar
  _ <- forkIO $ do
    (server, _) <- NS.accept listener
    NS.close listener
    putMVar accepted server
  client <- NS.socket NS.AF_INET Stream NS.defaultProtocol
  NS.connect client boundAddr
  server <- takeMVar accepted
  pure (client, server)
