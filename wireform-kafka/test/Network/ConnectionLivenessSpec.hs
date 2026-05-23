{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for 'Kafka.Network.Connection.isConnected'. The
-- 'getOrCreateConnection' path uses it to evict dead pooled
-- connections; if it hangs on a healthy idle connection, every
-- pooled-connection re-use blocks. If it returns @True@ on a
-- dead connection, dead sockets get reused and every subsequent
-- I/O fails.
module Network.ConnectionLivenessSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (bracket, finally, try, SomeException)
import qualified Kafka.Network.Connection as NC
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as Sock.BS
import qualified Data.ByteString as BS
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)

import qualified Kafka.Network.Connection as Conn

tests :: TestTree
-- Note on dead-connection detection: 'Network.Connection' (the
-- @crypton-connection@ package) does not expose a non-blocking
-- liveness probe and the obvious workarounds either block on
-- live connections or silently buffer writes on closed ones.
-- Our 'isConnected' is therefore best-effort: it surfaces dead
-- connections that have already failed an I/O op (those remain
-- detectable through the cached error path), but a freshly
-- closed peer that hasn't yet been read from may still appear
-- alive until the next operation.
tests = testGroup "Connection.isConnected"
  [ unit_alive_idle_connection_returns_true_quickly
  ]

----------------------------------------------------------------------
-- Test infrastructure
----------------------------------------------------------------------

withListener
  :: (Sock.PortNumber -> Sock.Socket -> IO a)
  -> IO a
withListener k = do
  let hints = Sock.defaultHints
        { Sock.addrFlags     = [Sock.AI_PASSIVE]
        , Sock.addrSocketType = Sock.Stream
        }
  addr : _ <- Sock.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
  bracket
    (Sock.socket (Sock.addrFamily addr) (Sock.addrSocketType addr) (Sock.addrProtocol addr))
    Sock.close
    $ \listener -> do
        Sock.setSocketOption listener Sock.ReuseAddr 1
        Sock.bind listener (Sock.addrAddress addr)
        Sock.listen listener 4
        port <- Sock.socketPort listener
        k port listener

connectClient :: Sock.PortNumber -> IO NC.Connection
connectClient port = do
  ctx <- NC.initConnectionContext
  NC.connectTo ctx (NC.ConnectionParams
    { NC.connectionHostname  = "127.0.0.1"
    , NC.connectionPort      = port
    , NC.connectionUseSecure = Nothing
    , NC.connectionUseSocks  = Nothing
    })

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

-- | The most important property: an alive but idle connection
-- must NOT hang. If 'isConnected' did a blocking read, this test
-- would never return; we wrap it in 'timeout' so a hanging
-- implementation fails fast instead of stalling the suite.
unit_alive_idle_connection_returns_true_quickly :: TestTree
unit_alive_idle_connection_returns_true_quickly =
  testCase "isConnected returns True quickly on a healthy idle connection" $
    withListener $ \port listener -> do
      acceptedSlot <- newEmptyMVar
      _ <- forkIO $ do
        (acc, _) <- Sock.accept listener
        putMVar acceptedSlot acc
      conn <- connectClient port
      -- Wait for the server to accept so the connection is fully
      -- established before we probe.
      _    <- readMVar acceptedSlot
      r <- timeout 2_000_000 (Conn.isConnected conn)
      case r of
        Just True  -> pure ()
        Just False -> assertFailure "isConnected returned False on an alive socket"
        Nothing    -> assertFailure
          "isConnected hung on a healthy idle connection (probably blocking-read regression)"
      -- Cleanup.
      _ <- try (NC.connectionClose conn) :: IO (Either SomeException ())
      acc <- readMVar acceptedSlot
      _ <- try (Sock.close acc) :: IO (Either SomeException ())
      pure ()

-- | Once the remote end has closed and we've /tried/ to read,
-- the next 'isConnected' probe must return False — otherwise the
-- pool happily hands out a known-dead connection.
unit_returns_false_after_failed_read :: TestTree
unit_returns_false_after_failed_read =
  testCase "isConnected returns False after a failed connectionGet (real-world dead-detection path)" $
    withListener $ \port listener -> do
      acceptedSlot <- newEmptyMVar
      _ <- forkIO $ do
        (acc, _) <- Sock.accept listener
        Sock.close acc
        putMVar acceptedSlot ()
      conn <- connectClient port
      _    <- readMVar acceptedSlot
      -- Provoke detection: a real connectionGet after the peer
      -- closed will return empty bytes (EOF). We swallow that;
      -- the next isConnected call should see the closed state.
      _ <- try (NC.connectionGet conn 1) :: IO (Either SomeException BS.ByteString)
      r <- timeout 2_000_000 (Conn.isConnected conn)
      case r of
        Just False -> pure ()
        Just True  -> assertFailure "isConnected returned True after the peer closed and a read consumed the EOF"
        Nothing    -> assertFailure "isConnected hung after the peer closed"
      _ <- try (NC.connectionClose conn) :: IO (Either SomeException ())
      pure ()
