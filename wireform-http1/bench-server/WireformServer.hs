{- | Hello-world HTTP\/1.1 server for throughput / latency benchmarking.

This is the wireform-http1 analogue of @nginx -c hello.conf@ \/
@h2o -c hello.conf@: a single-handler, static-response server tuned
for the same workload h2o and nghttpd publish numbers against.

Tuning knobs that matter for the bench number:

  * Per-capability accept distribution via 'forkOn'. The accept loop
    is single-threaded; the dispatch round-robins across
    'getNumCapabilities' so each connection lives entirely on one
    core (no cross-core wakeups during request handling).
  * @TCP_NODELAY@ on every connection (set by the runtime).
  * Linux-only @TCP_DEFER_ACCEPT@ on the listening socket so the
    accept loop wakes up only when there's already data to read.
  * Pre-encoded static response head + body so the per-request work
    is parse → method lookup → single @send()@. No allocation of the
    response object and no encoder run on the hot path.

Run with:

    cabal run wireform-http1:wireform-http1-bench-server -- 8080
    wrk -t2 -c50 -d10s http://127.0.0.1:8080/
-}
module Main (main) where

import Control.Concurrent (forkOn, getNumCapabilities)
import Control.Exception (SomeException, bracket, catch)
import qualified Data.ByteString as BS
import Data.IORef
import Network.Socket (Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)

import qualified Network.HTTP1.Connection as Conn
import qualified Network.HTTP1.Encode as Enc
import Network.HTTP1.Parser
import Network.HTTP1.Status
import Network.HTTP1.Types

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let port = case args of
        (p : _) -> p
        []      -> "8080"
  caps <- getNumCapabilities
  putStrLn $ "wireform-http1-bench-server: " <> show caps
           <> " capabilities, port " <> port
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just "0.0.0.0") (Just port)
  case addrs of
    [] -> error "no address"
    (addr : _) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \sock -> do
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.setSocketOption sock NS.NoDelay 1
        NS.bind sock (NS.addrAddress addr)
        setTcpDeferAccept sock 5
        NS.listen sock 4096
        capCounter <- newIORef (0 :: Int)
        acceptLoop caps capCounter sock

-- | Round-robin across capabilities. Each connection runs end-to-end on
-- a single pinned scheduler so the recv buffer, send buffer and parser
-- stay resident in one core's L1\/L2.
acceptLoop :: Int -> IORef Int -> Socket -> IO ()
acceptLoop caps capCounter listenSock = do
  (clientSock, _) <- NS.accept listenSock
  NS.setSocketOption clientSock NS.NoDelay 1
  cap <- atomicModifyIORef' capCounter
           (\n -> let !n' = if n + 1 >= caps then 0 else n + 1 in (n', n))
  _ <- forkOn cap $
    handleClient clientSock
      `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoop caps capCounter listenSock

------------------------------------------------------------------------
-- Per-connection loop
------------------------------------------------------------------------

handleClient :: Socket -> IO ()
handleClient sock = do
  conn <- Conn.newConnection sock
  let rb = Conn.connectionRecvBuffer conn
  let go = do
        mHead <- Conn.recvBufferReadUntilDoubleCRLF rb sock 32768
        case mHead of
          Nothing -> pure ()
          Just headBs -> case parseRequest headBs of
            Left _ -> NBS.sendAll sock badRequest
            Right (req, _framing) -> do
              NBS.sendAll sock (staticResponseFor (requestMethod req))
              go
  go `catch` (\(_ :: SomeException) -> pure ())
  Conn.closeConnection conn

------------------------------------------------------------------------
-- Static responses, pre-encoded once at module init
------------------------------------------------------------------------

-- | The default response: 200 OK with a tiny text body. Pre-encoded once
-- via the normal encoder so we still exercise its shape, just not its
-- per-request cost.
staticGetResponse :: BS.ByteString
staticGetResponse = Enc.encodeResponseHead resp <> body
  where
    body = "Hello, world!\n"
    resp = Response
      { responseStatus  = OK
      , responseVersion = HTTP_1_1
      , responseHeaders =
          [ ("Content-Type", "text/plain")
          , ("Server", "wireform-http1")
          ]
      , responseBody = BodyBytes body
      }

-- | HEAD shares the same head as GET (RFC 9110 § 9.3.2) but no body.
staticHeadResponse :: BS.ByteString
staticHeadResponse = Enc.encodeResponseHead resp
  where
    resp = Response
      { responseStatus  = OK
      , responseVersion = HTTP_1_1
      , responseHeaders =
          [ ("Content-Type", "text/plain")
          , ("Server", "wireform-http1")
          , ("Content-Length", "14")
          ]
      , responseBody = BodyEmpty
      }

staticResponseFor :: Method -> BS.ByteString
staticResponseFor HEAD = staticHeadResponse
staticResponseFor _    = staticGetResponse
{-# INLINE staticResponseFor #-}

badRequest :: BS.ByteString
badRequest = Enc.encodeResponseHead $ Response
  { responseStatus  = BadRequest
  , responseVersion = HTTP_1_1
  , responseHeaders =
      [ ("Connection", "close")
      , ("Content-Length", "0")
      ]
  , responseBody = BodyEmpty
  }

------------------------------------------------------------------------
-- Linux-only socket tuning
------------------------------------------------------------------------

-- | Enable @TCP_DEFER_ACCEPT@ with a small timeout on a listening
-- socket. The kernel won't return the socket from @accept()@ until at
-- least one byte of data has arrived, so we skip the wakeup-then-poll
-- pattern that costs us 1-2 µs per connection.
--
-- On non-Linux systems @TCP_DEFER_ACCEPT@ doesn't exist and the call
-- silently no-ops; that's fine.
setTcpDeferAccept :: Socket -> Int -> IO ()
setTcpDeferAccept sock secs = do
  let optName = NS.SockOpt ipprotoTcp tcpDeferAccept
  _ <- NS.setSocketOption sock optName secs
         `catch` (\(_ :: SomeException) -> pure ())
  pure ()
  where
    ipprotoTcp = 6
    tcpDeferAccept = 9  -- /usr/include/netinet/tcp.h on Linux
