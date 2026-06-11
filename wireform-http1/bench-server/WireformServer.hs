{- | Hello-world HTTP\/1.1 server for throughput / latency benchmarking.

This is the wireform-http1 analogue of @nginx -c hello.conf@ \/
@h2o -c hello.conf@: a single-handler, static-response server tuned
for the same workload h2o and nghttpd publish numbers against.

Tuning knobs that matter for the bench number:

  * Per-capability accept distribution via 'forkOn'. The accept loop is
    single-threaded; the dispatch round-robins across
    'getNumCapabilities' so each connection lives entirely on one core
    (no cross-core wakeups during request handling).
  * @TCP_NODELAY@ on every connection (set by the runtime).
  * Linux-only @TCP_DEFER_ACCEPT@ on the listening socket so the accept
    loop wakes up only when there's already data to read.
  * Pre-encoded static response via 'precomputeResponse' — the response
    is built /once/ at module init and the server's send path then
    emits the wire bytes verbatim with a single @send()@ per request,
    while still going through the normal 'runServer' \/ 'Handler' API
    (so keep-alive, request smuggling guards, HEAD handling, etc. all
    keep working).

Run with:

    cabal run wireform-http1:wireform-http1-bench-server -- 8080
    wrk -t2 -c50 -d10s http://127.0.0.1:8080/
-}
module Main (main) where

import Control.Concurrent (forkOn, getNumCapabilities)
import Data.IORef
import Network.HTTP1.Encode qualified as Enc
import Network.HTTP1.Server
import Network.HTTP1.Status
import Network.HTTP1.Types
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let port = case args of
        (p : _) -> p
        [] -> "8080"
  caps <- getNumCapabilities
  capCounter <- newIORef (0 :: Int)
  let pinningFork io = do
        cap <-
          atomicModifyIORef'
            capCounter
            (\n -> let !n' = if n + 1 >= caps then 0 else n + 1 in (n', n))
        forkOn cap io
  let cfg =
        defaultServerConfig
          { serverHost = "0.0.0.0"
          , serverPort = port
          , serverHandler = handler
          , serverForkConnection = pinningFork
          , serverListenBacklog = 4096
          , serverTcpDeferAcceptSecs = Just 5
          }
  putStrLn $
    "wireform-http1-bench-server: "
      <> show caps
      <> " capabilities, port "
      <> port
  runServer cfg


------------------------------------------------------------------------
-- Pre-encoded responses
------------------------------------------------------------------------

{- | The default GET response: 200 OK with a tiny text body.

'Enc.precomputeResponse' runs the encoder /once/ at module init and
wraps the wire bytes in a 'BodyPreEncoded' marker. The server's send
path recognises that marker and emits the bytes with a single
@send()@ — no encoder run, no headers-list traversal, no Builder
allocation per request.

The Response record stays intact (status, version, headers are still
inspectable) so the framework's keep-alive bookkeeping continues to
work normally. For HEAD requests the server zero-copy slices to
'peHeadLen', so the metadata (incl. Content-Length) survives but the
body is dropped, per RFC 9110 § 9.3.2.
-}
staticOk :: Response
staticOk =
  Enc.precomputeResponse $
    Response
      { responseStatus = OK
      , responseVersion = HTTP_1_1
      , responseHeaders =
          [ ("Content-Type", "text/plain")
          , ("Server", "wireform-http1")
          ]
      , responseBody = BodyBytes "Hello, world!\n"
      , responseTrailers = pure []
      }


handler :: Handler
handler _ = pure staticOk
