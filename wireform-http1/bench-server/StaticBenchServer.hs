{- | Static-file HTTP\/1.1 server for the apples-to-apples comparison
against @nginx sendfile on@ and @h2o file.dir@.

Serves the contents of a single file at @\/@ for every GET, using
'BodyFile' so the server's send path picks the @sendfile(2)@ branch:

  1. Open the file once at startup (so we don't pay the open cost per
     request — same as @h2o file.dir@'s mmap-then-serve shape).
  2. Stat once to get the size for @Content-Length@.
  3. Per request: emit head (auto-injected @Content-Length@ + @Date@)
     + @sendfile@ the body. No userspace buffer touch.

We re-open the file on every request rather than caching the fd
across requests because each request opens its own fd via
@bracket@ (so a leaked fd from a misbehaving handler can't outlive
the request). For a true production static-file handler we'd want
to cache an open fd per (path, mtime) and revalidate; that's a
follow-up.

Run:

    cabal run wireform-http1:wireform-http1-static-bench-server -- 8084 /tmp/hello.txt
    wrk -t2 -c50 -d10s http://127.0.0.1:8084/
-}
module Main (main) where

import Control.Concurrent (forkOn, getNumCapabilities)
import Data.ByteString qualified as BS
import Data.IORef
import Network.HTTP1.Server
import Network.HTTP1.Status
import Network.HTTP1.Types
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdout)


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  let (port, path, mime) = case args of
        (p : f : m : _) -> (p, f, m)
        (p : f : _) -> (p, f, "application/octet-stream")
        [p] -> (p, "/tmp/hello.txt", "text/plain")
        [] -> ("8084", "/tmp/hello.txt", "text/plain")
  caps <- getNumCapabilities
  capCounter <- newIORef (0 :: Int)
  let pinningFork io = do
        cap <-
          atomicModifyIORef'
            capCounter
            (\n -> let !n' = if n + 1 >= caps then 0 else n + 1 in (n', n))
        forkOn cap io
  -- Open the file once at startup and cache its fd in the 'FileBody'.
  -- Every request reuses the same fd — no 'open()' / 'close()' on
  -- the hot path, just one 'sendfile(2)' call.
  --
  -- This is the @nginx open_file_cache@ / @h2o file.dir@ shape and
  -- the comparison target for the published numbers from those
  -- servers. The fd lives until the process exits.
  fb <- wholeFileBodyFd path
  let mimeBs = stringToBS mime
      cfg =
        defaultServerConfig
          { serverHost = "0.0.0.0"
          , serverPort = port
          , serverHandler = staticHandler fb mimeBs
          , serverForkConnection = pinningFork
          , serverListenBacklog = 4096
          , serverTcpDeferAcceptSecs = Just 5
          }
  putStrLn $
    "wireform-http1-static-bench-server: "
      <> show caps
      <> " capabilities, port "
      <> port
      <> ", serving "
      <> path
      <> " ("
      <> show (fbLength fb)
      <> " bytes, "
      <> mime
      <> ")"
  runServer cfg


staticHandler :: FileBody -> BS.ByteString -> Handler
staticHandler fb mime = \_req ->
  pure
    Response
      { responseStatus = OK
      , responseVersion = HTTP_1_1
      , responseHeaders =
          [ ("Content-Type", mime)
          , ("Server", "wireform-http1")
          ]
      , responseBody = BodyFile fb
      , responseTrailers = pure []
      }
{-# INLINE staticHandler #-}


{- | ASCII-only conversion is enough for HTTP header values; if the
mime type carries non-ASCII (it shouldn't) we just truncate to the
low byte.
-}
stringToBS :: String -> BS.ByteString
stringToBS = BS.pack . map (fromIntegral . fromEnum)
