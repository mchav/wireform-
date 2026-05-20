{- | Hello-world server using the public 'runServer' API.

This is the apples-to-apples comparison number against frameworks like
@warp@ \/ @http-types+wai@: same dispatch shape (user-supplied
'Handler'), but with our SIMD parser \/ single-send encoder \/ pinned
recv buffer underneath. Use it to track how close the public API gets
to the precomputed bench-server (which sets the upper bound for the
same hardware).

Run with:

    cabal run wireform-http1:wireform-http1-runserver-bench -- 8081
    wrk -t2 -c50 -d10s http://127.0.0.1:8081/
-}
module Main (main) where

import Control.Concurrent (forkOn, getNumCapabilities)
import Data.IORef
import System.Environment (getArgs)

import Network.HTTP1.Server
import Network.HTTP1.Status
import Network.HTTP1.Types

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        (p : _) -> p
        []      -> "8081"
  caps <- getNumCapabilities
  capCounter <- newIORef (0 :: Int)
  let pinningFork io = do
        cap <- atomicModifyIORef' capCounter
                 (\n -> let !n' = if n + 1 >= caps then 0 else n + 1 in (n', n))
        forkOn cap io
  let cfg = defaultServerConfig
        { serverHost = "0.0.0.0"
        , serverPort = port
        , serverHandler = handler
        , serverForkConnection = pinningFork
        }
  putStrLn $ "wireform-http1-runserver-bench: " <> show caps
           <> " capabilities, port " <> port
  runServer cfg

handler :: Handler
handler _req = pure $ Response
  { responseStatus  = OK
  , responseVersion = HTTP_1_1
  , responseHeaders =
      [ ("Content-Type", "text/plain")
      , ("Server", "wireform-http1")
      ]
  , responseBody = BodyBytes "Hello, world!\n"
  }
