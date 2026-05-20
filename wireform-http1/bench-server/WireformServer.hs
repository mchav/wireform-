{- | Hello-world HTTP\/1.1 server for throughput / latency benchmarking.

Run on its own and hit with @wrk@ \/ @h2load@ \/ @hey@ for an
apples-to-apples comparison against libh2o's @h2o_handler_t@ +
@h2o_send_inline@ static-string fast path.

    wireform-http1-bench-server [PORT]    # default 8080
-}
module Main (main) where

import qualified Data.ByteString as BS
import System.Environment (getArgs)

import Network.HTTP1.Server
import Network.HTTP1.Types

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        (p : _) -> p
        []      -> "8080"
  let cfg = defaultServerConfig
        { serverPort = port
        , serverHandler = handler
        }
  putStrLn $ "wireform-http1-bench-server: listening on " <> port
  runServer cfg

handler :: Handler
handler _req = pure $ Response
  { responseStatus  = OK
  , responseVersion = HTTP_1_1
  , responseHeaders =
      [ ("Content-Type", "text/plain")
      , ("Server", "wireform-http1")
      ]
  , responseBody = BodyBytes body
  }
  where
    body = BS.pack (map (fromIntegral . fromEnum) "Hello, world!\n")
