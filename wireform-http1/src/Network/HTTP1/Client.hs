{- | HTTP\/1.x client.

Two primary modes:

* 'sendRequest' / 'sendRequestOn' — one request on a caller-supplied
  'Connection'. Pipelining is the caller's responsibility (issue
  N requests, then read N responses; ordered by the server per RFC
  9112 § 9.3.2).
* 'withClientConnection' — bracket pattern: open a TCP connection,
  run an action, close it.

For TLS (or any non-socket transport) build the 'Connection' via
'newConnectionFromTransport' and call 'sendRequestOn' yourself; the
high-level helpers here are TCP-only.

A pooled variant lives in "Network.HTTP1.Client.Pool".
-}
module Network.HTTP1.Client
  ( ClientConfig (..)
  , defaultClientConfig
  , ClientConnection (..)
  , openClientConnection
  , closeClientConnection
  , withClientConnection
    -- * Per-request API
  , sendRequest
  , sendRequestOn
    -- * Connection introspection
  , clientConnectionSocket
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket (Socket)
import qualified Network.Socket as NS

import qualified Wireform.Builder as B

import Network.HTTP1.Chunked (encodeChunk, encodeLastChunk)
import Network.HTTP1.Connection
import Network.HTTP1.Encode (requestBuilder)
import Network.HTTP1.Parser
import Network.HTTP1.Types

data ClientConfig = ClientConfig
  { clientHost :: !String
  , clientPort :: !String
  , clientMaxHeaderBytes :: !Int
  }

defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig
  { clientHost = "127.0.0.1"
  , clientPort = "80"
  , clientMaxHeaderBytes = 32 * 1024
  }

-- | Opaque handle for a client-side HTTP\/1.x connection.  Construct
-- via 'openClientConnection' (TCP) or by wrapping a
-- 'newConnectionFromTransport' yourself (TLS / other).
newtype ClientConnection = ClientConnection { unClientConnection :: Connection }

-- | The underlying socket, if the connection is socket-backed.  TLS
-- connections return 'Nothing'.
clientConnectionSocket :: ClientConnection -> Maybe Socket
clientConnectionSocket (ClientConnection c) = connectionSocket c

openClientConnection :: ClientConfig -> IO ClientConnection
openClientConnection cfg = do
  let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
  addrs <- NS.getAddrInfo (Just hints) (Just (clientHost cfg)) (Just (clientPort cfg))
  case addrs of
    [] -> error "wireform-http1: no address found"
    (addr : _) -> do
      sock <- NS.openSocket addr
      NS.connect sock (NS.addrAddress addr)
      NS.setSocketOption sock NS.NoDelay 1
      ClientConnection <$> newConnection sock

closeClientConnection :: ClientConnection -> IO ()
closeClientConnection (ClientConnection c) = closeConnection c

withClientConnection :: ClientConfig -> (ClientConnection -> IO a) -> IO a
withClientConnection cfg = bracket (openClientConnection cfg) closeClientConnection

-- | Send a single request on a freshly-opened connection and return
-- the response. The connection is closed afterwards.
sendRequest :: ClientConfig -> Request -> IO (Either ParseError Response)
sendRequest cfg req = withClientConnection cfg $ \conn -> sendRequestOn conn req

-- | Send a request on an existing connection (for keep-alive \/
-- pipelining). The returned response's body is a streaming producer;
-- you MUST consume it (or call 'drainBody') before sending another
-- request on the same connection — otherwise the next response will
-- be misframed.
sendRequestOn :: ClientConnection -> Request -> IO (Either ParseError Response)
sendRequestOn (ClientConnection conn) req = do
  let recv = tRecvBuf (connectionTransport conn)
  sendBuilder conn (requestBuilder req)
  case requestBody req of
    BodyEmpty -> pure ()
    BodyBytes bs -> if BS.null bs then pure () else sendBuilder conn (B.byteString bs)
    BodyStream producer -> streamChunked conn producer
    BodyPreEncoded _ -> pure ()
    -- ^ Pre-encoded request bodies are unusual but not nonsensical
    -- (e.g. a pre-built JSON payload). The Body has already been
    -- baked into the request bytes the user constructed; if they
    -- wanted it sent here they should have used BodyBytes.
    BodyFile _ -> pure ()
    -- ^ Client-side sendfile is a follow-up. For now you can stream
    -- a file with @BodyStream@ + a producer that 'hGet's chunks.
  -- Read the response head.
  mHead <- recvBufferReadUntilDoubleCRLF
             (connectionRecvBuffer conn)
             recv
             (32 * 1024)
  case mHead of
    Nothing -> pure (Left ParseUnexpectedEof)
    Just headBs -> case parseResponse (requestMethod req) headBs of
      Left err -> pure (Left err)
      Right (resp0, framing) -> do
        body <- readBody conn framing
        pure $ Right resp0 { responseBody = body }

streamChunked :: Connection -> IO (Maybe ByteString) -> IO ()
streamChunked conn producer = loop
  where
    loop = do
      mc <- producer
      case mc of
        Nothing -> sendBuilder conn encodeLastChunk
        Just bs
          | BS.null bs -> loop
          | otherwise -> sendBuilder conn (encodeChunk bs) >> loop
