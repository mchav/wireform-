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
import qualified Network.HTTP1.StreamingReader as SR
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
--
-- The response body is fully read and materialised as a
-- 'BodyBytes' before the connection is torn down — otherwise the
-- magic ring the body producer reads from would be freed by the
-- 'closeClientConnection' that the inner bracket runs, and the
-- producer's subsequent reads would access unmapped memory.
-- Applications that need to stream large response bodies should
-- use 'sendRequestOn' with their own 'withClientConnection'
-- scope so the connection (and its ring) stays alive while the
-- body is being consumed.
sendRequest :: ClientConfig -> Request -> IO (Either ParseError Response)
sendRequest cfg req = withClientConnection cfg $ \conn -> do
  r <- sendRequestOn conn req
  case r of
    Left e     -> pure (Left e)
    Right resp -> do
      bs  <- collectBody (responseBody resp)
      trs <- responseTrailers resp
      pure $ Right resp
        { responseBody     = BodyBytes bs
        , responseTrailers = pure trs
        }

-- | Drain a 'Body' into a single contiguous 'ByteString'.
collectBody :: Body -> IO ByteString
collectBody = \case
  BodyEmpty           -> pure BS.empty
  BodyBytes bs        -> pure bs
  BodyPreEncoded _    -> pure BS.empty
  BodyFile _          -> pure BS.empty
  BodyStream producer -> loop []
    where
      loop acc = do
        mc <- producer
        case mc of
          Nothing -> pure (BS.concat (reverse acc))
          Just c  -> loop (c : acc)

-- | Send a request on an existing connection (for keep-alive \/
-- pipelining). The returned response's body is a streaming producer;
-- you MUST consume it (or call 'drainBody') before sending another
-- request on the same connection — otherwise the next response will
-- be misframed.
sendRequestOn :: ClientConnection -> Request -> IO (Either ParseError Response)
sendRequestOn (ClientConnection conn) req = do
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
  -- Read the response head.  RFC 9110 §15.2 / RFC 9112 §4.1: 1xx
  -- (informational) responses are interim — keep reading until we
  -- see the final one.  This is how an @Expect: 100-continue@ on
  -- the request gets its 100 acknowledgement transparently absorbed.
  readFinal
  where
    readFinal = do
      hE <- readResponseHead conn (requestMethod req)
      case hE of
        Left SR.ReadUnexpectedEof    -> pure (Left ParseUnexpectedEof)
        Left (SR.ReadMessageTooLong _) -> pure (Left ParseMessageTooLong)
        Left (SR.ReadTransportError _) -> pure (Left ParseUnexpectedEof)
        Left (SR.ReadParse e)          -> pure (Left e)
        Right (resp0, framing)
          | is1xx (responseStatus resp0) -> readFinal
          | otherwise -> do
              (body, trailersIO) <- readBodyAndTrailers conn framing
              pure $ Right resp0
                { responseBody     = body
                , responseTrailers = trailersIO
                }
    is1xx (Status code) = code >= 100 && code < 200

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
