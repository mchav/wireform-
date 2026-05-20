{- | HTTP\/1.x server runtime.

The runtime is intentionally minimal: it implements RFC 9112 framing,
keep-alive, and pipelining, and delegates everything else to the
application handler.

Pipeline rules (RFC 9112 § 9.3.2):

  * Server SHOULD process requests in the order they arrive on a
    connection.
  * Server MUST send responses in the same order.

We implement this by running the handler synchronously on the
connection thread (one accept = one OS thread = one connection), so
ordering is automatic. Applications that want to fan out request
handling can fork their own worker and stash the response back via an
'Control.Concurrent.MVar.MVar'.

Connection lifetime:

  * Read request head -> framing -> request body producer.
  * Call user handler.
  * After the handler returns:
      - drain unread body bytes (so the next request lines up).
      - decide whether to keep alive (RFC 9112 § 9.3): respect
        @Connection: close@ from either side and HTTP\/1.0 default.
  * Close on parse error with an appropriate status.
-}
module Network.HTTP1.Server
  ( ServerConfig (..)
  , defaultServerConfig
  , runServer
  , runServerOnSocket
  , Handler
  ) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Exception (bracket, catch, SomeException, finally)
import qualified Data.ByteString as BS
import Network.Socket (Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS

import qualified Wireform.Builder as B

import Network.HTTP1.Chunked (encodeChunk, encodeLastChunk)
import Network.HTTP1.Connection
import Network.HTTP1.Encode (responseBuilder)
import Network.HTTP1.Headers
import Network.HTTP1.Parser
import Network.HTTP1.Status
import Network.HTTP1.Types

-- | The application's request handler.
--
-- The handler is called once per request. The 'Request' it receives has
-- a 'BodyStream' producer that pulls from the recv buffer; pulling is
-- optional but if you don't pull, the server will drain whatever is
-- left for you before reading the next request.
type Handler = Request -> IO Response

data ServerConfig = ServerConfig
  { serverHost :: !String
  , serverPort :: !String
  , serverHandler :: !Handler
  , serverForkConnection :: IO () -> IO ThreadId
    -- ^ How to fork a new thread for each accepted connection. Default:
    -- 'forkIO'. Use 'Control.Concurrent.forkOn' for pinned-core
    -- scheduling, which the bench-server demonstrates.
  , serverMaxHeaderBytes :: !Int
    -- ^ Cap on request head size (request line + headers + CRLFCRLF).
    -- h2o's default is 16 KiB; we use 32 KiB. Oversized heads get a
    -- 431 response.
  , serverKeepAlive :: !Bool
    -- ^ If 'False', the server emits @Connection: close@ on every
    -- response and tears down after each request. Useful for tests
    -- and for unusual deployments.
  , serverListenBacklog :: !Int
    -- ^ TCP @listen()@ backlog. Default 1024. Raise to 4096+ for very
    -- high accept rates.
  , serverTcpDeferAcceptSecs :: !(Maybe Int)
    -- ^ If 'Just n', set Linux's @TCP_DEFER_ACCEPT@ to @n@ seconds on
    -- the listening socket. With this enabled, @accept()@ won't
    -- return a connection until at least one byte of data has arrived
    -- on it — skipping a syscall round-trip per connection. On
    -- non-Linux kernels the call silently no-ops. Default 'Nothing'.
  }

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { serverHost = "0.0.0.0"
  , serverPort = "8080"
  , serverHandler = \_ -> pure $ Response OK HTTP_1_1 [] (BodyBytes "")
  , serverForkConnection = forkIO
  , serverMaxHeaderBytes = 32 * 1024
  , serverKeepAlive = True
  , serverListenBacklog = 1024
  , serverTcpDeferAcceptSecs = Nothing
  }

runServer :: ServerConfig -> IO ()
runServer cfg = do
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just (serverHost cfg)) (Just (serverPort cfg))
  case addrs of
    [] -> error "wireform-http1: no address found"
    (addr : _) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \sock -> do
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.setSocketOption sock NS.NoDelay 1
        NS.bind sock (NS.addrAddress addr)
        applyTcpDeferAccept (serverTcpDeferAcceptSecs cfg) sock
        NS.listen sock (serverListenBacklog cfg)
        acceptLoop cfg sock

-- | Apply Linux's @TCP_DEFER_ACCEPT@ if requested. Swallows errors
-- (the option doesn't exist on non-Linux kernels; that's fine).
applyTcpDeferAccept :: Maybe Int -> Socket -> IO ()
applyTcpDeferAccept Nothing _ = pure ()
applyTcpDeferAccept (Just secs) sock = do
  let optName = NS.SockOpt 6 {- IPPROTO_TCP -} 9 {- TCP_DEFER_ACCEPT -}
  NS.setSocketOption sock optName secs
    `catch` (\(_ :: SomeException) -> pure ())

acceptLoop :: ServerConfig -> Socket -> IO ()
acceptLoop cfg listenSock = do
  (clientSock, _) <- NS.accept listenSock
  NS.setSocketOption clientSock NS.NoDelay 1
  _ <- serverForkConnection cfg $
    handleClient cfg clientSock
      `catch` (\(_ :: SomeException) -> NS.close clientSock)
  acceptLoop cfg listenSock

runServerOnSocket :: ServerConfig -> Socket -> IO ()
runServerOnSocket = handleClient

handleClient :: ServerConfig -> Socket -> IO ()
handleClient cfg sock = do
  conn <- newConnection sock
  loop conn `finally` closeConnection conn
  where
    loop conn = do
      mHead <- recvBufferReadUntilDoubleCRLF
                 (connectionRecvBuffer conn)
                 (connectionSocket conn)
                 (serverMaxHeaderBytes cfg)
      case mHead of
        Nothing -> pure ()  -- EOF or oversized head
        Just headBs -> case parseRequest headBs of
          Left err -> do
            sendErrorResponse conn (errorToStatus err)
          Right (req0, framing) -> do
            body <- readBody conn framing
            let req = req0 { requestBody = body }
            resp0 <- (serverHandler cfg req) `catch` (\(_ :: SomeException) ->
                       pure (Response InternalServerError HTTP_1_1 [] (BodyBytes "")))
            let willClose = decideClose cfg req resp0
                resp =
                  if willClose
                    then addCloseHeader resp0
                    else resp0
            sendResponse conn (requestMethod req) resp
            drainBody (requestBody req)
            if willClose
              then pure ()
              else loop conn

------------------------------------------------------------------------
-- Response writing
------------------------------------------------------------------------

-- | Send a complete response (head + body) over the connection.
--
-- Three fast paths:
--
--   1. 'BodyPreEncoded' — emit the precomputed wire bytes with a
--      single 'NBS.sendAll'. For HEAD we slice to 'peHeadLen' so the
--      body is dropped while the metadata survives (RFC 9110 § 9.3.2).
--   2. 'BodyBytes' — combine head + body in one builder, one send.
--   3. Everything else falls back to the head-first send + body stream.
sendResponse :: Connection -> Method -> Response -> IO ()
sendResponse conn reqMethod resp = do
  let mustOmitBody =
        reqMethod == HEAD
          || let sc = case responseStatus resp of Status w -> w
             in (sc >= 100 && sc < 200) || sc == 204 || sc == 304
  let sock = connectionSocket conn
  case responseBody resp of
    BodyPreEncoded pe ->
      let bs = if mustOmitBody then preEncodedHead pe else peBytes pe
      in NBS.sendAll sock bs
    body -> do
      let headBs = B.toStrictByteStringWith 1024 (responseBuilder resp)
      if mustOmitBody
        then NBS.sendAll sock headBs
        else case body of
          BodyEmpty -> NBS.sendAll sock headBs
          BodyBytes bs
            | BS.null bs -> NBS.sendAll sock headBs
            | otherwise  ->
                -- Vectored I/O: head + body land in one writev() with
                -- no body copy. The kernel takes both buffers from
                -- their native locations (head from our scratch pinned
                -- buffer, body from the caller-supplied ByteString).
                NBS.sendMany sock [headBs, bs]
          BodyStream producer -> do
            -- Streaming bodies need the head out first (handler-driven
            -- back-pressure) so we don't accumulate the whole body in
            -- RAM.
            NBS.sendAll sock headBs
            case responseVersion resp of
              HTTP_1_1 -> streamChunked conn producer
              HTTP_1_0 -> streamRaw conn producer

streamChunked :: Connection -> IO (Maybe BS.ByteString) -> IO ()
streamChunked conn producer = loop
  where
    loop = do
      mc <- producer
      case mc of
        Nothing -> sendBuilder conn encodeLastChunk
        Just bs ->
          if BS.null bs
            then loop  -- skip empty chunk (would be a premature terminator)
            else do
              sendBuilder conn (encodeChunk bs)
              loop

streamRaw :: Connection -> IO (Maybe BS.ByteString) -> IO ()
streamRaw conn producer = loop
  where
    loop = do
      mc <- producer
      case mc of
        Nothing -> pure ()
        Just bs -> sendBuilder conn (B.byteString bs) >> loop

------------------------------------------------------------------------
-- Errors / framing decisions
------------------------------------------------------------------------

-- | Send a static error response after a parse failure. Always closes
-- the connection because at that point our reader is desynced from the
-- wire and we can't safely recover.
sendErrorResponse :: Connection -> Status -> IO ()
sendErrorResponse conn st = do
  let resp = Response
        { responseStatus = st
        , responseVersion = HTTP_1_1
        , responseHeaders = [("Connection", "close"), ("Content-Length", "0")]
        , responseBody = BodyEmpty
        }
  sendBuilder conn (responseBuilder resp)
    `catch` (\(_ :: SomeException) -> pure ())

errorToStatus :: ParseError -> Status
errorToStatus = \case
  ParseMessageTooLong -> RequestHeaderFieldsTooLarge
  ParseBadRequestLine -> BadRequest
  ParseBadStatusLine -> BadGateway
  ParseBadHeaderName -> BadRequest
  ParseInvalidHeaderValue -> BadRequest
  ParseLengthConflict -> BadRequest
  ParseLengthAndTransferEncoding -> BadRequest
  ParseChunkedNotFinal -> BadRequest
  ParseInvalidLength -> BadRequest
  ParseUnsupportedVersion -> HttpVersionNotSupported
  ParseBadChunkHeader -> BadRequest
  ParseChunkTooLarge -> PayloadTooLarge
  ParseUnexpectedEof -> BadRequest

------------------------------------------------------------------------
-- Keep-alive decision
------------------------------------------------------------------------

decideClose :: ServerConfig -> Request -> Response -> Bool
decideClose cfg req resp
  | not (serverKeepAlive cfg) = True
  | requestVersion req == HTTP_1_0 = not (clientWantsKeepAlive (requestHeaders req))
  | otherwise = clientWantsClose (requestHeaders req) || serverWantsClose (responseHeaders resp)

clientWantsKeepAlive :: Headers -> Bool
clientWantsKeepAlive hs = case findConnection hs of
  Nothing -> False
  Just v  -> any (== ConnKeepAlive) (parseConnection v)

clientWantsClose :: Headers -> Bool
clientWantsClose hs = case findConnection hs of
  Nothing -> False
  Just v  -> any (== ConnClose) (parseConnection v)

serverWantsClose :: Headers -> Bool
serverWantsClose hs = case findConnection hs of
  Nothing -> False
  Just v  -> any (== ConnClose) (parseConnection v)

addCloseHeader :: Response -> Response
addCloseHeader r
  | hHas "connection" (responseHeaders r) = r
  | otherwise = r { responseHeaders = responseHeaders r <> [("Connection", "close")] }
