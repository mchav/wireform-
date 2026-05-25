{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | WebSocket server.

Two integration shapes:

* __Standalone listener__ \u2014 'runWebSocketServer' /
  'runWebSocketServerOnListener' bind a socket and drive the
  full accept loop themselves.  TLS is supported via the same
  'Wireform.Network.TLS.OpenSSL' machinery the wireform-http
  server uses, so configuring @wss:\/\/@ is a one-liner.
* __Hand off from wireform-http__ \u2014 the unified
  'Network.HTTP.Server' lets a 'Handler' inspect a 'Request'
  and decide how to respond.  Use 'isWebSocketRequest' inside the
  handler to recognise the upgrade attempt and then pass the
  hijacked socket to 'acceptWebSocketOnSocket' \/
  'acceptWebSocketOnTls' from your own accept loop.

The handler signature is 'WebSocketHandler', which receives a
'Connection' plus the validated request and is free to use any of
"Network.WebSocket.Message" \/ "Network.WebSocket.Connection".
-}
module Network.WebSocket.Server
  ( -- * Configuration
    WebSocketServerConfig (..)
  , defaultWebSocketServerConfig
  , WebSocketTlsConfig (..)
  , WebSocketServerLimits (..)
  , defaultWebSocketServerLimits

    -- * Handler
  , WebSocketHandler

    -- * Standalone listener
  , runWebSocketServer
  , runWebSocketServerOnListener

    -- * Hand-off from an accepted socket
  , acceptWebSocketOnSocket
  , acceptWebSocketOnTls
  ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Exception
  (SomeException, bracket, catch, finally, mask, try)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.CaseInsensitive as CI
import Data.IORef
import Data.Word (Word8)
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import qualified Network.HTTP.Types.Method  as U
import qualified Network.HTTP.Types.Status  as S
import qualified Network.HTTP.Types.Version as V
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))

import qualified Network.HTTP1.Encode as H1E
import qualified Network.HTTP1.Method as H1M
import qualified Network.HTTP1.Parser as H1P
import qualified Network.HTTP1.Types  as H1

import qualified Wireform.Network as N
import qualified Wireform.Network.TLS.Config as TLSCfg
import qualified Wireform.Network.TLS.OpenSSL as TLS
import qualified Wireform.Transport.Config as WC

import Network.WebSocket.Connection
import Network.WebSocket.Frame (defaultPayloadLimit)
import Network.WebSocket.Handshake

------------------------------------------------------------------------
-- Config
------------------------------------------------------------------------

-- | A 'WebSocketHandler' receives the validated handshake metadata
-- and a live 'Connection' to drive.  When the handler returns
-- normally, the server sends a polite close frame and tears down
-- the connection.  Exceptions are caught and reported through
-- 'wscOnException'.
type WebSocketHandler = WebSocketRequest -> Connection -> IO ()

data WebSocketServerConfig = WebSocketServerConfig
  { wscHost             :: !String
  , wscPort             :: !String
  , wscHandler          :: !WebSocketHandler
  , wscTls              :: !(Maybe WebSocketTlsConfig)
  , wscLimits           :: !WebSocketServerLimits
  , wscOnException      :: !(WebSocketRequest -> SomeException -> IO ())
  , wscForkConnection   :: !(IO () -> IO ThreadId)
  , wscOnHandshakeError :: !(HandshakeError -> IO ())
    -- ^ Logging callback when the client's request fails the RFC
    -- 6455 sec 4 checks.  Default is a no-op.
  , wscSelectSubProtocol :: !(WebSocketRequest -> Maybe ByteString)
    -- ^ Choose a sub-protocol from those the client offered, or
    -- 'Nothing' to decline.  RFC 6455 sec 4.2.2 says the server
    -- may select 0 or 1 of the client's offered protocols.  The
    -- default declines (returns 'Nothing'), which works fine for
    -- the typical chat-app shape where one app == one protocol.
    --
    -- This is the server-side counterpart of the client's
    -- 'Network.WebSocket.Client.wcSubProtocols' field; the
    -- client validates that whatever you return here is in the
    -- list it offered.
  , wscSingleThreaded    :: !Bool
    -- ^ When 'True' (default), the handler is the only thread
    -- that ever touches its 'Connection' and the per-direction
    -- 'MVar' locks are skipped (~1.4 \u00b5s per round-trip
    -- saved).  Set 'False' if a single connection is shared
    -- across multiple threads (broadcast \/ fan-out shapes).
  , wscRingSizeHint     :: !Int
    -- ^ Magic-ring buffer size for both directions.  Default 256 KiB,
    -- which leaves enough headroom for the WebSocket handshake
    -- block plus several control \/ data frames.
  }

data WebSocketTlsConfig = WebSocketTlsConfig
  { wstCertPath :: !FilePath
  , wstKeyPath  :: !FilePath
  , wstAlpn     :: ![ByteString]
    -- ^ ALPN protocols to advertise.  Empty list disables ALPN.
    --   For @wss:\/\/@ this typically isn't needed (browsers
    --   don't gate on ALPN); set @[\"http\/1.1\"]@ when sharing
    --   a port with an HTTP\/1.1 server.
  }

data WebSocketServerLimits = WebSocketServerLimits
  { wslMaxHeaderBytes  :: !Int
  , wslMaxMessageBytes :: !Int
  }

defaultWebSocketServerLimits :: WebSocketServerLimits
defaultWebSocketServerLimits = WebSocketServerLimits
  { wslMaxHeaderBytes  = 32 * 1024
  , wslMaxMessageBytes = 32 * 1024 * 1024
  }

defaultWebSocketServerConfig :: WebSocketServerConfig
defaultWebSocketServerConfig = WebSocketServerConfig
  { wscHost              = "0.0.0.0"
  , wscPort              = "8080"
  , wscHandler           = \_ _ -> pure ()
  , wscTls               = Nothing
  , wscLimits            = defaultWebSocketServerLimits
  , wscOnException       = \_ _ -> pure ()
  , wscForkConnection    = forkIO
  , wscOnHandshakeError  = \_ -> pure ()
  , wscSelectSubProtocol = \_ -> Nothing
  , wscSingleThreaded    = True
  , wscRingSizeHint      = 256 * 1024
  }

------------------------------------------------------------------------
-- Standalone listener
------------------------------------------------------------------------

-- | Bind a listening socket and serve WebSocket connections.
runWebSocketServer :: WebSocketServerConfig -> IO ()
runWebSocketServer cfg = do
  let hints = NS.defaultHints
        { NS.addrFlags      = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just (wscHost cfg)) (Just (wscPort cfg))
  case addrs of
    []        -> error "Network.WebSocket.Server.runWebSocketServer: no bind address"
    (addr:_)  -> bracket (NS.openSocket addr) NS.close $ \listenSock -> do
      NS.setSocketOption listenSock NS.ReuseAddr 1
      NS.bind listenSock (NS.addrAddress addr)
      NS.listen listenSock 128
      runWebSocketServerOnListener cfg listenSock

-- | Variant for callers that have already bound a listening socket
-- (e.g. systemd socket activation or ephemeral-port tests via
-- @bind 0@).
runWebSocketServerOnListener :: WebSocketServerConfig -> NS.Socket -> IO ()
runWebSocketServerOnListener cfg listenSock = do
  mCtx <- case wscTls cfg of
    Nothing -> pure Nothing
    Just t  -> Just <$> buildTlsCtx t
  acceptLoop mCtx `finally` maybe (pure ()) TLS.freeCtx mCtx
  where
    acceptLoop mCtx = do
      (clientSock, _) <- NS.accept listenSock
      NS.setSocketOption clientSock NS.NoDelay 1
      _ <- wscForkConnection cfg $
        (case mCtx of
           Nothing  -> acceptWebSocketOnSocket cfg clientSock
           Just ctx -> acceptViaTls cfg ctx clientSock)
        `catch` (\(_ :: SomeException) -> NS.close clientSock)
      acceptLoop mCtx

buildTlsCtx :: WebSocketTlsConfig -> IO TLS.SslCtx
buildTlsCtx t = do
  let cfg = (TLSCfg.defaultTlsServerConfig (wstCertPath t) (wstKeyPath t))
        { TLSCfg.tlsServerAlpn = wstAlpn t }
  ctx <- TLSCfg.buildServerCtx cfg
  case wstAlpn t of
    [] -> pure ()
    xs -> TLS.setAlpnServer ctx xs
  pure ctx

acceptViaTls :: WebSocketServerConfig -> TLS.SslCtx -> NS.Socket -> IO ()
acceptViaTls cfg ctx sock = do
  ssl <- TLS.newServer ctx sock
  acceptWebSocketOnTls cfg ssl `finally` TLS.freeConn ssl

------------------------------------------------------------------------
-- Hand-off: plain socket
------------------------------------------------------------------------

-- | Drive the WebSocket handshake + handler loop on an already
-- accepted plain-TCP 'NS.Socket'.  Reads the HTTP handshake from
-- the socket, validates it, sends the 101 response, and invokes
-- the handler.  Closes the socket on return.
acceptWebSocketOnSocket :: WebSocketServerConfig -> NS.Socket -> IO ()
acceptWebSocketOnSocket cfg sock = act `finally` closeIgnore (NS.close sock)
  where
    act = do
      let lim = wslMaxHeaderBytes (wscLimits cfg)
      block <- readHttpHead sock lim
      case parseHandshakeBytes block of
        Left e -> do
          wscOnHandshakeError cfg e
          writeBadRequest sock e
        Right (req, leftover) -> case parseWebSocketRequest req of
          Left e -> do
            wscOnHandshakeError cfg e
            writeBadRequest sock e
          Right wsreq -> do
            let resp = serverAccept wsreq (wscSelectSubProtocol cfg wsreq)
            NSB.sendAll sock (renderResponseHead resp)
            runHandlerOverDuplex cfg wsreq sock Nothing leftover

-- | TLS variant: handshake has already happened, we own the
-- 'TLS.SslConn'.  Reads the HTTP handshake through TLS, validates,
-- replies, and runs the handler over the encrypted duplex
-- transport.  The 'TLS.SslConn' is /not/ freed here \u2014 caller
-- bracket-frees it.
acceptWebSocketOnTls :: WebSocketServerConfig -> TLS.SslConn -> IO ()
acceptWebSocketOnTls cfg ssl = do
  let lim = wslMaxHeaderBytes (wscLimits cfg)
  block <- readHttpHeadTls ssl lim
  case parseHandshakeBytes block of
    Left e -> do
      wscOnHandshakeError cfg e
      _ <- try @SomeException (TLS.tlsSend ssl (renderBadRequest e))
      pure ()
    Right (req, leftover) -> case parseWebSocketRequest req of
      Left e -> do
        wscOnHandshakeError cfg e
        _ <- try @SomeException (TLS.tlsSend ssl (renderBadRequest e))
        pure ()
      Right wsreq -> do
        let resp = serverAccept wsreq (wscSelectSubProtocol cfg wsreq)
        TLS.tlsSend ssl (renderResponseHead resp)
        runHandlerOverDuplex cfg wsreq (TLS.sslConnSocket ssl)
          (Just ssl) leftover

------------------------------------------------------------------------
-- Handler driver
------------------------------------------------------------------------

runHandlerOverDuplex
  :: WebSocketServerConfig
  -> WebSocketRequest
  -> NS.Socket
  -> Maybe TLS.SslConn
  -> ByteString          -- ^ leftover bytes pulled past the header
                         --   block; must be the first thing the
                         --   parser sees.
  -> IO ()
runHandlerOverDuplex cfg req sock mSsl leftover = do
  let tcfg = N.defaultTransportConfig { WC.ringSizeHint = wscRingSizeHint cfg }
  duplex <- case mSsl of
    Nothing  -> prebufferedDuplex tcfg sock leftover
    Just ssl -> prebufferedDuplexTls tcfg sock ssl leftover
  conn <- if wscSingleThreaded cfg
            then newConnectionUnlocked Server defaultPayloadLimit duplex
            else newConnection         Server defaultPayloadLimit duplex
  invokeHandler cfg req conn

invokeHandler :: WebSocketServerConfig -> WebSocketRequest -> Connection -> IO ()
invokeHandler cfg req conn = mask $ \restore -> do
  r <- try $ restore (wscHandler cfg req conn)
  case r of
    Right ()           -> politeClose
    Left (e :: SomeException) -> do
      wscOnException cfg req e
      politeClose
  where
    politeClose = do
      _ <- try @SomeException (sendClose conn)
      closeConnection conn

------------------------------------------------------------------------
-- Duplex with leftover pre-buffer
------------------------------------------------------------------------

prebufferedDuplex
  :: WC.TransportConfig
  -> NS.Socket
  -> ByteString
  -> IO N.DuplexTransport
prebufferedDuplex cfg sock leftover = do
  ref <- newIORef leftover
  N.newDuplexBufTransport cfg
    (prefixedRecv ref (NS.recvBuf sock))
    (\p n -> NS.sendBuf sock p n)
    (NS.shutdown sock NS.ShutdownSend)

prebufferedDuplexTls
  :: WC.TransportConfig
  -> NS.Socket
  -> TLS.SslConn
  -> ByteString
  -> IO N.DuplexTransport
prebufferedDuplexTls cfg sock ssl leftover = do
  ref <- newIORef leftover
  N.newDuplexBufTransport cfg
    (prefixedRecv ref (TLS.tlsReceiveFn ssl))
    (TLS.tlsSendFn ssl)
    (NS.shutdown sock NS.ShutdownSend)

prefixedRecv :: IORef ByteString -> N.ReceiveFn -> N.ReceiveFn
prefixedRecv ref fallback = recv
  where
    recv :: Ptr Word8 -> Int -> IO Int
    recv dst want = do
      buf <- readIORef ref
      if BS.null buf
        then fallback dst want
        else do
          let !taken = min (BS.length buf) want
              !slice = BS.take taken buf
              !rest  = BS.drop taken buf
          writeIORef ref rest
          BSU.unsafeUseAsCStringLen slice $ \(src, len) ->
            copyBytes dst (castPtr src) len
          pure taken

------------------------------------------------------------------------
-- HTTP header read helpers
------------------------------------------------------------------------

readHttpHead :: NS.Socket -> Int -> IO ByteString
readHttpHead sock cap = go BS.empty
  where
    go acc
      | "\r\n\r\n" `BS.isInfixOf` acc = pure acc
      | BS.length acc >= cap          = pure acc
      | otherwise = do
          chunk <- NSB.recv sock 4096
          if BS.null chunk
            then pure acc
            else go (acc <> chunk)

-- | TLS variant of 'readHttpHead'.  Pulls plaintext through
-- 'TLS.tlsReceiveFn' until the header terminator appears.
readHttpHeadTls :: TLS.SslConn -> Int -> IO ByteString
readHttpHeadTls ssl cap = loop BS.empty
  where
    loop acc
      | "\r\n\r\n" `BS.isInfixOf` acc = pure acc
      | BS.length acc >= cap          = pure acc
      | otherwise = do
          (n, bs) <- recvChunk 4096
          if n == 0
            then pure acc
            else loop (acc <> bs)
    recvChunk n = do
      fp <- mallocForeignPtrBytes n
      withForeignPtr fp $ \p -> do
        got <- TLS.tlsReceiveFn ssl p n
        if got <= 0
          then pure (0, BS.empty)
          else do
            -- Copy into a fresh 'ByteString' so the buffer can
            -- be reused next iteration; this is the slow path
            -- (only the handshake walks it), so the copy is fine.
            bs <- BS.packCStringLen (castPtr p, got)
            pure (got, bs)

------------------------------------------------------------------------
-- HTTP\/1.1 parsing / rendering
--
-- Delegates to wireform-http1's SIMD-backed parser
-- ('Network.HTTP1.Parser.parseRequest') and the chunked-builder-based
-- encoder ('Network.HTTP1.Encode.encodeResponseHead'); the websocket
-- layer just adapts between the unified 'Request' \/ 'Response'
-- shapes that 'Network.WebSocket.Handshake' speaks and the H1
-- shapes that the parser \/ encoder consume.
------------------------------------------------------------------------

parseHandshakeBytes
  :: ByteString
  -> Either HandshakeError (Request, ByteString)
parseHandshakeBytes block =
  let (headBlock, leftover) = splitHttpHeaderBlock block
  in case H1P.parseRequest headBlock of
    Left  e               -> Left (httpParseToHandshakeError e)
    Right (h1req, _frame) -> Right (h1ReqToUnified h1req, leftover)

-- | Split the on-the-wire header block from any leftover bytes
-- received past its terminator.  The leftover bytes are the first
-- frame the client sent right after the handshake.
splitHttpHeaderBlock :: ByteString -> (ByteString, ByteString)
splitHttpHeaderBlock bs = case BS.breakSubstring "\r\n\r\n" bs of
  (h, t) | BS.null t -> (h, BS.empty)
         | otherwise -> (h, BS.drop 4 t)

-- | Convert wireform-http1's H1.ParseError into a HandshakeError
-- that the websocket layer can present to the caller.  Lossy on
-- purpose: H1's error vocabulary is more granular than the
-- handshake's surface needs.
httpParseToHandshakeError :: H1P.ParseError -> HandshakeError
httpParseToHandshakeError e = HandshakeBadMethod (BS8.pack (show e))

h1ReqToUnified :: H1.Request -> Request
h1ReqToUnified r =
  let !hdrs = map (\(n, v) -> (CI.mk n, v)) (H1.requestHeaders r)
  in Request
       { requestMethod    = U.methodFromBytes (H1M.methodToBytes (H1.requestMethod r))
       , requestTarget    = H1.requestTarget r
       , requestAuthority = lookup (CI.mk "Host") hdrs
       , requestScheme    = SchemeHttp
       , requestHeaders   = hdrs
       , requestBody      = BodyEmpty
       , requestVersion   = case H1.requestVersion r of
           H1.HTTP_1_0 -> V.HTTP1_0
           H1.HTTP_1_1 -> V.HTTP1_1
       , requestTrailers  = pure []
       }

------------------------------------------------------------------------
-- Response writers
------------------------------------------------------------------------

renderResponseHead :: Response -> ByteString
renderResponseHead = H1E.encodeResponseHead . unifiedToH1Response

unifiedToH1Response :: Response -> H1.Response
unifiedToH1Response r = H1.Response
  { H1.responseStatus   = H1.Status (S.statusCode (responseStatus r))
  , H1.responseVersion  = case responseVersion r of
      V.HTTP1_0 -> H1.HTTP_1_0
      _         -> H1.HTTP_1_1
  , H1.responseHeaders  = map (\(n, v) -> (CI.original n, v)) (responseHeaders r)
  , H1.responseBody     = H1.BodyEmpty
  , H1.responseTrailers = pure []
  }

writeBadRequest :: NS.Socket -> HandshakeError -> IO ()
writeBadRequest sock e =
  NSB.sendAll sock (renderBadRequest e)
    `catch` (\(_ :: SomeException) -> pure ())

renderBadRequest :: HandshakeError -> ByteString
renderBadRequest e = H1E.encodeResponseHead H1.Response
  { H1.responseStatus   = H1.Status 400
  , H1.responseVersion  = H1.HTTP_1_1
  , H1.responseHeaders  =
      [ ("Connection",     "close")
      , ("Content-Type",   "text/plain; charset=utf-8")
      , ("Content-Length", BS8.pack (show (BS.length body)))
      ]
  , H1.responseBody     = H1.BodyBytes body
  , H1.responseTrailers = pure []
  } <> body
  where
    body = BS8.pack (show e)

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

closeIgnore :: IO () -> IO ()
closeIgnore action = action `catch` (\(_ :: SomeException) -> pure ())
