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

import qualified Network.HTTP.Types.Header  as H
import qualified Network.HTTP.Types.Method  as M
import qualified Network.HTTP.Types.Status  as S
import qualified Network.HTTP.Types.Version as V
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Message (Request (..), Response (..), Scheme (..))

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
    -- 6455 \u00a74 checks.  Default is a no-op.
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
  { wscHost             = "0.0.0.0"
  , wscPort             = "8080"
  , wscHandler          = \_ _ -> pure ()
  , wscTls              = Nothing
  , wscLimits           = defaultWebSocketServerLimits
  , wscOnException      = \_ _ -> pure ()
  , wscForkConnection   = forkIO
  , wscOnHandshakeError = \_ -> pure ()
  , wscRingSizeHint     = 256 * 1024
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
            let resp = serverAccept wsreq Nothing
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
        let resp = serverAccept wsreq Nothing
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
  conn <- newConnection Server defaultPayloadLimit duplex
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
-- Tiny HTTP/1.1 request parser \u2014 just enough to drive the
-- WebSocket handshake.
------------------------------------------------------------------------

parseHandshakeBytes
  :: ByteString
  -> Either HandshakeError (Request, ByteString)
parseHandshakeBytes block = do
  let (head_, leftover) = splitHeaderBlock block
      ls = splitOn "\r\n" head_
  case ls of
    []        -> Left (HandshakeBadMethod "")
    (l0:rest) -> do
      (method, target, _ver) <- parseRequestLine l0
      hdrs <- traverse parseHeaderLine
                (filter (not . BS.null) rest)
      let authority = lookup H.hHost hdrs
      pure ( Request
              { requestMethod    = M.Method method
              , requestTarget    = target
              , requestAuthority = authority
              , requestScheme    = SchemeHttp
              , requestHeaders   = hdrs
              , requestBody      = BodyEmpty
              , requestVersion   = V.HTTP1_1
              , requestTrailers  = pure []
              }
           , leftover
           )

splitHeaderBlock :: ByteString -> (ByteString, ByteString)
splitHeaderBlock bs = case BS.breakSubstring "\r\n\r\n" bs of
  (h, t) | BS.null t -> (h, BS.empty)
         | otherwise -> (h, BS.drop 4 t)

splitOn :: ByteString -> ByteString -> [ByteString]
splitOn sep =
  let !slen = BS.length sep
      go acc bs = case BS.breakSubstring sep bs of
        (h, t) | BS.null t -> reverse (h : acc)
               | otherwise -> go (h : acc) (BS.drop slen t)
  in go []

parseRequestLine
  :: ByteString
  -> Either HandshakeError (ByteString, ByteString, ByteString)
parseRequestLine l = case BS.split 0x20 l of
  [m, t, v] -> Right (m, t, v)
  _         -> Left (HandshakeBadMethod l)

parseHeaderLine :: ByteString -> Either HandshakeError H.Header
parseHeaderLine bs = case BS.break (== 0x3A {- ':' -}) bs of
  (n, rest) | BS.null rest -> Left (HandshakeMissingHeader (CI.mk n))
            | otherwise    ->
                let v0 = BS.drop 1 rest
                    v  = BS.dropWhile (\b -> b == 0x20 || b == 0x09) v0
                    v' = stripTrailingWs v
                in Right (CI.mk n, v')
  where
    stripTrailingWs s =
      let n = BS.length s
          go i | i <= 0    = BS.empty
               | otherwise = case BS.index s (i - 1) of
                   b | b == 0x20 || b == 0x09 || b == 0x0d -> go (i - 1)
                     | otherwise -> BS.take i s
      in go n

------------------------------------------------------------------------
-- Response writers
------------------------------------------------------------------------

renderResponseHead :: Response -> ByteString
renderResponseHead r =
  let status = responseStatus r
      hdrs   = responseHeaders r
  in BS.concat $
        [ "HTTP/1.1 "
        , BS8.pack (show (S.statusCode status))
        , " "
        , S.statusReason status
        , "\r\n"
        ]
        <> concatMap renderHeader hdrs
        <> ["\r\n"]
  where
    renderHeader (n, v) = [CI.original n, ": ", v, "\r\n"]

writeBadRequest :: NS.Socket -> HandshakeError -> IO ()
writeBadRequest sock e =
  NSB.sendAll sock (renderBadRequest e)
    `catch` (\(_ :: SomeException) -> pure ())

renderBadRequest :: HandshakeError -> ByteString
renderBadRequest e = BS.concat
  [ "HTTP/1.1 400 Bad Request\r\n"
  , "Connection: close\r\n"
  , "Content-Type: text/plain; charset=utf-8\r\n"
  , "Content-Length: ", BS8.pack (show (BS.length body)), "\r\n"
  , "\r\n"
  , body
  ]
  where
    body = BS8.pack (show e)

------------------------------------------------------------------------
-- Misc
------------------------------------------------------------------------

closeIgnore :: IO () -> IO ()
closeIgnore action = action `catch` (\(_ :: SomeException) -> pure ())
