{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | WebSocket client.

Connects to a @ws:\/\/@ or @wss:\/\/@ endpoint, completes the
RFC 6455 \u00a74 handshake, and runs a caller-supplied callback
with a live 'Connection'.  Both plain TCP and TLS (via
"Wireform.Network.TLS.OpenSSL", same backend the wireform-http
client uses) are supported.

@
withWebSocketClient cfg $ \\conn -> do
  sendTextMessage conn \"hello\"
  TextMessage reply <- receiveMessage conn defaultMessageLimit
  print reply
@
-}
module Network.WebSocket.Client (
  -- * Client config
  WebSocketClientConfig (..),
  defaultWebSocketClientConfig,
  WebSocketClientTls (..),
  wsTlsDefault,
  clientConfigFromURI,

  -- * Connecting (bracketed)
  withWebSocketClient,
  withWebSocketClient',
  withWebSocketClientURI,

  -- * Connecting (imperative)
  openWebSocketClient,
  openWebSocketClient',
  openWebSocketClientURI,
  closeWebSocketClient,

  -- * Handshake result
  ServerHandshakeResult (..),

  -- * Errors
  WebSocketClientError (..),
) where

import Control.Exception (
  Exception,
  SomeException,
  bracket,
  onException,
  throwIO,
  try,
 )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Unsafe qualified as BSU
import Data.CaseInsensitive qualified as CI
import Data.IORef
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import Network.HTTP.Types.Header qualified as H
import Network.HTTP1.Method qualified as H1M
import Network.HTTP1.Parser qualified as H1P
import Network.HTTP1.Types qualified as H1
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB
import Network.WebSocket.Connection
import Network.WebSocket.Frame (defaultPayloadLimit)
import Network.WebSocket.Handshake (
  buildClientHandshake,
  defaultWebSocketHandshakeOpts,
  verifyServerHandshake,
  wsOptExtensions,
  wsOptExtraHeaders,
  wsOptProtocols,
 )
import Network.WebSocket.Handshake qualified as HS
import Network.WebSocket.PerMessageDeflate qualified as PMD
import Network.WebSocket.URI
import Wireform.Network qualified as N
import Wireform.Network.TLS.Config qualified as TLSCfg
import Wireform.Network.TLS.OpenSSL qualified as TLS
import Wireform.Network.Transport.Duplex qualified as N (DuplexTransport)
import Wireform.Transport.Config qualified as WC


------------------------------------------------------------------------
-- Config
------------------------------------------------------------------------

data WebSocketClientConfig = WebSocketClientConfig
  { wcHost :: !String
  , wcPort :: !String
  , wcTarget :: !ByteString
  -- ^ Request target, e.g. @"/chat?room=42"@.
  , wcAuthority :: !ByteString
  {- ^ Value for the @Host@ header.  Usually @wcHost <> ":" <> wcPort@
  but can differ for SNI \/ reverse-proxy setups.
  -}
  , wcSubProtocols :: ![ByteString]
  , wcExtensions :: ![ByteString]
  , wcExtraHeaders :: ![H.Header]
  , wcPermessageDeflate :: !(Maybe PMD.PmdOffer)
  {- ^ When 'Just', the client offers @permessage-deflate@
  (RFC 7692) in the handshake.  If the server returns the
  extension in its 101 reply we install a 'PmdContext' on the
  resulting 'Connection' before handing it to the caller, and
  'shrExtensions' will contain the negotiated parameters.
  'Nothing' (the default) sends no @Sec-WebSocket-Extensions@
  header.  Coexists with 'wcExtensions' — any verbatim
  strings there are concatenated with the PMD offer.
  -}
  , wcTls :: !(Maybe WebSocketClientTls)
  , wcRingSizeHint :: !Int
  , wcSingleThreaded :: !Bool
  {- ^ Whether the resulting 'Connection' will only ever be used
  by one thread at a time.  Default 'True'.  See the matching
  'Network.WebSocket.Server.wscSingleThreaded' for the
  semantics; the cost saved on the round-trip hot path is
  ~1.4 \u00b5s.
  -}
  }


data WebSocketClientTls = WebSocketClientTls
  { wctVerifyPeer :: !Bool
  {- ^ Verify the server's certificate against the system trust
  store.  Defaults to 'True'; flip to 'False' for self-signed
  test certs (or use 'wctCaBundle' instead).
  -}
  , wctCaBundle :: !(Maybe FilePath)
  -- ^ Additional CA bundle (layered on top of the system store).
  , wctServerName :: !(Maybe ByteString)
  {- ^ SNI \/ verify-hostname.  Defaults to 'wcHost' on the
  enclosing config.
  -}
  , wctAlpn :: ![ByteString]
  }


wsTlsDefault :: WebSocketClientTls
wsTlsDefault =
  WebSocketClientTls
    { wctVerifyPeer = True
    , wctCaBundle = Nothing
    , wctServerName = Nothing
    , wctAlpn = []
    }


defaultWebSocketClientConfig
  :: String
  -- ^ host
  -> String
  -- ^ port
  -> ByteString
  -- ^ target
  -> WebSocketClientConfig
defaultWebSocketClientConfig h p t =
  WebSocketClientConfig
    { wcHost = h
    , wcPort = p
    , wcTarget = t
    , wcAuthority = BS8.pack (h <> ":" <> p)
    , wcSubProtocols = []
    , wcExtensions = []
    , wcExtraHeaders = []
    , wcPermessageDeflate = Nothing
    , wcTls = Nothing
    , wcRingSizeHint = 256 * 1024
    , wcSingleThreaded = True
    }


{- | Build a 'WebSocketClientConfig' from a parsed
'Network.WebSocket.URI.WebSocketURI'.  TLS is selected
automatically for @wss:@ URIs; the SNI hostname defaults to the
URI's host.  Callers that need a custom CA bundle, mTLS, etc.
can post-process the returned record's 'wcTls' field.
-}
clientConfigFromURI :: WebSocketURI -> WebSocketClientConfig
clientConfigFromURI u =
  let host = BS8.unpack (wsuHost u)
      port = show (wsuPort u)
      tls = case wsuScheme u of
        WsScheme -> Nothing
        WssScheme ->
          Just
            wsTlsDefault
              { wctServerName = Just (wsuHost u)
              }
  in WebSocketClientConfig
       { wcHost = host
       , wcPort = port
       , wcTarget = wsuTarget u
       , wcAuthority = canonicalAuthority u
       , wcSubProtocols = []
       , wcExtensions = []
       , wcExtraHeaders = []
       , wcPermessageDeflate = Nothing
       , wcTls = tls
       , wcRingSizeHint = 256 * 1024
       , wcSingleThreaded = True
       }
  where
    canonicalAuthority WebSocketURI {wsuScheme = s, wsuHost = h, wsuPort = p}
      | p == case s of WsScheme -> 80; WssScheme -> 443 = h
      | otherwise = h <> ":" <> BS8.pack (show p)


------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

newtype WebSocketClientError = WebSocketClientError String
  deriving stock (Show)


instance Exception WebSocketClientError


{- | What the server returned alongside the 101 reply.  Lets the
caller see which sub-protocol the server selected (if any) and
inspect any extension negotiation, cookies, or custom auth
headers the server set on the response.
-}
data ServerHandshakeResult = ServerHandshakeResult
  { shrSelectedProtocol :: !(Maybe ByteString)
  {- ^ @Sec-WebSocket-Protocol@ from the response.  Must be one
  of the values the client advertised in 'wcSubProtocols';
  if the server returned something else, the handshake is
  rejected with a 'WebSocketClientError' before this record
  is constructed.
  -}
  , shrExtensions :: ![ByteString]
  {- ^ @Sec-WebSocket-Extensions@ values from the response, in
  order.  No extension is interpreted by this layer; callers
  that wired permessage-deflate (etc.) into 'wcExtensions'
  inspect this field to confirm what the server agreed to.
  -}
  , shrHeaders :: ![H.Header]
  {- ^ Full response header block.  Useful for cookies, custom
  auth, @Server@ identification.
  -}
  }
  deriving stock (Show)


------------------------------------------------------------------------
-- Connect: imperative
------------------------------------------------------------------------

{- | Open a 'Connection' to the WebSocket server described by @cfg@.
The connection is owned by the caller and must be released with
'closeWebSocketClient' (or 'withWebSocketClient', which does that
in a bracket).  Discards the server's handshake reply; use
'openWebSocketClient'' to keep it.
-}
openWebSocketClient :: WebSocketClientConfig -> IO Connection
openWebSocketClient cfg = snd <$> openWebSocketClient' cfg


{- | Variant of 'openWebSocketClient' that also returns the server's
'ServerHandshakeResult' (selected sub-protocol, agreed extensions,
full response header block).
-}
openWebSocketClient'
  :: WebSocketClientConfig
  -> IO (ServerHandshakeResult, Connection)
openWebSocketClient' cfg = do
  let hints = NS.defaultHints {NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just (wcHost cfg)) (Just (wcPort cfg))
  case addrs of
    [] -> throwIO (WebSocketClientError "no addresses for host")
    (addr : _) -> do
      sock <- NS.openSocket addr
      flip onException (NS.close sock) $ do
        NS.connect sock (NS.addrAddress addr)
        NS.setSocketOption sock NS.NoDelay 1
        case wcTls cfg of
          Nothing -> openPlain cfg sock
          Just tls -> openTls cfg tls sock


{- | URI-driven variant.  Parses @ws:\/\/@ \/ @wss:\/\/@ and opens
the connection with default settings.
-}
openWebSocketClientURI :: ByteString -> IO Connection
openWebSocketClientURI uri = case parseWebSocketURI uri of
  Left e -> throwIO (WebSocketClientError ("bad URI: " <> show e))
  Right u -> openWebSocketClient (clientConfigFromURI u)


{- | Polite RFC 6455 close: send a 1000 close frame (best effort,
ignored if the peer already went away), then release the
connection's magic ring + underlying socket / TLS state.
Idempotent — calling twice is safe.
-}
closeWebSocketClient :: Connection -> IO ()
closeWebSocketClient = politeClose


------------------------------------------------------------------------
-- Connect: bracketed
------------------------------------------------------------------------

{- | Connect, complete the handshake, hand the live 'Connection' to
@action@.  Tears down the connection (polite close + ring
release) on exit.  Equivalent to 'bracket' over
'openWebSocketClient' + 'closeWebSocketClient'.
-}
withWebSocketClient
  :: WebSocketClientConfig
  -> (Connection -> IO a)
  -> IO a
withWebSocketClient cfg action =
  bracket (openWebSocketClient cfg) closeWebSocketClient action


{- | Variant of 'withWebSocketClient' that exposes the server's
handshake reply.  Use this when:

  * the client offered multiple sub-protocols in
    'wcSubProtocols' and needs to know which one the server
    selected,
  * the server sets cookies or other headers on the 101 reply
    that the application needs to read,
  * the application negotiated extensions through
    'wcExtensions' and needs to confirm the server-side choice.
-}
withWebSocketClient'
  :: WebSocketClientConfig
  -> (ServerHandshakeResult -> Connection -> IO a)
  -> IO a
withWebSocketClient' cfg action =
  bracket
    (openWebSocketClient' cfg)
    (\(_, conn) -> closeWebSocketClient conn)
    (\(shr, conn) -> action shr conn)


{- | Convenience: parse @"ws:\/\/..."@ or @"wss:\/\/..."@ and
connect with default settings.  Mirrors the server-side
'Network.WebSocket.Server.runWebSocketServer' /
'WebSocketServerConfig' split for the client.
-}
withWebSocketClientURI
  :: ByteString
  -> (Connection -> IO a)
  -> IO a
withWebSocketClientURI uri action =
  bracket
    (openWebSocketClientURI uri)
    closeWebSocketClient
    action


------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------

openPlain
  :: WebSocketClientConfig
  -> NS.Socket
  -> IO (ServerHandshakeResult, Connection)
openPlain cfg sock = do
  (shr, mPmd, leftover) <-
    doHandshake
      cfg
      (NSB.sendAll sock)
      (NSB.recv sock 4096)
  duplex <- prebufferedDuplex (transportCfg cfg) sock leftover
  conn <- mkClientConnection cfg duplex
  attachPmdIfNegotiated conn mPmd
  -- The duplex's close doesn't tear down the raw socket — its
  -- contract is to release the magic ring and half-close the
  -- write side.  Attach an explicit socket close so the imperative
  -- open\/close pair fully releases everything.
  attachCleanup conn (NS.close sock)
  pure (shr, conn)


openTls
  :: WebSocketClientConfig
  -> WebSocketClientTls
  -> NS.Socket
  -> IO (ServerHandshakeResult, Connection)
openTls cfg tlsCfg sock = do
  let tcsf =
        TLSCfg.defaultTlsClientConfig
          { TLSCfg.tlsClientVerifyPeer = wctVerifyPeer tlsCfg
          , TLSCfg.tlsClientCaBundle = wctCaBundle tlsCfg
          , TLSCfg.tlsClientAlpn = wctAlpn tlsCfg
          }
      serverName = case wctServerName tlsCfg of
        Just s -> Just s
        Nothing -> Just (BS8.pack (wcHost cfg))
  ctx <- TLSCfg.buildClientCtx tcsf
  -- Hand off ownership of @ctx@ to the TLS connection.  We free
  -- both ssl and ctx together in 'closeWebSocketClient' via the
  -- connection's cleanup; if the handshake throws before then,
  -- the bracket-style 'onException' covers it.
  flip onException (TLS.freeCtx ctx) $ do
    ssl <- TLS.newClient ctx sock serverName
    flip onException (TLS.freeConn ssl >> TLS.freeCtx ctx) $ do
      case wctServerName tlsCfg of
        Just s
          | wctVerifyPeer tlsCfg ->
              TLS.setClientHostnameVerify ssl s
        _ -> pure ()
      (shr, mPmd, leftover) <-
        doHandshake
          cfg
          (TLS.tlsSend ssl)
          (recvTlsChunk ssl)
      duplex <- prebufferedDuplexTls (transportCfg cfg) sock ssl leftover
      conn <- mkClientConnection cfg duplex
      attachPmdIfNegotiated conn mPmd
      -- The TLS context, the SSL connection, and the raw socket
      -- all outlive the duplex itself; attach a cleanup chain so
      -- 'closeWebSocketClient' tears them down in the right order
      -- (SSL_shutdown + SSL_free first, then SSL_CTX_free, then
      -- the socket).
      attachCleanup conn $ do
        TLS.freeConn ssl
        TLS.freeCtx ctx
        NS.close sock
      pure (shr, conn)


attachPmdIfNegotiated :: Connection -> Maybe PMD.PmdParams -> IO ()
attachPmdIfNegotiated _ Nothing = pure ()
attachPmdIfNegotiated conn (Just p) = do
  ctx <- PMD.newPmdContext Client p
  attachPmd conn ctx


politeClose :: Connection -> IO ()
politeClose conn = do
  _ <- try @SomeException (sendClose conn)
  closeConnection conn


mkClientConnection
  :: WebSocketClientConfig
  -> N.DuplexTransport
  -> IO Connection
mkClientConnection cfg duplex
  | wcSingleThreaded cfg = newConnectionUnlocked Client defaultPayloadLimit duplex
  | otherwise = newConnection Client defaultPayloadLimit duplex


------------------------------------------------------------------------
-- Handshake driver (raw bytes)
------------------------------------------------------------------------

{- | Roll a request, send it, drain the response head, validate the
101 reply, and return the parsed 'ServerHandshakeResult'
alongside any bytes already received past the @\\r\\n\\r\\n@
terminator (which become the first frame bytes).

Verifies that any 'wcSubProtocols' selection the server made
was actually one we offered; rejects with a
'WebSocketClientError' otherwise.
-}
doHandshake
  :: WebSocketClientConfig
  -> (ByteString -> IO ())
  -- ^ send raw bytes
  -> IO ByteString
  -- ^ recv one chunk
  -> IO (ServerHandshakeResult, Maybe PMD.PmdParams, ByteString)
  -- ^ (result, negotiated PMD, leftover)
doHandshake cfg send recv = do
  let pmdExt = case wcPermessageDeflate cfg of
        Nothing -> []
        Just off -> [PMD.offerHeader off]
      opts =
        (defaultWebSocketHandshakeOpts (wcTarget cfg) (wcAuthority cfg))
          { wsOptProtocols = wcSubProtocols cfg
          , wsOptExtensions = wcExtensions cfg <> pmdExt
          , wsOptExtraHeaders = wcExtraHeaders cfg
          }
  (reqBytes, key) <- buildClientHandshake opts
  send reqBytes
  block <- drainHttpHead recv BS.empty 64000
  let (head_, leftover) = splitHeaderBlock block
  case parseStatusAndHeaders head_ of
    Left e -> throwIO (WebSocketClientError ("malformed 101 response: " <> e))
    Right (code, hdrs) -> case verifyServerHandshake key code hdrs of
      Left e -> throwIO (WebSocketClientError ("handshake rejected: " <> show e))
      Right () -> do
        shr <- validateNegotiation cfg hdrs
        mPmd <- validatePmdResponse cfg (shrExtensions shr)
        pure (shr, mPmd, leftover)


{- | When the client offered @permessage-deflate@, look for the
server's reply.  If the server included @permessage-deflate@ in
its response, parse the negotiated parameters; otherwise return
'Nothing' (server declined, no extension is active).  Rejects
if the client did /not/ offer PMD but the server returned it.
-}
validatePmdResponse
  :: WebSocketClientConfig
  -> [ByteString]
  -> IO (Maybe PMD.PmdParams)
validatePmdResponse cfg exts = do
  let pmdRespHeaders = filter isPmdExt exts
  case (wcPermessageDeflate cfg, pmdRespHeaders) of
    (Nothing, []) -> pure Nothing
    (Nothing, _ : _) ->
      throwIO
        ( WebSocketClientError
            "server responded with permessage-deflate but client did not offer it"
        )
    (Just _, []) -> pure Nothing
    (Just _, hdr : _) -> case PMD.parseResponseParams hdr of
      Just p -> pure (Just p)
      Nothing ->
        throwIO
          ( WebSocketClientError
              ("malformed permessage-deflate response parameters: " <> show hdr)
          )
  where
    isPmdExt v = case BS.split 0x3B v of
      (name : _) -> stripOws name == "permessage-deflate"
      _ -> False
    stripOws = BS.dropWhile isWs . trimEnd
    isWs b = b == 0x20 || b == 0x09
    trimEnd s =
      let !n = BS.length s
          go i
            | i <= 0 = BS.empty
            | isWs (BS.index s (i - 1)) = go (i - 1)
            | otherwise = BS.take i s
      in go n


{- | Pull the negotiated sub-protocol and extension list out of
the server's reply, validating that the sub-protocol is one
the client actually offered.
-}
validateNegotiation
  :: WebSocketClientConfig
  -> [H.Header]
  -> IO ServerHandshakeResult
validateNegotiation cfg hdrs = do
  let selProto = H.lookupHeader (CI.mk "Sec-WebSocket-Protocol") hdrs
      exts =
        HS.splitTokenList
          (H.lookupHeaders (CI.mk "Sec-WebSocket-Extensions") hdrs)
  case selProto of
    Just p
      | not (any (== p) (wcSubProtocols cfg)) ->
          throwIO
            ( WebSocketClientError
                ("server selected an unoffered sub-protocol: " <> show p)
            )
    _ -> pure ()
  pure
    ServerHandshakeResult
      { shrSelectedProtocol = selProto
      , shrExtensions = exts
      , shrHeaders = hdrs
      }


drainHttpHead :: IO ByteString -> ByteString -> Int -> IO ByteString
drainHttpHead recv acc cap
  | "\r\n\r\n" `BS.isInfixOf` acc = pure acc
  | BS.length acc >= cap = pure acc
  | otherwise = do
      chunk <- recv
      if BS.null chunk
        then pure acc
        else drainHttpHead recv (acc <> chunk) cap


splitHeaderBlock :: ByteString -> (ByteString, ByteString)
splitHeaderBlock bs = case BS.breakSubstring "\r\n\r\n" bs of
  (h, t)
    | BS.null t -> (h, BS.empty)
    | otherwise -> (h, BS.drop 4 t)


{- | Parse the 101 response head via wireform-http1's
'Network.HTTP1.Parser.parseResponse'.  The handshake is always
a GET request, so we pass 'H1M.mGet' for the framing
computation; the body is always empty on 101 anyway so framing
only matters to keep parseResponse from refusing.
-}
parseStatusAndHeaders
  :: ByteString
  -> Either String (Int, [H.Header])
parseStatusAndHeaders block = case H1P.parseResponse H1M.GET block of
  Left e -> Left (show e)
  Right (resp, _framing) ->
    Right
      ( fromIntegral (H1.statusCode (H1.responseStatus resp))
      , map (\(n, v) -> (CI.mk n, v)) (H1.responseHeaders resp)
      )


------------------------------------------------------------------------
-- Raw TLS recv helper
------------------------------------------------------------------------

recvTlsChunk :: TLS.SslConn -> IO ByteString
recvTlsChunk ssl = do
  let n = 4096
  fp <- mallocForeignPtrBytes n
  withForeignPtr fp $ \p -> do
    got <- TLS.tlsReceiveFn ssl p n
    if got <= 0
      then pure BS.empty
      else BS.packCStringLen (castPtr p, got)


------------------------------------------------------------------------
-- Duplex with leftover pre-buffer
------------------------------------------------------------------------

transportCfg :: WebSocketClientConfig -> WC.TransportConfig
transportCfg cfg =
  N.defaultTransportConfig
    { WC.ringSizeHint = wcRingSizeHint cfg
    }


prebufferedDuplex
  :: WC.TransportConfig
  -> NS.Socket
  -> ByteString
  -> IO N.DuplexTransport
prebufferedDuplex cfg sock leftover = do
  ref <- newIORef leftover
  N.newDuplexBufTransport
    cfg
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
  N.newDuplexBufTransport
    cfg
    (prefixedRecv ref (TLS.tlsReceiveFn ssl))
    (TLS.tlsSendFn ssl)
    (NS.shutdown sock NS.ShutdownSend)


prefixedRecv :: IORef ByteString -> N.ReceiveFn -> N.ReceiveFn
prefixedRecv ref fallback = recv
  where
    recv dst want = do
      buf <- readIORef ref
      if BS.null buf
        then fallback dst want
        else do
          let !taken = min (BS.length buf) want
              !slice = BS.take taken buf
              !rest = BS.drop taken buf
          writeIORef ref rest
          BSU.unsafeUseAsCStringLen slice $ \(src, len) ->
            copyBytes dst (castPtr src) len
          pure taken
