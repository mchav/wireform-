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
module Network.WebSocket.Client
  ( -- * Client config
    WebSocketClientConfig (..)
  , defaultWebSocketClientConfig
  , WebSocketClientTls (..)
  , wsTlsDefault
  , clientConfigFromURI

    -- * Connecting
  , withWebSocketClient
  , withWebSocketClient'
  , withWebSocketClientURI

    -- * Handshake result
  , ServerHandshakeResult (..)

    -- * Errors
  , WebSocketClientError (..)
  ) where

import Control.Exception
  (Exception, SomeException, bracket, throwIO, try)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.CaseInsensitive as CI
import Data.IORef
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import qualified Network.HTTP.Types.Header as H

import qualified Network.WebSocket.Handshake as HS
import Network.WebSocket.Handshake
  (buildClientHandshake, defaultWebSocketHandshakeOpts, verifyServerHandshake,
   wsOptExtensions, wsOptExtraHeaders, wsOptProtocols)
import Network.WebSocket.URI

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

data WebSocketClientConfig = WebSocketClientConfig
  { wcHost            :: !String
  , wcPort            :: !String
  , wcTarget          :: !ByteString
    -- ^ Request target, e.g. @"/chat?room=42"@.
  , wcAuthority       :: !ByteString
    -- ^ Value for the @Host@ header.  Usually @wcHost <> ":" <> wcPort@
    --   but can differ for SNI \/ reverse-proxy setups.
  , wcSubProtocols    :: ![ByteString]
  , wcExtensions      :: ![ByteString]
  , wcExtraHeaders    :: ![H.Header]
  , wcTls             :: !(Maybe WebSocketClientTls)
  , wcRingSizeHint    :: !Int
  }

data WebSocketClientTls = WebSocketClientTls
  { wctVerifyPeer    :: !Bool
    -- ^ Verify the server's certificate against the system trust
    -- store.  Defaults to 'True'; flip to 'False' for self-signed
    -- test certs (or use 'wctCaBundle' instead).
  , wctCaBundle      :: !(Maybe FilePath)
    -- ^ Additional CA bundle (layered on top of the system store).
  , wctServerName    :: !(Maybe ByteString)
    -- ^ SNI \/ verify-hostname.  Defaults to 'wcHost' on the
    --   enclosing config.
  , wctAlpn          :: ![ByteString]
  }

wsTlsDefault :: WebSocketClientTls
wsTlsDefault = WebSocketClientTls
  { wctVerifyPeer = True
  , wctCaBundle   = Nothing
  , wctServerName = Nothing
  , wctAlpn       = []
  }

defaultWebSocketClientConfig
  :: String       -- ^ host
  -> String       -- ^ port
  -> ByteString   -- ^ target
  -> WebSocketClientConfig
defaultWebSocketClientConfig h p t = WebSocketClientConfig
  { wcHost           = h
  , wcPort           = p
  , wcTarget         = t
  , wcAuthority      = BS8.pack (h <> ":" <> p)
  , wcSubProtocols   = []
  , wcExtensions     = []
  , wcExtraHeaders   = []
  , wcTls            = Nothing
  , wcRingSizeHint   = 256 * 1024
  }

-- | Build a 'WebSocketClientConfig' from a parsed
-- 'Network.WebSocket.URI.WebSocketURI'.  TLS is selected
-- automatically for @wss:@ URIs; the SNI hostname defaults to the
-- URI's host.  Callers that need a custom CA bundle, mTLS, etc.
-- can post-process the returned record's 'wcTls' field.
clientConfigFromURI :: WebSocketURI -> WebSocketClientConfig
clientConfigFromURI u =
  let host = BS8.unpack (wsuHost u)
      port = show (wsuPort u)
      tls = case wsuScheme u of
        WsScheme  -> Nothing
        WssScheme -> Just wsTlsDefault
          { wctServerName = Just (wsuHost u) }
  in WebSocketClientConfig
       { wcHost           = host
       , wcPort           = port
       , wcTarget         = wsuTarget u
       , wcAuthority      = canonicalAuthority u
       , wcSubProtocols   = []
       , wcExtensions     = []
       , wcExtraHeaders   = []
       , wcTls            = tls
       , wcRingSizeHint   = 256 * 1024
       }
  where
    canonicalAuthority WebSocketURI{ wsuScheme = s, wsuHost = h, wsuPort = p }
      | p == case s of WsScheme -> 80 ; WssScheme -> 443 = h
      | otherwise = h <> ":" <> BS8.pack (show p)

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

newtype WebSocketClientError = WebSocketClientError String
  deriving stock (Show)

instance Exception WebSocketClientError

-- | What the server returned alongside the 101 reply.  Lets the
-- caller see which sub-protocol the server selected (if any) and
-- inspect any extension negotiation, cookies, or custom auth
-- headers the server set on the response.
data ServerHandshakeResult = ServerHandshakeResult
  { shrSelectedProtocol :: !(Maybe ByteString)
    -- ^ @Sec-WebSocket-Protocol@ from the response.  Must be one
    -- of the values the client advertised in 'wcSubProtocols';
    -- if the server returned something else, the handshake is
    -- rejected with a 'WebSocketClientError' before this record
    -- is constructed.
  , shrExtensions       :: ![ByteString]
    -- ^ @Sec-WebSocket-Extensions@ values from the response, in
    -- order.  No extension is interpreted by this layer; callers
    -- that wired permessage-deflate (etc.) into 'wcExtensions'
    -- inspect this field to confirm what the server agreed to.
  , shrHeaders          :: ![H.Header]
    -- ^ Full response header block.  Useful for cookies, custom
    -- auth, @Server@ identification.
  } deriving stock (Show)

------------------------------------------------------------------------
-- Connect
------------------------------------------------------------------------

-- | Connect, complete the handshake, hand the live 'Connection' to
-- @action@.  Tears down the connection (polite close + ring
-- release) on exit.
--
-- Discards the server's 'ServerHandshakeResult'.  Use
-- 'withWebSocketClient'' if you need the server-selected
-- sub-protocol or the response headers.
withWebSocketClient
  :: WebSocketClientConfig
  -> (Connection -> IO a)
  -> IO a
withWebSocketClient cfg action =
  withWebSocketClient' cfg (\_ conn -> action conn)

-- | Variant of 'withWebSocketClient' that exposes the server's
-- handshake reply.  Use this when:
--
--   * the client offered multiple sub-protocols in
--     'wcSubProtocols' and needs to know which one the server
--     selected,
--   * the server sets cookies or other headers on the 101 reply
--     that the application needs to read,
--   * the application negotiated extensions through
--     'wcExtensions' and needs to confirm the server-side choice.
withWebSocketClient'
  :: WebSocketClientConfig
  -> (ServerHandshakeResult -> Connection -> IO a)
  -> IO a
withWebSocketClient' cfg action = do
  let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
  addrs <- NS.getAddrInfo (Just hints) (Just (wcHost cfg)) (Just (wcPort cfg))
  case addrs of
    []        -> throwIO (WebSocketClientError "no addresses for host")
    (addr:_)  -> bracket (NS.openSocket addr) NS.close $ \sock -> do
      NS.connect sock (NS.addrAddress addr)
      NS.setSocketOption sock NS.NoDelay 1
      case wcTls cfg of
        Nothing  -> connectPlain cfg sock action
        Just tls -> connectTls   cfg tls sock action

-- | Convenience: parse @"ws:\/\/..."@ or @"wss:\/\/..."@ and
-- connect with default settings.  Mirrors the server-side
-- 'Network.WebSocket.Server.runWebSocketServer' /
-- 'WebSocketServerConfig' split for the client.
withWebSocketClientURI
  :: ByteString
  -> (Connection -> IO a)
  -> IO a
withWebSocketClientURI uri action =
  case parseWebSocketURI uri of
    Left e   -> throwIO (WebSocketClientError ("bad URI: " <> show e))
    Right u  -> withWebSocketClient (clientConfigFromURI u) action

connectPlain
  :: WebSocketClientConfig
  -> NS.Socket
  -> (ServerHandshakeResult -> Connection -> IO a)
  -> IO a
connectPlain cfg sock action = do
  (shr, leftover) <- doHandshake cfg
                                 (NSB.sendAll sock)
                                 (NSB.recv sock 4096)
  duplex <- prebufferedDuplex (transportCfg cfg) sock leftover
  bracket
    (newConnection Client defaultPayloadLimit duplex)
    politeClose
    (action shr)

connectTls
  :: WebSocketClientConfig
  -> WebSocketClientTls
  -> NS.Socket
  -> (ServerHandshakeResult -> Connection -> IO a)
  -> IO a
connectTls cfg tlsCfg sock action = do
  let alpn = wctAlpn tlsCfg
      tcsf = TLSCfg.defaultTlsClientConfig
        { TLSCfg.tlsClientVerifyPeer = wctVerifyPeer tlsCfg
        , TLSCfg.tlsClientCaBundle   = wctCaBundle tlsCfg
        , TLSCfg.tlsClientAlpn       = alpn
        }
  bracket (TLSCfg.buildClientCtx tcsf) TLS.freeCtx $ \ctx -> do
    let serverName = case wctServerName tlsCfg of
          Just s  -> Just s
          Nothing -> Just (BS8.pack (wcHost cfg))
    bracket
      (TLS.newClient ctx sock serverName)
      TLS.freeConn
      $ \ssl -> do
        case wctServerName tlsCfg of
          Just s | wctVerifyPeer tlsCfg ->
            TLS.setClientHostnameVerify ssl s
          _ -> pure ()
        (shr, leftover) <- doHandshake cfg
                                       (TLS.tlsSend ssl)
                                       (recvTlsChunk ssl)
        duplex <- prebufferedDuplexTls (transportCfg cfg) sock ssl leftover
        bracket
          (newConnection Client defaultPayloadLimit duplex)
          politeClose
          (action shr)

politeClose :: Connection -> IO ()
politeClose conn = do
  _ <- try @SomeException (sendClose conn)
  closeConnection conn

------------------------------------------------------------------------
-- Handshake driver (raw bytes)
------------------------------------------------------------------------

-- | Roll a request, send it, drain the response head, validate the
-- 101 reply, and return the parsed 'ServerHandshakeResult'
-- alongside any bytes already received past the @\\r\\n\\r\\n@
-- terminator (which become the first frame bytes).
--
-- Verifies that any 'wcSubProtocols' selection the server made
-- was actually one we offered; rejects with a
-- 'WebSocketClientError' otherwise.
doHandshake
  :: WebSocketClientConfig
  -> (ByteString -> IO ())                  -- ^ send raw bytes
  -> IO ByteString                           -- ^ recv one chunk
  -> IO (ServerHandshakeResult, ByteString)  -- ^ (result, leftover)
doHandshake cfg send recv = do
  let opts = (defaultWebSocketHandshakeOpts (wcTarget cfg) (wcAuthority cfg))
        { wsOptProtocols    = wcSubProtocols cfg
        , wsOptExtensions   = wcExtensions cfg
        , wsOptExtraHeaders = wcExtraHeaders cfg
        }
  (reqBytes, key) <- buildClientHandshake opts
  send reqBytes
  block <- drainHttpHead recv BS.empty 64000
  let (head_, leftover) = splitHeaderBlock block
  case parseStatusAndHeaders head_ of
    Left e  -> throwIO (WebSocketClientError ("malformed 101 response: " <> e))
    Right (code, hdrs) -> case verifyServerHandshake key code hdrs of
      Left  e -> throwIO (WebSocketClientError ("handshake rejected: " <> show e))
      Right () -> do
        shr <- validateNegotiation cfg hdrs
        pure (shr, leftover)

-- | Pull the negotiated sub-protocol and extension list out of
-- the server's reply, validating that the sub-protocol is one
-- the client actually offered.
validateNegotiation
  :: WebSocketClientConfig
  -> [H.Header]
  -> IO ServerHandshakeResult
validateNegotiation cfg hdrs = do
  let selProto = H.lookupHeader  (CI.mk "Sec-WebSocket-Protocol")   hdrs
      exts     = HS.splitTokenList
                   (H.lookupHeaders (CI.mk "Sec-WebSocket-Extensions") hdrs)
  case selProto of
    Just p | not (any (== p) (wcSubProtocols cfg)) ->
      throwIO (WebSocketClientError
        ("server selected an unoffered sub-protocol: " <> show p))
    _ -> pure ()
  pure ServerHandshakeResult
    { shrSelectedProtocol = selProto
    , shrExtensions       = exts
    , shrHeaders          = hdrs
    }

drainHttpHead :: IO ByteString -> ByteString -> Int -> IO ByteString
drainHttpHead recv acc cap
  | "\r\n\r\n" `BS.isInfixOf` acc = pure acc
  | BS.length acc >= cap          = pure acc
  | otherwise = do
      chunk <- recv
      if BS.null chunk
        then pure acc
        else drainHttpHead recv (acc <> chunk) cap

splitHeaderBlock :: ByteString -> (ByteString, ByteString)
splitHeaderBlock bs = case BS.breakSubstring "\r\n\r\n" bs of
  (h, t) | BS.null t -> (h, BS.empty)
         | otherwise -> (h, BS.drop 4 t)

parseStatusAndHeaders
  :: ByteString
  -> Either String (Int, [H.Header])
parseStatusAndHeaders block =
  let ls = splitOn "\r\n" block
  in case ls of
       []        -> Left "empty response head"
       (l0:rest) -> do
         code <- parseStatusLine l0
         hdrs <- traverse parseHeaderLine (filter (not . BS.null) rest)
         Right (code, hdrs)

parseStatusLine :: ByteString -> Either String Int
parseStatusLine l = case BS.split 0x20 l of
  (_ver : code : _) -> case BS8.readInt code of
    Just (n, leftover) | BS.null leftover -> Right n
    _ -> Left "bad status code"
  _ -> Left "bad status line"

parseHeaderLine :: ByteString -> Either String H.Header
parseHeaderLine bs = case BS.break (== 0x3A {- ':' -}) bs of
  (n, rest) | BS.null rest -> Left "missing colon"
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

splitOn :: ByteString -> ByteString -> [ByteString]
splitOn sep =
  let !slen = BS.length sep
      go acc bs = case BS.breakSubstring sep bs of
        (h, t) | BS.null t -> reverse (h : acc)
               | otherwise -> go (h : acc) (BS.drop slen t)
  in go []

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
transportCfg cfg = N.defaultTransportConfig
  { WC.ringSizeHint = wcRingSizeHint cfg }

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
