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

    -- * Connecting
  , withWebSocketClient
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

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

newtype WebSocketClientError = WebSocketClientError String
  deriving stock (Show)

instance Exception WebSocketClientError

------------------------------------------------------------------------
-- Connect
------------------------------------------------------------------------

-- | Connect, complete the handshake, hand the live 'Connection' to
-- @action@.  Tears down the connection (polite close + ring
-- release) on exit.
withWebSocketClient
  :: WebSocketClientConfig
  -> (Connection -> IO a)
  -> IO a
withWebSocketClient cfg action = do
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

connectPlain
  :: WebSocketClientConfig
  -> NS.Socket
  -> (Connection -> IO a)
  -> IO a
connectPlain cfg sock action = do
  (_key, leftover) <- doHandshake cfg
                                  (NSB.sendAll sock)
                                  (NSB.recv sock 4096)
  duplex <- prebufferedDuplex (transportCfg cfg) sock leftover
  bracket
    (newConnection Client defaultPayloadLimit duplex)
    politeClose
    action

connectTls
  :: WebSocketClientConfig
  -> WebSocketClientTls
  -> NS.Socket
  -> (Connection -> IO a)
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
        (_key, leftover) <- doHandshake cfg
                                        (TLS.tlsSend ssl)
                                        (recvTlsChunk ssl)
        duplex <- prebufferedDuplexTls (transportCfg cfg) sock ssl leftover
        bracket
          (newConnection Client defaultPayloadLimit duplex)
          politeClose
          action

politeClose :: Connection -> IO ()
politeClose conn = do
  _ <- try @SomeException (sendClose conn)
  closeConnection conn

------------------------------------------------------------------------
-- Handshake driver (raw bytes)
------------------------------------------------------------------------

-- | Roll a request, send it, drain the response head, validate the
-- 101 reply, and return both the @Sec-WebSocket-Key@ we used and
-- any bytes already received past the @\\r\\n\\r\\n@ terminator
-- (which become the first frame bytes).
doHandshake
  :: WebSocketClientConfig
  -> (ByteString -> IO ())                -- ^ send raw bytes
  -> IO ByteString                         -- ^ recv one chunk
  -> IO (ByteString, ByteString)           -- ^ (key, leftover)
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
      Right () -> pure (key, leftover)

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
