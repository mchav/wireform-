{- | TLS-with-ALPN negotiation glue for "Network.HTTP.Client" \/
"Network.HTTP.Server".

The TLS handshake is shared between HTTP\/1.x and HTTP\/2; once it
completes the ALPN-negotiated protocol determines which runtime
drives the connection.  Both protocols ride over a thin
'Network.HTTP1.Transport.Transport' \/
'Network.HTTP2.Transport.Transport' bridge built from the live
'TLS.Context'.

If ALPN picks a protocol that isn't covered by the configured
'VersionRange' (e.g. the server only offered @http\/1.1@ but the
client requested 'http2Only'), 'VersionOutOfRange' is raised; if it
picks something we don't understand at all, 'TlsNoAlpnOverlap' is
raised.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.TLS
  ( -- * Client
    TlsClient (..)
  , withTlsClient
    -- * Server
  , runTlsServer
    -- * Errors
  , TlsHandshakeError (..)
  ) where

import Control.Concurrent (forkIO)
import Control.Exception
  (Exception, SomeException, bracket, catch, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import qualified Network.Socket as NS
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

import qualified Network.HTTP1.Connection as H1C
import qualified Network.HTTP1.Client as H1
import qualified Network.HTTP1.Transport as H1T
import qualified Network.HTTP2.Client as H2
import qualified Network.HTTP2.Server as H2
import qualified Network.HTTP2.TLS as H2TLS

import Network.HTTP.VersionRange
import qualified Network.HTTP.Types.Version as U

-- | Errors raised by the TLS adapter that aren't already
-- 'TLS.TLSException' or 'Network.HTTP2.TLS.ALPNFailed'.
data TlsHandshakeError
  = TlsNoAlpnOverlap !(Maybe ByteString) !VersionRange
    -- ^ The peer didn't pick anything from our ALPN list (the
    -- peer-selected protocol, if any, is in the first field).
  | TlsCertNotFound !FilePath
  | TlsHostnameMismatch !String !String
    -- ^ The peer's certificate didn't cover the expected SNI \/ X.509
    -- name. First field: configured server name; second: a
    -- diagnostic string from the underlying TLS library.
  deriving stock (Show)

instance Exception TlsHandshakeError

-- | A live TLS-protected client connection.  Inspect via pattern
-- match; 'Network.HTTP.Client' converts to its own 'Client'.
data TlsClient
  = TlsClientHttp2 !H2.ClientHandle
  | TlsClientHttp1 !H1.ClientConnection

------------------------------------------------------------------------
-- Client
------------------------------------------------------------------------

-- | Connect to @host:port@ over TLS, advertise the supplied
-- 'VersionRange' via ALPN, and run @action@ with the
-- per-protocol connection handle.
--
-- 'serverName' is the X.509 hostname \/ SNI value.
-- 'validateCert' toggles peer-certificate validation.
withTlsClient
  :: String        -- ^ host
  -> String        -- ^ port
  -> String        -- ^ TLS server name (SNI \/ X.509)
  -> Bool          -- ^ validate cert
  -> VersionRange
  -> (TlsClient -> IO a)
  -> IO a
withTlsClient host port serverName validateCert range action = do
  let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
      alpnList = versionAlpnProtocols range
  addrs <- NS.getAddrInfo (Just hints) (Just host) (Just port)
  case addrs of
    [] -> error "Network.HTTP.TLS.withTlsClient: no addresses for host"
    (addr:_) -> bracket
      (NS.openSocket addr)
      NS.close
      $ \sock -> do
        NS.connect sock (NS.addrAddress addr)
        NS.setSocketOption sock NS.NoDelay 1
        ctx <- TLS.contextNew sock (buildClientParams serverName validateCert alpnList)
        TLS.handshake ctx
        negotiated <- TLS.getNegotiatedProtocol ctx
        case negotiated >>= versionForAlpn of
          Nothing -> throwIO (TlsNoAlpnOverlap negotiated range)
          Just v
            | not (versionAllowed v range) ->
                throwIO (VersionOutOfRange (Just v) range)
            | v == U.HTTP2 -> do
                transport <- H2TLS.tlsTransport ctx
                let h2cfg = H2.defaultClientConfig
                      { H2.clientHost = host
                      , H2.clientPort = port
                      }
                H2.withConnectionOnTransport h2cfg transport (Just sock)
                  (action . TlsClientHttp2)
            | otherwise -> do
                transport <- tlsTransportH1 ctx
                bracket
                  (H1C.newConnectionFromTransport transport)
                  H1C.closeConnection
                  (\conn -> action (TlsClientHttp1 (H1.ClientConnection conn)))

buildClientParams
  :: String -> Bool -> [ByteString] -> TLS.ClientParams
buildClientParams serverName validateCert alpnList = base
  where
    base0 = TLS.defaultParamsClient serverName (BS8.pack "")
    base = base0
      { TLS.clientShared = (TLS.clientShared base0)
          { TLS.sharedValidationCache =
              if validateCert
                then TLS.sharedValidationCache (TLS.clientShared base0)
                else TLS.ValidationCache
                       (\_ _ _ -> pure TLS.ValidationCachePass)
                       (\_ _ _ -> pure ())
          }
      , TLS.clientSupported = (TLS.clientSupported base0)
          { TLS.supportedCiphers = TLS.ciphersuite_default
          }
      , TLS.clientHooks = (TLS.clientHooks base0)
          { TLS.onSuggestALPN = pure (Just alpnList)
          }
      , TLS.clientUseServerNameIndication = True
      }

------------------------------------------------------------------------
-- Server
------------------------------------------------------------------------

-- | Bind a TLS-protected listener.  ALPN advertises the protocols in
-- the supplied 'VersionRange'; the server picks the
-- highest-preference protocol the client also supports.
-- Connections that don't negotiate an in-range protocol are dropped.
--
-- Two protocol-specific handlers cover the dispatch.  Either can
-- ignore the negotiated 'U.Version' if it doesn't care about which
-- dialect the peer ended up picking — but the value is supplied so
-- log messages and handlers can branch.
runTlsServer
  :: String          -- ^ host
  -> String          -- ^ port
  -> FilePath        -- ^ certificate chain (PEM)
  -> FilePath        -- ^ private key (PEM)
  -> VersionRange
  -> (U.Version -> H1T.Transport -> IO ())
     -- ^ HTTP\/1.x dispatch (called on ALPN @http\/1.0@ or @http\/1.1@).
  -> (U.Version -> H2.ServerConfig)
     -- ^ HTTP\/2 'ServerConfig' factory (called on ALPN @h2@); the
     -- resulting server runs on the live TLS 'Transport'.
  -> IO ()
runTlsServer host port certPath keyPath range http1Handler http2CfgFor = do
  cred <- TLS.credentialLoadX509 certPath keyPath >>= \case
    Left _ -> throwIO (TlsCertNotFound certPath)
    Right c -> pure c
  let hints = NS.defaultHints
        { NS.addrFlags = [NS.AI_PASSIVE]
        , NS.addrSocketType = NS.Stream
        }
  addrs <- NS.getAddrInfo (Just hints) (Just host) (Just port)
  case addrs of
    [] -> error "Network.HTTP.TLS.runTlsServer: no bind address"
    (addr:_) -> bracket (NS.openSocket addr) NS.close $ \listenSock -> do
      NS.setSocketOption listenSock NS.ReuseAddr 1
      NS.setSocketOption listenSock NS.NoDelay 1
      NS.bind listenSock (NS.addrAddress addr)
      NS.listen listenSock 128
      acceptLoop listenSock cred
  where
    alpnList = versionAlpnProtocols range
    acceptLoop listenSock cred = do
      (clientSock, _) <- NS.accept listenSock
      NS.setSocketOption clientSock NS.NoDelay 1
      _ <- forkIO $ handleConn cred clientSock
        `catch` (\(_ :: SomeException) -> NS.close clientSock)
      acceptLoop listenSock cred

    handleConn cred sock = do
      ctx <- TLS.contextNew sock (buildServerParams cred alpnList)
      TLS.handshake ctx
      negotiated <- TLS.getNegotiatedProtocol ctx
      case negotiated >>= versionForAlpn of
        Nothing -> closeCleanly ctx sock
        Just v
          | not (versionAllowed v range) -> closeCleanly ctx sock
          | v == U.HTTP2 -> do
              transport <- H2TLS.tlsTransport ctx
              H2.runServerOnTransport (http2CfgFor v) transport
          | otherwise -> do
              transport <- tlsTransportH1 ctx
              http1Handler v transport

    closeCleanly ctx sock = do
      (TLS.bye ctx) `catch` (\(_ :: SomeException) -> pure ())
      (TLS.contextClose ctx) `catch` (\(_ :: SomeException) -> pure ())
      NS.close sock

buildServerParams :: TLS.Credential -> [ByteString] -> TLS.ServerParams
buildServerParams cred alpnList = base
  where
    base = TLS.defaultParamsServer
      { TLS.serverShared = (TLS.serverShared TLS.defaultParamsServer)
          { TLS.sharedCredentials = TLS.Credentials [cred]
          }
      , TLS.serverSupported = (TLS.serverSupported TLS.defaultParamsServer)
          { TLS.supportedCiphers = TLS.ciphersuite_default
          }
      , TLS.serverHooks = (TLS.serverHooks TLS.defaultParamsServer)
          { TLS.onALPNClientSuggest = Just (selectAlpn alpnList)
          }
      }

-- | Pick the highest-preference protocol from our allowlist that the
-- client also advertised.  Returns the empty 'ByteString' (\"no
-- selection\") when there is no overlap.
selectAlpn :: [ByteString] -> [ByteString] -> IO ByteString
selectAlpn allowed offered = pure $ pick allowed
  where
    pick []     = ""
    pick (a:as)
      | a `elem` offered = a
      | otherwise        = pick as

------------------------------------------------------------------------
-- HTTP/1 transport over TLS
------------------------------------------------------------------------

-- | Build an HTTP\/1 'H1T.Transport' from a live 'TLS.Context'.  Sends
-- are flattened into 'TLS.sendData'; receives use a small holdover
-- buffer to bridge tls's chunk-returning recv to the Ptr-filling
-- 'H1T.tRecvBuf'.
tlsTransportH1 :: TLS.Context -> IO H1T.Transport
tlsTransportH1 ctx = do
  leftover <- newIORef BS.empty
  pure H1T.Transport
    { H1T.tSendAll = \bs -> TLS.sendData ctx (LBS.fromStrict bs)
    , H1T.tSendMany = \bss -> TLS.sendData ctx (LBS.fromChunks bss)
    , H1T.tRecvBuf = \ptr n -> bufferedFill leftover (TLS.recvData ctx) ptr n
    , H1T.tClose = closeCtx
    , H1T.tSocket = Nothing
    }
  where
    closeCtx = do
      (TLS.bye ctx) `catch` swallow
      (TLS.contextClose ctx) `catch` swallow
    swallow :: SomeException -> IO ()
    swallow _ = pure ()

bufferedFill
  :: IORef ByteString
  -> IO ByteString
  -> Ptr Word8
  -> Int
  -> IO Int
bufferedFill leftoverRef recvChunk dst want = do
  leftover <- readIORef leftoverRef
  if not (BS.null leftover)
    then copyFromChunk leftoverRef leftover dst want
    else do
      chunk <- recvChunk
      if BS.null chunk
        then pure 0
        else copyFromChunk leftoverRef chunk dst want

copyFromChunk
  :: IORef ByteString
  -> ByteString
  -> Ptr Word8
  -> Int
  -> IO Int
copyFromChunk leftoverRef chunk dst want = do
  let take_ = min (BS.length chunk) want
      (taken, rest) = BS.splitAt take_ chunk
  writeIORef leftoverRef rest
  let (fp, off, len) = BSI.toForeignPtr taken
  withForeignPtr fp $ \src ->
    copyBytes dst (src `plusPtr` off) len
  pure len
