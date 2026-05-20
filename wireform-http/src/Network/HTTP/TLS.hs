{- | TLS-with-ALPN negotiation glue for "Network.HTTP.Client" /
"Network.HTTP.Server".

Most of the heavy lifting (building 'TLS.ClientParams' /
'TLS.ServerParams', bridging a 'TLS.Context' to wireform-http2's
'Transport') already lives in @wireform-http2@'s
"Network.HTTP2.TLS"; this module is the small adapter that drives
ALPN from a 'VersionRange' instead of hardcoding @h2@.

We currently only carry an implementation of HTTP\/2 over TLS — the
HTTP\/1.x stack doesn't have a TLS adapter yet, so a range that picks
the @http\/1.1@ ALPN protocol will raise 'TlsHttp1NotImplemented'.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.TLS
  ( -- * Client
    withTlsClient
    -- * Server
  , runTlsServer
    -- * Errors
  , TlsHandshakeError (..)
  ) where

import Control.Concurrent (forkIO)
import Control.Exception
  (Exception, SomeException, bracket, catch, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Network.Socket as NS
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS

import qualified Network.HTTP2.Client as H2
import qualified Network.HTTP2.Server as H2
import qualified Network.HTTP2.TLS as H2TLS

import Network.HTTP.VersionRange
import qualified Network.HTTP.Types.Version as U

-- | Errors raised by the TLS adapter that aren't already 'TLS.TLSException'
-- or 'Network.HTTP2.TLS.ALPNFailed'.
data TlsHandshakeError
  = TlsHttp1NotImplemented
    -- ^ ALPN negotiated @http\/1.1@ but @wireform-http1@ has no TLS
    -- adapter yet.  Set 'clientVersionRange' to 'http2Only' or
    -- 'http2OrHttp11' with 'preferHttp2' to make sure h2 is picked.
  | TlsNoAlpnOverlap !(Maybe ByteString) !VersionRange
    -- ^ The server didn't pick anything from our ALPN list (the
    -- server-selected protocol, if any, is in the first field).
  | TlsCertNotFound !FilePath
  deriving stock (Show)

instance Exception TlsHandshakeError

------------------------------------------------------------------------
-- Client
------------------------------------------------------------------------

-- | Connect to @host:port@ over TLS, advertise the supplied
-- 'VersionRange' via ALPN, and run @action@ over the resulting
-- 'H2.ClientHandle' (HTTP\/2 only for now).
--
-- 'serverName' is the X.509 hostname \/ SNI value.
-- 'validateCert' toggles peer-certificate validation.
withTlsClient
  :: String        -- ^ host
  -> String        -- ^ port
  -> String        -- ^ TLS server name (SNI \/ X.509)
  -> Bool          -- ^ validate cert
  -> VersionRange
  -> (H2.ClientHandle -> IO a)
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
          Just v
            | not (versionAllowed v range) ->
                throwIO (VersionOutOfRange (Just v) range)
            | v == U.HTTP2 -> do
                transport <- H2TLS.tlsTransport ctx
                let h2cfg = H2.defaultClientConfig
                      { H2.clientHost = host
                      , H2.clientPort = port
                      }
                H2.withConnectionOnTransport h2cfg transport (Just sock) action
            | otherwise ->
                throwIO TlsHttp1NotImplemented
          Nothing -> throwIO (TlsNoAlpnOverlap negotiated range)

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

-- | Bind a TLS-protected HTTP listener.  ALPN is advertised from the
-- supplied 'VersionRange'; the server picks the highest-preference
-- protocol the client also supports.  Connections that don't
-- negotiate an in-range protocol are dropped.
--
-- The handler is invoked with the negotiated 'U.Version' and the
-- HTTP\/2 'H2.ServerConfig' for that connection (since HTTP\/1.x over
-- TLS isn't wired up yet, the version will always be 'U.HTTP2' for
-- accepted connections — but we still pass it so the handler doesn't
-- have to assume).
runTlsServer
  :: String          -- ^ host
  -> String          -- ^ port
  -> FilePath        -- ^ certificate chain (PEM)
  -> FilePath        -- ^ private key (PEM)
  -> VersionRange
  -> (U.Version -> H2.ServerConfig -> H2.ServerConfig)
     -- ^ Per-connection 'ServerConfig' adjustment; receives the
     -- negotiated version so the application handler can branch on it.
  -> H2.ServerConfig
     -- ^ Base server config (host / port are overwritten).
  -> IO ()
runTlsServer host port certPath keyPath range mkCfg baseCfg = do
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
        Just v
          | not (versionAllowed v range) -> closeCleanly ctx sock
          | v == U.HTTP2 -> do
              transport <- H2TLS.tlsTransport ctx
              H2.runServerOnTransport (mkCfg v baseCfg) transport
          | otherwise -> closeCleanly ctx sock
        Nothing -> closeCleanly ctx sock

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
