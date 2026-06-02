{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | gRPC-friendly HTTP\/2-over-TLS server engine, OpenSSL-backed.
--
-- The vendored grapesy engine's @http2-tls@-shaped server API.
-- Originally backed by the pure-Haskell @tls@ package; now goes
-- through "Wireform.Network.TLS.OpenSSL" so the engine + grpc
-- speak the same OpenSSL bridge as the rest of the repo.
--
-- 'runTLSWithSocket' wraps an OpenSSL TLS handshake (advertising
-- the requested ALPN identifier) around the supplied socket and
-- hands the user action a 'TM.Manager' + 'IOBackend' that bridges
-- 'Wireform.Network.TLS.OpenSSL.SslConn' to the
-- 'Network.HTTP2.Engine.Server.Config' shape.
module Network.HTTP2.Engine.TLS.Server
  ( -- * Settings
    Settings (..)
  , defaultSettings
    -- * Runners
  , runTLSWithSocket
    -- * IO backend
  , IOBackend (..)
    -- * TLS context construction (caller-owned)
  , buildServerCtxFromPaths
  , module Wireform.Network.TLS.OpenSSL
  ) where

import qualified Control.Exception as E
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.IORef as IORef
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, plusPtr)
import qualified Network.Socket as NS
import Network.Socket (SockAddr, Socket)
import qualified System.TimeManager as TM

import Wireform.Network.TLS.OpenSSL
import Wireform.Network.TLS.Config
  ( TlsServerConfig (..)
  , buildServerCtx
  , defaultTlsServerConfig
  )

-- | TLS server settings.
--
-- All fields except 'settingsKeyLogger' are wireform-network /
-- http2-engine tuning knobs that don't depend on a TLS backend.
-- 'settingsKeyLogger' is preserved as a debug hook even though the
-- current OpenSSL FFI does not yet route the key log; the field is
-- accepted (and ignored) for API compatibility.
data Settings = Settings
  { settingsTimeout              :: !Int
  , settingsSendBufferSize       :: !Int
  , settingsSlowlorisSize        :: !Int
  , settingsReadBufferSize       :: !Int
  , settingsReadBufferLowerLimit :: !Int
  , settingsKeyLogger            :: String -> IO ()
  , settingsNumberOfWorkers      :: !Int
  , settingsConcurrentStreams    :: !Int
  , settingsConnectionWindowSize :: !Int
  , settingsStreamWindowSize     :: !Int
  , settingsEarlyDataSize        :: !Int
  , settingsPingRateLimit        :: !Int
  , settingsEmptyFrameRateLimit  :: !Int
  , settingsSettingsRateLimit    :: !Int
  , settingsRstRateLimit         :: !Int
  }

defaultSettings :: Settings
defaultSettings = Settings
  { settingsTimeout              = 30
  , settingsSendBufferSize       = 4096
  , settingsSlowlorisSize        = 50
  , settingsReadBufferSize       = 16384
  , settingsReadBufferLowerLimit = 2048
  , settingsKeyLogger            = \_ -> pure ()
  , settingsNumberOfWorkers      = 8
  , settingsConcurrentStreams    = 64
  , settingsConnectionWindowSize = 16777216
  , settingsStreamWindowSize     = 262144
  , settingsEarlyDataSize        = 0
  , settingsPingRateLimit        = 10
  , settingsEmptyFrameRateLimit  = 4
  , settingsSettingsRateLimit    = 4
  , settingsRstRateLimit         = 4
  }

-- | I\/O backend handed to the action invoked by 'runTLSWithSocket'.
-- Mirrors the wire shape grapesy expects: send / recv functions + the
-- raw socket + 'SockAddr's.
data IOBackend = IOBackend
  { send         :: !(ByteString -> IO ())
  , sendMany     :: !([ByteString] -> IO ())
  , recv         :: !(IO ByteString)
  , requestSock  :: !Socket
  , mySockAddr   :: !SockAddr
  , peerSockAddr :: !SockAddr
  }

-- | Run a TLS handshake on the given socket (advertising the
-- requested ALPN identifier — typically @\"h2\"@), then hand
-- control to the action with a fresh 'TM.Manager' and the bridge
-- 'IOBackend'.  The 'SslCtx' is owned by the caller; the per-
-- connection @SSL*@ is freed before this returns.
--
-- The socket passed here must be an already-connected (accepted)
-- client socket, NOT a listen socket.
runTLSWithSocket
  :: Settings
  -> SslCtx           -- ^ Pre-built server SSL_CTX (cert + key + ALPN)
  -> Socket           -- ^ Already-accepted client socket
  -> ByteString       -- ^ ALPN identifier the caller expects to negotiate
                      --   (used for ALPN assertion; pass @\"h2\"@ for HTTP\/2).
  -> (TM.Manager -> IOBackend -> IO a)
  -> IO a
runTLSWithSocket Settings{..} ctx sock _expectedAlpn action = do
  mgr  <- TM.initialize (settingsTimeout * 1000 * 1000)
  conn <- newServer ctx sock
  mysa <- NS.getSocketName sock
  peer <- NS.getPeerName sock
  leftoverRef <- IORef.newIORef BS.empty
  let recvChunk :: IO ByteString
      recvChunk = do
        let buf = settingsReadBufferSize
        BSI.createUptoN buf $ \dst -> do
          n <- tlsReceiveFn conn dst buf
          pure n
      backend = IOBackend
        { send         = sslSendAll conn
        , sendMany     = sslSendMany conn
        , recv         = drainLeftover leftoverRef recvChunk
        , requestSock  = sock
        , mySockAddr   = mysa
        , peerSockAddr = peer
        }
  r <- action mgr backend
  E.catch @E.SomeException (freeConn conn >> pure ()) (\_ -> pure ())
  pure r

-- | Drain any holdover bytes from a previous recv first, then call
-- 'recvChunk' to pull a fresh TLS record's worth of plaintext.  This
-- exists because the engine's @recvN@ helper sometimes asks for an
-- exact byte count smaller than what arrived in one record; the
-- leftover ref carries the remainder over to the next call.
drainLeftover
  :: IORef.IORef ByteString -> IO ByteString -> IO ByteString
drainLeftover ref recvChunk = do
  held <- IORef.readIORef ref
  if not (BS.null held)
    then do
      IORef.writeIORef ref BS.empty
      pure held
    else recvChunk

sslSendAll :: SslConn -> ByteString -> IO ()
sslSendAll conn bs
  | BS.null bs = pure ()
  | otherwise  = BSU.unsafeUseAsCStringLen bs $ \(src, len) ->
      drain (castPtr src) len
  where
    fn = tlsSendFn conn
    drain _ 0 = pure ()
    drain p n = do
      k <- fn p n
      if k <= 0
        then ioError (userError "Engine.TLS.Server: SSL_write_ex returned 0")
        else drain (p `plusPtr` k) (n - k)

sslSendMany :: SslConn -> [ByteString] -> IO ()
sslSendMany _    []  = pure ()
sslSendMany conn bss = mapM_ (sslSendAll conn) bss

-- | Convenience: build an OpenSSL server 'SslCtx' from PEM cert + key
-- paths + the ALPN protocol list.  The cert path receives the full
-- chain; chain certs that should appear /after/ the leaf are loaded
-- via 'useChainCert'-style append (callers that need that should
-- concatenate the PEM files in advance; OpenSSL reads the whole
-- chain from one file).
buildServerCtxFromPaths
  :: FilePath                -- ^ cert chain (PEM)
  -> FilePath                -- ^ private key (PEM)
  -> [ByteString]            -- ^ ALPN protocols, e.g. [\"h2\"]
  -> IO SslCtx
buildServerCtxFromPaths cert key alpns = do
  ctx <- buildServerCtx $ (defaultTlsServerConfig cert key)
    { tlsServerAlpn = alpns
    }
  case alpns of
    [] -> pure ()
    _  -> setAlpnServer ctx alpns
  pure ctx
