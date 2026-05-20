{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | gRPC-friendly HTTP\/2-over-TLS server engine.
--
-- The @http2-tls@-shaped server API in wireform-http2's namespace.
-- Currently exposes 'runTLSWithSocket', which wraps a TLS handshake
-- (advertising the requested ALPN identifier) around the supplied
-- socket and hands the user action a 'TM.Manager' + 'IOBackend'.
module Network.HTTP2.Engine.TLS.Server
  ( -- * Settings
    Settings (..)
  , defaultSettings
    -- * Runners
  , runTLSWithSocket
    -- * IO backend
  , IOBackend (..)
    -- * Re-exports
  , Credentials
  ) where

import qualified Control.Exception as E
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket as NS
import Network.Socket (SockAddr, Socket)
import Network.TLS (Credentials)
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import qualified System.TimeManager as TM

-- | TLS server settings (matches the @http2-tls@ shape).
data Settings = Settings
  { settingsTimeout :: !Int
  , settingsSendBufferSize :: !Int
  , settingsSlowlorisSize :: !Int
  , settingsReadBufferSize :: !Int
  , settingsReadBufferLowerLimit :: !Int
  , settingsKeyLogger :: String -> IO ()
  , settingsNumberOfWorkers :: !Int
  , settingsConcurrentStreams :: !Int
  , settingsConnectionWindowSize :: !Int
  , settingsStreamWindowSize :: !Int
  , settingsSessionManager :: !TLS.SessionManager
  , settingsEarlyDataSize :: !Int
  , settingsPingRateLimit :: !Int
  , settingsEmptyFrameRateLimit :: !Int
  , settingsSettingsRateLimit :: !Int
  , settingsRstRateLimit :: !Int
  }

defaultSettings :: Settings
defaultSettings = Settings
  { settingsTimeout = 30
  , settingsSendBufferSize = 4096
  , settingsSlowlorisSize = 50
  , settingsReadBufferSize = 16384
  , settingsReadBufferLowerLimit = 2048
  , settingsKeyLogger = \_ -> pure ()
  , settingsNumberOfWorkers = 8
  , settingsConcurrentStreams = 64
  , settingsConnectionWindowSize = 16777216
  , settingsStreamWindowSize = 262144
  , settingsSessionManager = TLS.noSessionManager
  , settingsEarlyDataSize = 0
  , settingsPingRateLimit = 10
  , settingsEmptyFrameRateLimit = 4
  , settingsSettingsRateLimit = 4
  , settingsRstRateLimit = 4
  }

-- | I\/O backend handed to the action invoked by 'runTLSWithSocket'.
-- Wraps the TLS context's send / recv / sockaddrs in the layout
-- wireform-grpc expects.
data IOBackend = IOBackend
  { send :: !(ByteString -> IO ())
  , sendMany :: !([ByteString] -> IO ())
  , recv :: !(IO ByteString)
  , requestSock :: !Socket
  , mySockAddr :: !SockAddr
  , peerSockAddr :: !SockAddr
  }

-- | Run a TLS handshake on the given socket (advertising the
-- requested ALPN identifier — typically @\"h2\"@), then hand control
-- to the action with a fresh 'TM.Manager' and the bridge 'IOBackend'.
-- The TLS context is torn down via 'TLS.bye' on return.
runTLSWithSocket
  :: Settings
  -> Credentials
  -> Socket
  -> ByteString
  -> (TM.Manager -> IOBackend -> IO a)
  -> IO a
runTLSWithSocket Settings{..} creds sock alpn action = do
  mgr <- TM.initialize (settingsTimeout * 1000 * 1000)
  ctx <- TLS.contextNew sock (mkServerParams creds alpn settingsKeyLogger)
  TLS.handshake ctx
  mysa <- NS.getSocketName sock
  peer <- NS.getPeerName sock
  let backend = IOBackend
        { send = \bs -> TLS.sendData ctx (LBS.fromStrict bs)
        , sendMany = \bss -> TLS.sendData ctx (LBS.fromChunks bss)
        , recv = TLS.recvData ctx
        , requestSock = sock
        , mySockAddr = mysa
        , peerSockAddr = peer
        }
  r <- action mgr backend
  TLS.bye ctx `E.catch` (\(_ :: E.SomeException) -> pure ())
  pure r

mkServerParams
  :: Credentials
  -> ByteString
  -> (String -> IO ())
  -> TLS.ServerParams
mkServerParams creds alpn keyLogger =
  TLS.defaultParamsServer
    { TLS.serverShared = (TLS.serverShared TLS.defaultParamsServer)
        { TLS.sharedCredentials = creds
        }
    , TLS.serverSupported = (TLS.serverSupported TLS.defaultParamsServer)
        { TLS.supportedCiphers = TLS.ciphersuite_default
        }
    , TLS.serverHooks = (TLS.serverHooks TLS.defaultParamsServer)
        { TLS.onALPNClientSuggest = Just $ \offered ->
            if alpn `elem` offered then pure alpn else pure mempty
        }
    , TLS.serverDebug = (TLS.serverDebug TLS.defaultParamsServer)
        { TLS.debugKeyLogger = keyLogger
        }
    }
