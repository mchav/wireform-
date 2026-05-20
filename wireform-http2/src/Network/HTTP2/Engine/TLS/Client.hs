{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- | gRPC-friendly HTTP\/2-over-TLS client engine.
--
-- The @http2-tls@-shaped client API in wireform-http2's namespace.
-- Currently exposes 'runWithConfig' + the 'Settings' record;
-- the runtime is stubbed out (it errors at runtime) until the
-- matching @Network.HTTP2.Engine.Client.run@ implementation lands.
module Network.HTTP2.Engine.TLS.Client
  ( -- * Settings
    Settings (..)
  , defaultSettings
    -- * Runners
  , runWithConfig
  , defaultClientConfig
    -- * Re-exports
  , ClientConfig
  ) where

import Data.X509.CertificateStore (CertificateStore, makeCertificateStore)
import Network.Socket (AddrInfo, AddrInfoFlag, PortNumber, Socket, HostName)
import qualified Network.TLS as TLS

import Network.HTTP2.Engine.Client (ClientConfig)
import qualified Network.HTTP2.Engine.Client as H2C
import Network.HTTP2.Engine.Types (Authority)

-- | TLS client settings (matches the @http2-tls@ shape).
data Settings = Settings
  { settingsKeyLogger :: String -> IO ()
  , settingsValidateCert :: !Bool
  , settingsOnServerCertificate :: TLS.OnServerCertificate
  , settingsCAStore :: !CertificateStore
  , settingsAddrInfoFlags :: ![AddrInfoFlag]
  , settingsCacheLimit :: !Int
  , settingsConcurrentStreams :: !Int
  , settingsConnectionWindowSize :: !Int
  , settingsStreamWindowSize :: !Int
  , settingsServerNameOverride :: !(Maybe HostName)
  , settingsUseServerNameIndication :: !Bool
  , settingsSessionManager :: !TLS.SessionManager
  , settingsWantSessionResume :: !(Maybe (TLS.SessionID, TLS.SessionData))
  , settingsWantSessionResumeList :: ![(TLS.SessionID, TLS.SessionData)]
  , settingsOpenClientSocket :: AddrInfo -> IO Socket
  , settingsUseEarlyData :: !Bool
  , settingsOnServerFinished :: !(TLS.HandshakeMode13 -> IO ())
  , settingsTimeout :: !Int
  , settingsPingRateLimit :: !Int
  , settingsEmptyFrameRateLimit :: !Int
  , settingsSettingsRateLimit :: !Int
  , settingsRstRateLimit :: !Int
  }

defaultSettings :: Settings
defaultSettings = Settings
  { settingsKeyLogger = \_ -> pure ()
  , settingsValidateCert = True
  , settingsOnServerCertificate = \_ _ _ _ -> pure []
  , settingsCAStore = makeCertificateStore []
  , settingsAddrInfoFlags = []
  , settingsCacheLimit = 64
  , settingsConcurrentStreams = 64
  , settingsConnectionWindowSize = 16777216
  , settingsStreamWindowSize = 262144
  , settingsServerNameOverride = Nothing
  , settingsUseServerNameIndication = True
  , settingsSessionManager = TLS.noSessionManager
  , settingsWantSessionResume = Nothing
  , settingsWantSessionResumeList = []
  , settingsOpenClientSocket = defaultOpenClientSocket
  , settingsUseEarlyData = False
  , settingsOnServerFinished = \_ -> pure ()
  , settingsTimeout = 30
  , settingsPingRateLimit = 10
  , settingsEmptyFrameRateLimit = 4
  , settingsSettingsRateLimit = 4
  , settingsRstRateLimit = 4
  }

defaultOpenClientSocket :: AddrInfo -> IO Socket
defaultOpenClientSocket = error
  "Network.HTTP2.Engine.TLS.Client.defaultOpenClientSocket: \
  \callers must override this in Settings; the placeholder is here \
  \only because the http2-tls record demanded a default."

-- | Build a 'ClientConfig' from the supplied 'Settings' + authority.
-- Mirrors @http2-tls@'s @defaultClientConfig@.
defaultClientConfig :: Settings -> Authority -> ClientConfig
defaultClientConfig Settings{..} auth =
  H2C.defaultClientConfig
    { H2C.authority = auth
    , H2C.connectionWindowSize = settingsConnectionWindowSize
    , H2C.settings = (H2C.settings H2C.defaultClientConfig)
        { H2C.initialWindowSize = settingsStreamWindowSize
        , H2C.maxConcurrentStreams = Just (fromIntegral settingsConcurrentStreams)
        , H2C.pingRateLimit = settingsPingRateLimit
        , H2C.emptyFrameRateLimit = settingsEmptyFrameRateLimit
        , H2C.settingsRateLimit = settingsSettingsRateLimit
        , H2C.rstRateLimit = settingsRstRateLimit
        }
    }

-- | Run a TLS-protected HTTP\/2 client. TODO: stubbed; needs the
-- engine 'Network.HTTP2.Engine.Client.run' to come online first.
runWithConfig
  :: ClientConfig
  -> Settings
  -> HostName
  -> PortNumber
  -> H2C.Client a
  -> IO a
runWithConfig _ _ _ _ _ =
  error "Network.HTTP2.Engine.TLS.Client.runWithConfig: runtime not yet implemented"
