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

import qualified Control.Exception as E
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IORef as IORef
import Data.X509.CertificateStore (CertificateStore, makeCertificateStore)
import Foreign.Marshal.Alloc (mallocBytes)
import qualified Network.Socket as NS
import qualified Network.TLS.Extra.Cipher as TLS
import qualified System.TimeManager as TM
import Network.Socket (AddrInfo, AddrInfoFlag, PortNumber, Socket, HostName)
import qualified Network.TLS as TLS

import Network.HTTP2.Engine.Client (ClientConfig)
import qualified Network.HTTP2.Engine.Client as H2C
import Network.HTTP2.Engine.Types (Authority, defaultPositionReadMaker)

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

-- | Run a TLS-protected HTTP\/2 client.
--
-- Opens a TCP connection to @serverName:port@ (using the
-- caller-supplied 'settingsOpenClientSocket'), does a TLS handshake
-- with ALPN @h2@, allocates an 'H2C.Config' over the resulting
-- 'TLS.Context', and drives 'H2C.run'.
runWithConfig
  :: ClientConfig
  -> Settings
  -> HostName
  -> PortNumber
  -> H2C.Client a
  -> IO a
runWithConfig cliCfg Settings{..} serverName port client = do
  -- Resolve and connect.
  let hints = NS.defaultHints
        { NS.addrSocketType = NS.Stream
        , NS.addrFlags = settingsAddrInfoFlags
        }
  addrs <- NS.getAddrInfo (Just hints) (Just serverName) (Just (show port))
  case addrs of
    [] -> error "runWithConfig: no addresses"
    (addr:_) -> E.bracket (settingsOpenClientSocket addr) NS.close $ \sock -> do
      NS.connect sock (NS.addrAddress addr)
      ctx <- TLS.contextNew sock (mkClientParams Settings{..} serverName port)
      TLS.handshake ctx
      recvN <- mkTlsRecvN ctx
      let sendAll bs = TLS.sendData ctx (LBS.fromStrict bs)
      cfg <- buildClientConfig sendAll recvN 4096
      r <- H2C.run cliCfg cfg client
      (TLS.bye ctx) `E.catch` (\(_ :: E.SomeException) -> pure ())
      pure r

-- | Build a recvN over the TLS context with a small leftover buffer
-- (TLS returns chunks of indeterminate size; the engine asks for
-- exact byte counts).
mkTlsRecvN :: TLS.Context -> IO (Int -> IO ByteString)
mkTlsRecvN ctx = do
  leftoverRef <- IORef.newIORef BS.empty
  pure $ \n -> recvLoop leftoverRef n []
  where
    recvLoop leftoverRef remaining acc
      | remaining <= 0 = pure (BS.concat (reverse acc))
      | otherwise = do
          leftover <- IORef.readIORef leftoverRef
          if not (BS.null leftover)
            then do
              let take' = min remaining (BS.length leftover)
                  (consume, rest) = BS.splitAt take' leftover
              IORef.writeIORef leftoverRef rest
              recvLoop leftoverRef (remaining - take') (consume : acc)
            else do
              chunk <- TLS.recvData ctx
              if BS.null chunk
                then pure (BS.concat (reverse acc))
                else do
                  IORef.writeIORef leftoverRef chunk
                  recvLoop leftoverRef remaining acc

buildClientConfig
  :: (ByteString -> IO ())
  -> (Int -> IO ByteString)
  -> Int
  -> IO H2C.Config
buildClientConfig sendAll readN bufSize = do
  buf <- mallocBytes bufSize
  mgr <- TM.initialize (30 * 1000 * 1000)
  pure H2C.Config
    { H2C.confWriteBuffer = buf
    , H2C.confBufferSize = bufSize
    , H2C.confSendAll = sendAll
    , H2C.confReadN = readN
    , H2C.confPositionReadMaker = defaultPositionReadMaker
    , H2C.confTimeoutManager = mgr
    }

mkClientParams :: Settings -> HostName -> PortNumber -> TLS.ClientParams
mkClientParams Settings{..} serverName port =
  let base = TLS.defaultParamsClient serverName (BSI.packChars (show port))
  in base
      { TLS.clientHooks = (TLS.clientHooks base)
          { TLS.onSuggestALPN = pure (Just ["h2"])
          , TLS.onServerCertificate = settingsOnServerCertificate
          }
      , TLS.clientSupported = (TLS.clientSupported base)
          { TLS.supportedCiphers = TLS.ciphersuite_default
          }
      , TLS.clientShared = (TLS.clientShared base)
          { TLS.sharedValidationCache =
              if settingsValidateCert
                then TLS.sharedValidationCache (TLS.clientShared base)
                else TLS.ValidationCache
                       (\_ _ _ -> pure TLS.ValidationCachePass)
                       (\_ _ _ -> pure ())
          }
      , TLS.clientUseServerNameIndication = settingsUseServerNameIndication
      }
