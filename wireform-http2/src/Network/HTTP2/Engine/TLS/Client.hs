{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | gRPC-friendly HTTP\/2-over-TLS client engine, OpenSSL-backed.
--
-- The vendored grapesy engine's @http2-tls@-shaped client API.
-- Originally backed by the pure-Haskell @tls@ package; now goes
-- through "Wireform.Network.TLS.OpenSSL" so the engine + grpc
-- speak the same OpenSSL bridge as the rest of the repo.
--
-- 'runWithConfig' opens a TCP connection, performs the OpenSSL TLS
-- handshake (with ALPN @h2@), allocates an 'H2C.Config' over the
-- resulting 'SslConn', and drives 'H2C.run'.
module Network.HTTP2.Engine.TLS.Client
  ( -- * Settings
    Settings (..)
  , defaultSettings
    -- * Runners
  , runWithConfig
  , defaultClientConfig
    -- * Re-exports
  , ClientConfig
    -- * OpenSSL re-exports for callers that want fine-grained control
  , module Wireform.Network.TLS.OpenSSL
  ) where

import qualified Control.Exception as E
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.IORef as IORef
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (castPtr, plusPtr)
import qualified Network.Socket as NS
import qualified System.TimeManager as TM
import Network.Socket (AddrInfo, AddrInfoFlag, PortNumber, Socket, HostName)

import Wireform.Network.TLS.OpenSSL
import Wireform.Network.TLS.Config
  ( TlsClientConfig (..)
  , buildClientCtx
  , defaultTlsClientConfig
  )

import Network.HTTP2.Engine.Client (ClientConfig)
import qualified Network.HTTP2.Engine.Client as H2C
import Network.HTTP2.Engine.Types (Authority, defaultPositionReadMaker)

-- | TLS client settings.
--
-- All fields except 'settingsKeyLogger' / 'settingsValidateCert' /
-- 'settingsCAStorePath' are wireform-network / http2-engine tuning
-- knobs that don't depend on a TLS backend.
data Settings = Settings
  { settingsKeyLogger                :: String -> IO ()
  , settingsValidateCert             :: !Bool
  , settingsCAStorePath              :: !(Maybe FilePath)
    -- ^ Additional PEM trust roots beyond the system store.  When
    -- 'Nothing', only the system store + whatever 'tlsClientCaBundle'
    -- the caller's @TlsClientConfig@ adds.
  , settingsClientCertificate        :: !(Maybe (FilePath, FilePath))
    -- ^ Optional client certificate for mTLS.
  , settingsAddrInfoFlags            :: ![AddrInfoFlag]
  , settingsCacheLimit               :: !Int
  , settingsConcurrentStreams        :: !Int
  , settingsConnectionWindowSize     :: !Int
  , settingsStreamWindowSize         :: !Int
  , settingsServerNameOverride       :: !(Maybe HostName)
  , settingsUseServerNameIndication  :: !Bool
  , settingsOpenClientSocket         :: AddrInfo -> IO Socket
  , settingsUseEarlyData             :: !Bool
  , settingsTimeout                  :: !Int
  , settingsPingRateLimit            :: !Int
  , settingsEmptyFrameRateLimit      :: !Int
  , settingsSettingsRateLimit        :: !Int
  , settingsRstRateLimit             :: !Int
  }

defaultSettings :: Settings
defaultSettings = Settings
  { settingsKeyLogger                = \_ -> pure ()
  , settingsValidateCert             = True
  , settingsCAStorePath              = Nothing
  , settingsClientCertificate        = Nothing
  , settingsAddrInfoFlags            = []
  , settingsCacheLimit               = 64
  , settingsConcurrentStreams        = 64
  , settingsConnectionWindowSize     = 16777216
  , settingsStreamWindowSize         = 262144
  , settingsServerNameOverride       = Nothing
  , settingsUseServerNameIndication  = True
  , settingsOpenClientSocket         = defaultOpenClientSocket
  , settingsUseEarlyData             = False
  , settingsTimeout                  = 30
  , settingsPingRateLimit            = 10
  , settingsEmptyFrameRateLimit      = 4
  , settingsSettingsRateLimit        = 4
  , settingsRstRateLimit             = 4
  }

defaultOpenClientSocket :: AddrInfo -> IO Socket
defaultOpenClientSocket = error
  "Network.HTTP2.Engine.TLS.Client.defaultOpenClientSocket: \
  \callers must override this in Settings; the placeholder is here \
  \only because the engine record demanded a default."

-- | Build a 'ClientConfig' from the supplied 'Settings' + authority.
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

-- | Run a TLS-protected HTTP\/2 client over OpenSSL.
--
-- Opens a TCP connection to @serverName:port@ (using the caller-
-- supplied 'settingsOpenClientSocket'), does an OpenSSL TLS
-- handshake with ALPN @h2@, allocates an 'H2C.Config' over the
-- resulting 'SslConn', and drives 'H2C.run'.
runWithConfig
  :: ClientConfig
  -> Settings
  -> HostName
  -> PortNumber
  -> H2C.Client a
  -> IO a
runWithConfig cliCfg Settings{..} serverName port client = do
  let hints = NS.defaultHints
        { NS.addrSocketType = NS.Stream
        , NS.addrFlags      = settingsAddrInfoFlags
        }
  addrs <- NS.getAddrInfo (Just hints) (Just serverName) (Just (show port))
  case addrs of
    [] -> error "runWithConfig: no addresses"
    (addr:_) -> E.bracket (settingsOpenClientSocket addr) NS.close $ \sock -> do
      NS.connect sock (NS.addrAddress addr)
      ctx <- buildClientCtxFromSettings serverName Settings{..}
      conn <- newClient ctx sock (Just (BS8.pack (sniHost serverName Settings{..})))
      _ <- if settingsValidateCert
             then setClientHostnameVerify conn (BS8.pack serverName)
             else pure ()
      recvN <- mkTlsRecvN conn
      let sendAll = sslSendAll conn
      cfg <- buildClientConfig sendAll recvN 4096
      r <- H2C.run cliCfg cfg client
      E.catch @E.SomeException (freeConn conn >> freeCtx ctx >> pure ())
        (\_ -> pure ())
      pure r

-- | SNI hostname selection: override if requested, else the
-- connect-time hostname.
sniHost :: HostName -> Settings -> HostName
sniHost connectHost Settings{settingsServerNameOverride} =
  case settingsServerNameOverride of
    Just h  -> h
    Nothing -> connectHost

-- | Translate the engine 'Settings' into a configured client
-- 'SslCtx' (ALPN @h2@, optional CA bundle, optional client cert,
-- verify mode).
buildClientCtxFromSettings :: HostName -> Settings -> IO SslCtx
buildClientCtxFromSettings _serverName Settings{..} = do
  let tlsCfg = defaultTlsClientConfig
        { tlsClientVerifyPeer  = settingsValidateCert
        , tlsClientCaBundle    = settingsCAStorePath
        , tlsClientCertificate = settingsClientCertificate
        , tlsClientAlpn        = ["h2"]
        }
  ctx <- buildClientCtx tlsCfg
  setAlpnClient ctx ["h2"]
  pure ctx

-- | OpenSSL send-loop helper.  Loops on short writes; throws on
-- @SSL_write_ex@ returning 0.
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
        then ioError (userError "Engine.TLS.Client: SSL_write_ex returned 0")
        else drain (p `plusPtr` k) (n - k)

-- | Build a recvN over the OpenSSL connection with a small leftover
-- buffer (TLS returns plaintext in chunks of indeterminate size;
-- the engine asks for exact byte counts).
mkTlsRecvN :: SslConn -> IO (Int -> IO ByteString)
mkTlsRecvN conn = do
  leftoverRef <- IORef.newIORef BS.empty
  pure $ \n -> recvLoop leftoverRef n []
  where
    recvFn = tlsReceiveFn conn
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
              -- Pull up to 16 KiB (a TLS record's worth) into a
              -- fresh ByteString.  createUpTo lets the call return
              -- whatever the OpenSSL recv produced (it may be less
              -- if the record was smaller than the buffer).
              chunk <- BSI.createUptoN 16384 $ \dst ->
                         recvFn dst 16384
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
    { H2C.confWriteBuffer       = buf
    , H2C.confBufferSize        = bufSize
    , H2C.confSendAll           = sendAll
    , H2C.confReadN             = readN
    , H2C.confPositionReadMaker = defaultPositionReadMaker
    , H2C.confTimeoutManager    = mgr
    }
