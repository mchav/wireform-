-- | HTTP\/2 settings + per-server configuration record for the
-- engine. Names and defaults follow @http2-5.3.x@ so wireform-grpc
-- code constructs identical configurations.
module Network.HTTP2.Engine.Settings
  ( ServerConfig (..)
  , defaultServerConfig
  , Settings (..)
  , defaultSettings
  ) where

import Data.Word (Word32)

-- | Per-server-instance configuration (rate limits, default window
-- sizes, deprecated knobs).
data ServerConfig = ServerConfig
  { numberOfWorkers :: !Int
    -- ^ Deprecated, kept for source compatibility with the older
    -- @http2@ API.
  , connectionWindowSize :: !Int
    -- ^ Initial connection-level window size for incoming streams.
  , settings :: !Settings
    -- ^ HTTP\/2 settings to advertise.
  }
  deriving stock (Eq, Show)

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { numberOfWorkers = 8
  , connectionWindowSize = 16777216  -- 16 MiB
  , settings = defaultSettings
  }

-- | HTTP\/2 connection settings (RFC 9113 §6.5). Field names match
-- the @http2@ package so wireform-grpc record-update sites compile
-- unchanged.
data Settings = Settings
  { headerTableSize :: !Int
  , enablePush :: !Bool
  , maxConcurrentStreams :: !(Maybe Word32)
  , initialWindowSize :: !Int
  , maxFrameSize :: !Int
  , maxHeaderListSize :: !(Maybe Word32)
  , pingRateLimit :: !Int
    -- ^ Max PINGs per second (CVE-2019-9512).
  , emptyFrameRateLimit :: !Int
    -- ^ Max empty DATA frames per second (CVE-2019-9518).
  , settingsRateLimit :: !Int
    -- ^ Max SETTINGS frames per second (CVE-2019-9515).
  , rstRateLimit :: !Int
    -- ^ Max RST_STREAM frames per second (CVE-2023-44487).
  }
  deriving stock (Eq, Show)

defaultSettings :: Settings
defaultSettings = Settings
  { headerTableSize = 4096
  , enablePush = True
  , maxConcurrentStreams = Just 64
  , initialWindowSize = 262144  -- 256 KiB
  , maxFrameSize = 16384
  , maxHeaderListSize = Nothing
  , pingRateLimit = 10
  , emptyFrameRateLimit = 4
  , settingsRateLimit = 4
  , rstRateLimit = 4
  }
