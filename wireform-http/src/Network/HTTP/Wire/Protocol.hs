{- | Protocol-version metadata that flows alongside requests and
responses.

These are deliberately optional: a transport that doesn't care
populates 'HTTP1_1' and ignores 'ProtocolHints'. They exist so
callers that /do/ care can branch on the negotiated version (HTTP\/2
push, HTTP\/3 0-RTT) by pattern matching, without forcing every
transport to implement those features.
-}
module Network.HTTP.Wire.Protocol
  ( ProtocolInfo (..)
  , Http2Info (..)
  , Http3Info (..)
  , PushPromise (..)
  , ProtocolHints (..)
  , H2Priority (..)
  , defaultHints
    -- * Transport capabilities
  , TransportCapabilities (..)
  , defaultCapabilities
  ) where

import Data.Word (Word32, Word64)

-- | The version actually negotiated for a particular response.
data ProtocolInfo
  = HTTP1_1
  | HTTP2 !Http2Info
  | HTTP3 !Http3Info
  deriving stock (Show)

data Http2Info = Http2Info
  { h2StreamId     :: !Word32
  , h2PushPromises :: !(IO [PushPromise])
    -- ^ Realised lazily: forcing this returns whatever push promises
    -- the server has announced on this stream so far.
  }

instance Show Http2Info where
  show i = "Http2Info { h2StreamId = " <> show (h2StreamId i) <> " }"

data Http3Info = Http3Info
  { h3StreamId     :: !Word64
  , h3PeerAddr     :: !(Maybe String)
  }
  deriving stock (Show)

-- | An HTTP\/2 push promise. The promised request is the request the
-- server claims it's about to fulfil; 'pushFulfil' blocks until the
-- response actually arrives.
data PushPromise = PushPromise
  { pushPromisedRequest :: !()  -- placeholder; will be Request Void
                               -- once mutual recursion is broken out.
  , pushFulfil          :: !(IO ())  -- placeholder
  }

instance Show PushPromise where
  show _ = "<PushPromise>"

-- | Per-request advisory hints. Transports that don't understand a
-- given hint ignore it.
data ProtocolHints = ProtocolHints
  { h2Priority   :: !(Maybe H2Priority)
  , h3EarlyData  :: !Bool
    -- ^ Whether this request is eligible to be sent in HTTP\/3 0-RTT.
    -- Must only be set for idempotent requests with no body
    -- side-effects (per RFC 9114 § 4.1.4).
  }
  deriving stock (Show)

data H2Priority = H2Priority
  { h2Urgency      :: !Int    -- ^ 0 (highest) — 7 (lowest)
  , h2Incremental  :: !Bool
  }
  deriving stock (Show)

defaultHints :: ProtocolHints
defaultHints = ProtocolHints
  { h2Priority  = Nothing
  , h3EarlyData = False
  }

-- ---------------------------------------------------------------------------
-- Transport capabilities (for infrastructure code)
-- ---------------------------------------------------------------------------

data TransportCapabilities = TransportCapabilities
  { supportsH2Push  :: !Bool
  , supportsH3      :: !Bool
  , supports0RTT    :: !Bool
  , maxConcurrency  :: !(Maybe Int)
  }
  deriving stock (Show)

defaultCapabilities :: TransportCapabilities
defaultCapabilities = TransportCapabilities
  { supportsH2Push = False
  , supportsH3     = False
  , supports0RTT   = False
  , maxConcurrency = Nothing
  }
