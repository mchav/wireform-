{- | Protocol-version metadata that flows alongside requests and
responses.

These are deliberately optional: a transport that doesn't care
populates 'HTTP1_1' and ignores 'ProtocolHints'. They exist so
callers that /do/ care can branch on the negotiated version (HTTP\/2
push, HTTP\/3 0-RTT) by pattern matching, without forcing every
transport to implement those features.

The 'PushPromise' payload references the request and the raw
response types from "Network.HTTP.Client.Request" and
"Network.HTTP.Client.Response", which would induce a mutual-import
cycle if we declared them all in one module. To keep the dependency
graph clean, 'PushPromise' is parameterised over its request and
response types and re-exported with the concrete types in
"Network.HTTP.Client.Protocol.Pushed".
-}
module Network.HTTP.Client.Protocol
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

import Data.Void (Void)
import Data.Word (Word32, Word64)

-- | The version actually negotiated for a particular response.
-- 'Http2Info' / 'Http3Info' are parameterised over the request and
-- raw-response types so the protocol module can declare them
-- without depending on the higher-level Request \/ Response
-- modules; concrete aliases live in "Network.HTTP.Client" and
-- 'Network.HTTP.Client.Send'.
data ProtocolInfo req raw
  = HTTP1_1
  | HTTP2 !(Http2Info req raw)
  | HTTP3 !Http3Info

instance Show (ProtocolInfo req raw) where
  show HTTP1_1     = "HTTP1_1"
  show (HTTP2 _)   = "HTTP2 <Http2Info>"
  show (HTTP3 i)   = "HTTP3 " <> show i

data Http2Info req raw = Http2Info
  { h2StreamId     :: !Word32
  , h2PushPromises :: !(IO [PushPromise req raw])
    -- ^ Realised lazily: forcing this returns whatever push promises
    -- the server has announced on this stream so far.
  }

data Http3Info = Http3Info
  { h3StreamId     :: !Word64
  , h3PeerAddr     :: !(Maybe String)
  }
  deriving stock (Show)

-- | An HTTP\/2 push promise. 'pushPromisedRequest' is the request
-- the server claims it's about to fulfil (always a no-body 'req
-- Void' value); 'pushFulfil' blocks until the response actually
-- arrives.
data PushPromise req raw = PushPromise
  { pushPromisedRequest :: !(req Void)
  , pushFulfil          :: !(IO raw)
  }

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
