{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Network.TlsOffload
Description : Sidecar / kTLS-style TLS-offload configuration for the
              broker connection layer
Copyright   : (c) 2026
License     : BSD-3-Clause
Maintainer  : kafka-native

\"TLS offload\" describes the deployment pattern where the cipher
work for Kafka broker traffic is performed somewhere /other/ than
the Haskell client process:

  * a local sidecar proxy (Envoy, linkerd2-proxy, stunnel,
    @kafka-proxy@) that terminates mTLS upstream and exposes the
    plaintext Kafka wire on @127.0.0.1@ or a Unix domain socket;
  * a load balancer that strips TLS before forwarding to the
    client (AWS NLB target-group TLS, GCP L4 TLS, ...);
  * kernel TLS (kTLS) offload where the application keeps
    handing over plaintext and the kernel encrypts on the way
    out (Linux @CONFIG_TLS@).

In all of those cases the Haskell client should /not/ run the
@crypton-connection@ TLS handshake itself: the bytes leaving the
process are already ciphered (or are about to be, by something
else under the client's deployment control). What the client
/does/ need is:

  1. A way to disable its own TLS path and use plain TCP / UDS
     even though the logical broker URL says @kafka+ssl://@.
  2. A way to /redirect/ outgoing broker connections from the
     advertised broker address to a local proxy endpoint
     (because the broker still advertises its own host:port in
     'MetadataResponse', not the sidecar's).
  3. Optionally, a way to address that proxy over a Unix
     domain socket rather than another TCP port.

This module models exactly that. It is intentionally tiny: the
configuration is just a function @'OffloadBrokerKey' -> 'IO'
('Maybe' 'TlsOffloadEndpoint')@ plus a couple of smart
constructors for the common shapes. The wiring into the
connection layer lives in 'Kafka.Network.Connection'.

== Choosing an offload mode

* __Transparent offload__ ('transparentTlsOffload'): use when
  the offload happens out-of-band (iptables/TPROXY redirect,
  Linux kTLS, NLB target-group). The client opens a plain TCP
  connection to the broker's advertised address; the network
  layer transparently encrypts.

* __Static sidecar__ ('staticTlsOffload'): every broker
  connection routes through one fixed TCP or UDS endpoint. The
  sidecar (typically Envoy/stunnel/linkerd2-proxy) is
  responsible for selecting the upstream broker — usually via
  port-based routing, TLS SNI, or PROXY-v2 metadata.

* __Per-broker map__ ('perBrokerTlsOffload'): give each broker
  its own offload endpoint. Useful when a sidecar listens on a
  different port per upstream broker (this is the standard
  @stunnel@ configuration for an MSK cluster) and the client
  has a fixed deployment-time map of broker → port.

* __Custom__ ('customTlsOffload'): pass an arbitrary @'OffloadBrokerKey'
  -> 'IO' ('Maybe' 'TlsOffloadEndpoint')@ if your sidecar is
  queried by service discovery at runtime.

In every offload mode the client /still/ keys its connection
pool by the broker's logical @BrokerAddress@, so request
pipelining and SASL state remain per-broker even when several
brokers fan in to the same physical socket destination.

== Notes on import structure

We deliberately do /not/ import @Kafka.Network.Connection@ here:
'TlsOffload' is consumed /by/ that module, and we want a
straight-line dependency, not a cycle. Callers don't notice —
'Kafka.Network.Connection' re-exports the 'OffloadBrokerKey'
constructor / accessors so a typical client only has to import
@Kafka.Network.Connection@.
-}
module Kafka.Network.TlsOffload
  ( -- * Broker key
    OffloadBrokerKey (..)
    -- * Endpoint
  , TlsOffloadEndpoint (..)
  , describeOffloadEndpoint
    -- * Configuration
  , TlsOffloadConfig (..)
  , resolveOffloadEndpoint
    -- * Smart constructors
  , transparentTlsOffload
  , staticTlsOffload
  , perBrokerTlsOffload
  , customTlsOffload
  ) where

import Data.Hashable (Hashable (hashWithSalt))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Socket (HostName, PortNumber)

-- | A broker identity, used as the lookup key for an offload
-- resolver. Mirrors the @(host, port)@ pair of
-- @Kafka.Network.Connection.BrokerAddress@ without importing
-- it (so this module sits below 'Kafka.Network.Connection' in
-- the dependency graph).
data OffloadBrokerKey = OffloadBrokerKey
  { offloadBrokerHost :: !HostName
  , offloadBrokerPort :: !PortNumber
  } deriving stock (Eq, Show, Ord, Generic)

instance Hashable OffloadBrokerKey where
  hashWithSalt salt (OffloadBrokerKey h p) =
    salt `hashWithSalt` h `hashWithSalt` (fromIntegral p :: Int)

-- | Physical destination for an offloaded broker connection.
--
-- The proxy listening at this endpoint is responsible for the
-- TLS handshake (and any other security framing) toward the
-- upstream broker. The Haskell client only ever speaks the
-- Kafka wire protocol in cleartext to this address.
data TlsOffloadEndpoint
  = TlsOffloadTcp !HostName !PortNumber
    -- ^ Open a plain TCP connection to this host:port. The
    --   listener at the other end is assumed to be a sidecar
    --   proxy that performs the upstream TLS handshake.
  | TlsOffloadUnix !FilePath
    -- ^ Open a Unix-domain stream socket at this filesystem
    --   path. Same semantics as the TCP variant; the proxy is
    --   local-only and doesn't expose a TCP port.
  deriving stock (Eq, Show, Ord, Generic)

-- | A short human-readable label for an offload endpoint.
-- Used by 'Kafka.Network.Connection' for error messages and
-- log lines so the operator can tell which sidecar a failure
-- came from.
describeOffloadEndpoint :: TlsOffloadEndpoint -> String
describeOffloadEndpoint = \case
  TlsOffloadTcp  h p -> "tcp:"  <> h <> ":" <> show p
  TlsOffloadUnix path -> "unix:" <> path

-- | Top-level offload configuration plugged into
-- 'Kafka.Network.Connection.ConnectionConfig' via
-- @connTlsOffload@.
--
-- When this field is 'Just', the connection layer:
--
--   1. Skips the client-side TLS handshake regardless of
--      @connUseTls@ (the sidecar handles TLS upstream).
--   2. Looks up the physical endpoint for each broker via
--      'tlsOffloadResolve'. If that returns 'Nothing' the
--      client falls back to the broker's advertised address —
--      this is how 'transparentTlsOffload' avoids forcing
--      operators to enumerate brokers.
--   3. Still keys the connection pool by the logical broker
--      address, so per-broker SASL state and request
--      pipelining are preserved.
--
-- The 'tlsOffloadLabel' is informational; it's surfaced in
-- error messages so an operator can tell at a glance which
-- sidecar a connection went through.
data TlsOffloadConfig = TlsOffloadConfig
  { tlsOffloadResolve :: !(OffloadBrokerKey -> IO (Maybe TlsOffloadEndpoint))
    -- ^ Map a broker (logical host:port) to the physical
    --   sidecar endpoint we should open. Returning 'Nothing'
    --   means \"use the broker's own address unchanged\".
  , tlsOffloadLabel   :: !Text
    -- ^ Human-readable label for the offload mode (e.g.,
    --   @\"static:tcp:127.0.0.1:9092\"@,
    --   @\"per-broker(3 entries)\"@). Used only for logging.
  } deriving stock (Generic)

-- | Resolve the physical endpoint for a broker.
--
-- Convenience wrapper around 'tlsOffloadResolve'.
resolveOffloadEndpoint
  :: TlsOffloadConfig
  -> OffloadBrokerKey
  -> IO (Maybe TlsOffloadEndpoint)
resolveOffloadEndpoint = tlsOffloadResolve

-- | Transparent offload: every broker connection goes to the
-- broker's advertised address unchanged, on the assumption
-- that something outside the client process (TPROXY,
-- iptables, NLB, kTLS) is doing the cipher work.
--
-- This is the right choice for kernel-level TLS offload
-- (Linux @CONFIG_TLS@), for Layer-4 TLS-terminating load
-- balancers, and for environments where you've turned off the
-- application-level handshake but kept the routing identical.
transparentTlsOffload :: TlsOffloadConfig
transparentTlsOffload = TlsOffloadConfig
  { tlsOffloadResolve = \_ -> pure Nothing
  , tlsOffloadLabel   = "transparent"
  }

-- | Static sidecar offload: every broker connection routes to
-- the same physical endpoint. The sidecar is responsible for
-- mapping that to the right upstream broker (typically by
-- port-based routing, TLS SNI based on the broker hostname,
-- or @PROXY-v2@ metadata).
staticTlsOffload :: TlsOffloadEndpoint -> TlsOffloadConfig
staticTlsOffload ep = TlsOffloadConfig
  { tlsOffloadResolve = \_ -> pure (Just ep)
  , tlsOffloadLabel   = T.pack ("static:" <> describeOffloadEndpoint ep)
  }

-- | Per-broker offload: lookup table from broker key to
-- physical proxy endpoint.
--
-- The most common shape — one stunnel/Envoy listener per
-- upstream broker, on different localhost ports.
--
-- Brokers not present in the map fall through to the
-- broker's own address, mirroring 'transparentTlsOffload'
-- semantics for brokers you don't explicitly route.
perBrokerTlsOffload
  :: Map OffloadBrokerKey TlsOffloadEndpoint
  -> TlsOffloadConfig
perBrokerTlsOffload m = TlsOffloadConfig
  { tlsOffloadResolve = \b -> pure (Map.lookup b m)
  , tlsOffloadLabel   =
      T.pack ("per-broker(" <> show (Map.size m) <> " entries)")
  }

-- | Custom offload: arbitrary resolution function.
--
-- Use when the sidecar is discovered at runtime — for
-- example, asking a service-mesh control plane for the
-- current sidecar's listener address, or routing per cluster
-- region.
customTlsOffload
  :: Text
  -- ^ Label for logs.
  -> (OffloadBrokerKey -> IO (Maybe TlsOffloadEndpoint))
  -> TlsOffloadConfig
customTlsOffload label f = TlsOffloadConfig
  { tlsOffloadResolve = f
  , tlsOffloadLabel   = label
  }
