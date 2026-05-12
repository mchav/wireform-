{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Kafka.Streams.Discovery.RemoteIQ
-- Description : Cross-instance interactive-query forwarding hook
--
-- Cross-instance interactive queries: if a key the user asks
-- about lives on a /peer/ instance, the application forwards
-- the query to that peer's @application.server@ host:port
-- instead of returning \"store missing\" locally.
--
-- The Kafka Streams API ships the /discovery/ half of this in
-- the runtime (which peer owns each key) but deliberately
-- leaves the /transport/ to the application — neither the
-- protocol nor any KIP specifies a wire for cross-instance IQ.
-- Confluent's reference apps typically expose REST endpoints
-- (Jersey \/ JAX-RS); other shops use gRPC; some go straight
-- TCP. There is no single right answer, so this module follows
-- the JVM convention and exposes the routing /decision/ plus a
-- 'RemoteIQ' record that the application populates with
-- whatever client it already uses.
--
-- For an in-house gRPC implementation, plug your
-- @wireform-grpc@ client into 'runRemoteIQ'. For interop with
-- JVM Streams instances that expose @\/state\/keyvalue\/{store}\/{key}@,
-- plug an HTTP client in. For tests, use 'noopRemoteIQ' or a
-- hand-supplied stub.
--
-- == Typical usage
--
-- @
-- let metadata = makeKeyQueryMetadata peers \"orders\"
--                  (hashedPartitionFor key 12)
-- case routeQuery myHost metadata of
--   RouteMissing      -> ...
--   RouteLocal        -> readLocal store key
--   RouteRemote peer  -> runRemoteIQ transport peer
--                          (RemoteIQRequest \"orders\" key)
-- @
module Kafka.Streams.Discovery.RemoteIQ
  ( RemoteIQ (..)
  , RemoteIQRequest (..)
  , RemoteIQResponse (..)
  , noopRemoteIQ
  , routeQuery
  , RouteDecision (..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Kafka.Streams.Discovery
  ( HostInfo (..)
  , KeyQueryMetadata (..)
  )
import Kafka.Streams.State.Store (StoreName)

-- | One IQ request to a remote peer. Bytes-typed because the
-- typed serde layer lives on the calling side.
data RemoteIQRequest = RemoteIQRequest
  { store :: !StoreName
  , key   :: !ByteString
  }
  deriving stock (Eq, Show)

data RemoteIQResponse
  = RIQFound !ByteString
  | RIQAbsent
  | RIQError !Text
  deriving stock (Eq, Show)

-- | Pluggable transport. Production callers wire in their HTTP
-- / gRPC client; tests use a deterministic in-process
-- implementation.
newtype RemoteIQ = RemoteIQ
  { runRemoteIQ
      :: HostInfo -> RemoteIQRequest -> IO RemoteIQResponse
  }

-- | A 'RemoteIQ' that always replies 'RIQAbsent'. Use this
-- when the application doesn't /want/ to proxy across hosts —
-- callers still get a typed Nothing instead of a crash.
noopRemoteIQ :: RemoteIQ
noopRemoteIQ = RemoteIQ $ \_ _ -> pure RIQAbsent

-- | Decision of whether to read locally or forward.
data RouteDecision
  = RouteLocal
  | RouteRemote !HostInfo
  | RouteMissing
  deriving stock (Eq, Show)

-- | Given the resolved 'KeyQueryMetadata' for @(store, key)@
-- and the instance's own 'HostInfo', decide where to read.
-- Standbys are treated as fallbacks: when the active is on a
-- remote host and the standby is local, prefer the local
-- read.
routeQuery
  :: HostInfo                 -- ^ local host
  -> Maybe KeyQueryMetadata
  -> RouteDecision
routeQuery _local Nothing  = RouteMissing
routeQuery  local (Just kqm)
  | kqm.activeHost == local           = RouteLocal
  | any (== local) kqm.standbyHosts   = RouteLocal
  | otherwise                         = RouteRemote kqm.activeHost

