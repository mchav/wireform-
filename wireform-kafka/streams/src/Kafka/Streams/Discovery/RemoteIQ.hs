{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Kafka.Streams.Discovery.RemoteIQ
-- Description : Transport-agnostic remote interactive-query proxy
--
-- KIP-535 introduces cross-instance IQ: if a key the user
-- asks about lives on a /peer/ instance, the application
-- forwards the query to that peer's @application.server@
-- host:port instead of returning "store missing" locally.
--
-- The transport is the user's choice — gRPC, HTTP, raw TCP.
-- This module exposes the routing /decision/ and a
-- pluggable 'RemoteIQ' interface so call sites stay typed
-- and testable.
--
-- == Typical usage
--
-- @
-- let metadata = makeKeyQueryMetadata peers \"orders\"
--                  (hashedPartitionFor key 12)
-- case metadata of
--   Nothing  -> ...                 -- nobody owns it
--   Just kqm ->
--     if kqmActiveHost kqm == myHost
--       then readLocal store key      -- local read
--       else remoteIqFetch transport
--             (kqmActiveHost kqm) storeName key
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
import Kafka.Streams.State.Store (StoreName, unStoreName)

-- | One IQ request to a remote peer. Bytes-typed because the
-- typed serde layer lives on the calling side.
data RemoteIQRequest = RemoteIQRequest
  { rqStore :: !StoreName
  , rqKey   :: !ByteString
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
  | kqmActiveHost kqm == local             = RouteLocal
  | any (== local) (kqmStandbyHosts kqm)   = RouteLocal
  | otherwise                              = RouteRemote
                                               (kqmActiveHost kqm)

-- Silence the otherwise-unused 'unStoreName' import; callers
-- typically use it when building the transport URL.
_keep :: StoreName -> Text
_keep = unStoreName
