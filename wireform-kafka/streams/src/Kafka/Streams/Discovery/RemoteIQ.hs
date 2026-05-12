{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Kafka.Streams.Discovery.RemoteIQ
-- Description : Cross-instance interactive-query proxy over wireform-grpc
--
-- A streams app running as N pods needs each pod to be able to answer
-- queries about keys it doesn't own locally. When a query arrives at
-- pod A for a key that lives on pod B, pod A forwards the request to
-- pod B's @application.server@ and proxies the response back.
--
-- The wire is gRPC, served by
-- [wireform-grpc](https://hackage.haskell.org/package/wireform-grpc).
-- The RPC is a single unary call:
--
-- @
-- service wireform.kafka.streams.RemoteIQ {
--   rpc Fetch (Request) returns (Response);
-- }
-- @
--
-- with request \/ response payloads serialised via 'Data.Binary' (no
-- protobuf schema required — the typed envelopes live in this
-- module).
--
-- = Picking a transport
--
-- Three variants are supplied, matching the three deployment shapes:
--
--   * 'grpcRemoteIq' (production) — open a wireform-grpc connection to
--     the peer and issue the @Fetch@ RPC. Configure via
--     'GrpcRemoteIqConfig' (TLS on \/ off, server validation,
--     authority override).
--   * 'disabledRemoteIq' — the application doesn't want cross-instance
--     proxying at all. Every remote lookup returns 'RIQAbsent'
--     immediately. Use for single-instance deployments or when the
--     application owns its own DR-isolated query path.
--   * 'mockRemoteIq' — test-only. Hand-supply a responder.
--
-- = Hosting the RPC
--
-- The server-side handler factory 'remoteIqGrpcHandler' returns a
-- 'GrpcS.SomeRpcHandler' suitable for 'GrpcS.mkGrpcServer'. Wire it
-- into your existing wireform-grpc 'GrpcS.ServerParams' alongside
-- whatever other RPCs your application exposes.
module Kafka.Streams.Discovery.RemoteIQ
  ( -- * Routing decision
    RouteDecision (..)
  , routeQuery

    -- * Request \/ response shapes
  , RemoteIQRequest (..)
  , RemoteIQResponse (..)

    -- * Transport
  , RemoteIQ (..)
  , disabledRemoteIq
  , mockRemoteIq
  , grpcRemoteIq
  , GrpcRemoteIqConfig (..)
  , defaultGrpcRemoteIqConfig
  , executeRemoteIq

    -- * Server-side gRPC handler
  , RemoteIqRpc
  , remoteIqGrpcHandler
  ) where

import           Control.Exception                          (SomeException, try)
import           Data.Binary                                (Binary, Get, Put, get, put)
import           Data.Binary.Get                            (getByteString, getWord8, getWord32be)
import           Data.Binary.Put                            (putByteString, putWord8, putWord32be)
import           Data.ByteString                            (ByteString)
import qualified Data.ByteString                            as BS
import           Data.Default.Class                         (def)
import qualified Data.Text                                  as T
import           Data.Text                                  (Text)
import qualified Data.Text.Encoding                         as TE

import qualified Network.GRPC.Client                        as Grpc
import           Network.GRPC.Client                        (rpc)
import qualified Network.GRPC.Client.StreamType.IO.Binary   as GrpcCIO
import           Network.GRPC.Common.Binary                 (RawRpc)
import qualified Network.GRPC.Server                        as GrpcS
import qualified Network.GRPC.Server.StreamType             as GrpcST
import qualified Network.GRPC.Server.StreamType.Binary      as GrpcSB

import           Kafka.Streams.Discovery
  ( HostInfo (..)
  , KeyQueryMetadata (..)
  )
import           Kafka.Streams.State.Store                  (StoreName, storeName, unStoreName)

----------------------------------------------------------------------
-- Routing decision (pure)
----------------------------------------------------------------------

-- | Outcome of the per-key routing decision.
data RouteDecision
  = RouteLocal
  | RouteRemote !HostInfo
  | RouteMissing
  deriving stock (Eq, Show)

-- | Given the resolved 'KeyQueryMetadata' for @(store, key)@ and the
-- instance's own 'HostInfo', decide where to read. Standbys are
-- treated as fallbacks: when the active is on a remote host and the
-- standby is local, prefer the local read.
routeQuery
  :: HostInfo                 -- ^ local host
  -> Maybe KeyQueryMetadata
  -> RouteDecision
routeQuery _local Nothing  = RouteMissing
routeQuery  local (Just kqm)
  | kqm.activeHost == local         = RouteLocal
  | any (== local) kqm.standbyHosts = RouteLocal
  | otherwise                       = RouteRemote kqm.activeHost

----------------------------------------------------------------------
-- Request / response shapes
----------------------------------------------------------------------

-- | One IQ request to a remote peer. Bytes-typed because the typed
-- serde layer lives on the calling side.
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

----------------------------------------------------------------------
-- Binary instances
--
-- The wire shape is a length-prefixed UTF-8 store name followed by
-- a length-prefixed raw key. Responses are tag-prefixed: 0 absent,
-- 1 found+payload, 2 error+message.
----------------------------------------------------------------------

instance Binary RemoteIQRequest where
  put r = do
    putBS (TE.encodeUtf8 (unStoreName r.store))
    putBS r.key
  get = do
    s <- getBS
    k <- getBS
    pure $ RemoteIQRequest (storeName (TE.decodeUtf8 s)) k

instance Binary RemoteIQResponse where
  put RIQAbsent      = putWord8 0
  put (RIQFound bs)  = putWord8 1 *> putBS bs
  put (RIQError msg) = putWord8 2 *> putBS (TE.encodeUtf8 msg)
  get = do
    tag <- getWord8
    case tag of
      0 -> pure RIQAbsent
      1 -> RIQFound <$> getBS
      2 -> RIQError . TE.decodeUtf8 <$> getBS
      _ -> fail $ "RemoteIQResponse: unknown tag " <> show tag

putBS :: ByteString -> Put
putBS !bs = do
  putWord32be (fromIntegral (BS.length bs))
  putByteString bs

getBS :: Get ByteString
getBS = do
  !n <- getWord32be
  getByteString (fromIntegral n)

----------------------------------------------------------------------
-- gRPC service type
----------------------------------------------------------------------

-- | The wireform-grpc RPC identifier:
-- @wireform.kafka.streams.RemoteIQ/Fetch@. Use this as the @rpc@ type
-- argument to 'Network.GRPC.Client.withRPC' or to
-- 'Network.GRPC.Server.StreamType.fromMethod'.
type RemoteIqRpc = RawRpc "wireform.kafka.streams.RemoteIQ" "Fetch"

----------------------------------------------------------------------
-- Transport
----------------------------------------------------------------------

-- | How to forward an interactive-query request to the peer that owns
-- a key. Construct one of the three concrete cases (or build a
-- 'GrpcRemoteIqConfig' and pass it to 'grpcRemoteIq'); dispatch via
-- 'executeRemoteIq'.
data RemoteIQ
  = RemoteIqDisabled
    -- ^ No cross-instance proxying. Every remote lookup returns
    --   'RIQAbsent' immediately.
  | RemoteIqOverGrpc !GrpcRemoteIqConfig
    -- ^ Open a wireform-grpc connection to the peer and issue the
    --   @Fetch@ RPC.
  | RemoteIqMock !(HostInfo -> RemoteIQRequest -> IO RemoteIQResponse)
    -- ^ Test-only hand-supplied responder.

-- | Configuration for the production gRPC client.
data GrpcRemoteIqConfig = GrpcRemoteIqConfig
  { grpcRiqUseTls    :: !Bool
    -- ^ When 'True', open the connection to the peer over TLS. The
    --   server validator falls back to 'grpcRiqValidator' (or the
    --   system trust store if that is 'Nothing').
  , grpcRiqValidator :: !(Maybe Grpc.ServerValidation)
    -- ^ Optional TLS server validator override. 'Nothing' picks the
    --   system trust store via 'Grpc.certStoreFromSystem'.
  , grpcRiqAuthority :: !(Maybe String)
    -- ^ Optional @:authority@ pseudo-header override. 'Nothing' uses
    --   the peer's hostname.
  }

-- | Plaintext, system-trust, no authority override. Override the
-- fields you care about.
defaultGrpcRemoteIqConfig :: GrpcRemoteIqConfig
defaultGrpcRemoteIqConfig = GrpcRemoteIqConfig
  { grpcRiqUseTls    = False
  , grpcRiqValidator = Nothing
  , grpcRiqAuthority = Nothing
  }

-- | Use when the application doesn't proxy cross-instance queries.
disabledRemoteIq :: RemoteIQ
disabledRemoteIq = RemoteIqDisabled

-- | Test-only: hand-supply a responder. The function receives the
-- peer's 'HostInfo' and the original request and returns whatever
-- response the test wants the producer-side caller to see.
mockRemoteIq
  :: (HostInfo -> RemoteIQRequest -> IO RemoteIQResponse)
  -> RemoteIQ
mockRemoteIq = RemoteIqMock

-- | Production: open a wireform-grpc connection to the peer for every
-- 'executeRemoteIq' call and issue the @Fetch@ RPC.
grpcRemoteIq :: GrpcRemoteIqConfig -> RemoteIQ
grpcRemoteIq = RemoteIqOverGrpc

-- | Dispatch a remote IQ request through whichever 'RemoteIQ' the
-- caller chose. Errors thrown by the transport are caught and
-- surfaced as 'RIQError' so the streams runtime never has to deal
-- with gRPC exceptions directly.
executeRemoteIq
  :: RemoteIQ
  -> HostInfo
  -> RemoteIQRequest
  -> IO RemoteIQResponse
executeRemoteIq RemoteIqDisabled        _    _   = pure RIQAbsent
executeRemoteIq (RemoteIqMock f)        host req = f host req
executeRemoteIq (RemoteIqOverGrpc cfg)  host req = do
  result <- try $ grpcExecute cfg host req
  case (result :: Either SomeException RemoteIQResponse) of
    Right r -> pure r
    Left  e -> pure $ RIQError (T.pack ("grpc: " <> show e))

grpcExecute
  :: GrpcRemoteIqConfig
  -> HostInfo
  -> RemoteIQRequest
  -> IO RemoteIQResponse
grpcExecute cfg host req = do
  let addr = Grpc.Address
        { Grpc.addressHost      = T.unpack host.host
        , Grpc.addressPort      = fromIntegral host.port
        , Grpc.addressAuthority = cfg.grpcRiqAuthority
        }
      server
        | cfg.grpcRiqUseTls =
            Grpc.ServerSecure
              (case cfg.grpcRiqValidator of
                 Just v  -> v
                 Nothing -> Grpc.ValidateServer Grpc.certStoreFromSystem)
              Grpc.SslKeyLogNone
              addr
        | otherwise = Grpc.ServerInsecure addr
  Grpc.withConnection def server $ \conn ->
    GrpcCIO.nonStreaming conn (rpc @RemoteIqRpc) req

----------------------------------------------------------------------
-- Server-side handler
----------------------------------------------------------------------

-- | Build a gRPC handler that exposes the supplied
-- @RemoteIQRequest -> IO RemoteIQResponse@ as the @Fetch@ RPC. Plug
-- the result into 'GrpcS.mkGrpcServer' alongside any other handlers
-- your application exposes.
--
-- The typical wiring is to thread the streams app's state-store
-- lookup function through this handler:
--
-- @
-- let serve req = do
--       store <- queryKVStore @ByteString @ByteString ks req.store
--       case store of
--         Nothing -> pure 'RIQAbsent'
--         Just s  -> do
--           mv <- s.'roKvGet' req.key
--           pure $ maybe 'RIQAbsent' 'RIQFound' mv
--     handler = 'remoteIqGrpcHandler' serve
-- @
remoteIqGrpcHandler
  :: (RemoteIQRequest -> IO RemoteIQResponse)
  -> GrpcS.SomeRpcHandler IO
remoteIqGrpcHandler handler =
  GrpcS.someRpcHandler $
    GrpcST.fromMethod @RemoteIqRpc $
      GrpcSB.mkNonStreaming handler
