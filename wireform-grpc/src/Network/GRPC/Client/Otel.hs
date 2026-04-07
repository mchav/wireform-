{-# LANGUAGE OverloadedStrings #-}

-- | OpenTelemetry instrumentation for gRPC clients
--
-- This module provides client-side OTel instrumentation that wraps RPC calls
-- in spans following the
-- <https://opentelemetry.io/docs/specs/semconv/rpc/grpc/ OTel gRPC semantic conventions>.
--
-- Typical usage:
--
-- > withTracedRPC tracer conn callParams (Proxy @MyRpc) $ \call -> do
-- >   sendFinalInput call myInput
-- >   fst <$> recvFinalOutput call
--
-- __Trace context propagation:__ The W3C @traceparent@ header cannot be
-- injected into gRPC request headers from outside the core library (the
-- trace context field is set internally by @startRPC@). For full distributed
-- tracing propagation, use 'tracerInjectHeaders' from 'GrpcTracer' and
-- arrange for the resulting headers to be included at the transport level,
-- or use an SDK that hooks into the HTTP\/2 layer directly.
module Network.GRPC.Client.Otel (
    -- * Traced RPC calls
    withTracedRPC
    -- * Re-exports from "Network.GRPC.Server.Otel"
  , GrpcTracer(..)
  , SpanContext(..)
  , SpanStatus(..)
  , AttributeValue(..)
  , SpanAttributes(..)
  , noopTracer
  ) where

import Control.Exception (SomeException, throwIO)
import Data.ByteString.Char8 qualified as BS.Char8
import Data.Proxy (Proxy)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Control.Monad.Catch (MonadMask, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Stack (HasCallStack)

import Network.GRPC.Client.Call (Call, withRPC)
import Network.GRPC.Client.Connection (Connection)
import Network.GRPC.Server.Otel
    ( GrpcTracer(..)
    , SpanContext(..)
    , SpanStatus(..)
    , AttributeValue(..)
    , SpanAttributes(..)
    , noopTracer
    )
import Network.GRPC.Spec (CallParams, IsRPC(..), SupportsClientRpc)

{-------------------------------------------------------------------------------
  Traced RPC calls
-------------------------------------------------------------------------------}

-- | Like 'withRPC', but wraps the call in an OTel client span.
--
-- Creates a span named @\{service\}\/\{method\}@ with the standard gRPC
-- semantic convention attributes (@rpc.system@, @rpc.service@, @rpc.method@).
-- On success, sets @rpc.grpc.status_code@ to @0@ (OK). On exception, sets
-- it to @2@ (UNKNOWN) and records the error before re-raising.
--
-- The span is always closed via 'tracerEndSpan', even on exceptions.
withTracedRPC :: forall rpc m a.
     (MonadMask m, MonadIO m, SupportsClientRpc rpc, HasCallStack)
  => GrpcTracer
  -> Connection
  -> CallParams rpc
  -> Proxy rpc
  -> (Call rpc -> m a)
  -> m a
withTracedRPC tracer conn callParams proxy k = do
    let service = Text.Encoding.decodeUtf8Lenient $ rpcServiceName proxy
        method  = Text.Encoding.decodeUtf8Lenient $ rpcMethodName proxy
        spanName = service <> "/" <> method
        attrs = SpanAttributes
          [ ("rpc.system" , AVText "grpc")
          , ("rpc.service", AVText service)
          , ("rpc.method" , AVText method)
          ]

    spanCtx <- liftIO $ tracerStartSpan tracer spanName attrs

    withRPC conn callParams proxy $ \call -> do
      result <- k call `catchM` \(exc :: SomeException) -> liftIO $ do
        tracerSetAttribute tracer spanCtx "rpc.grpc.status_code" (AVInt 2)
        tracerEndSpan tracer spanCtx (SpanError (textShow exc))
        throwIO exc

      liftIO $ do
        tracerSetAttribute tracer spanCtx "rpc.grpc.status_code" (AVInt 0)
        tracerEndSpan tracer spanCtx SpanOk

      return result

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

-- | 'catch' lifted to 'MonadIO' + 'MonadMask'
catchM ::
     MonadMask m
  => m a -> (SomeException -> m a) -> m a
catchM = catch

textShow :: Show a => a -> Text
textShow = Text.Encoding.decodeUtf8Lenient . BS.Char8.pack . show
