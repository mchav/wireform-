{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

-- | OpenTelemetry instrumentation for gRPC servers
--
-- This module provides a tracer-agnostic interface for instrumenting gRPC
-- servers with OpenTelemetry spans following the
-- <https://opentelemetry.io/docs/specs/semconv/rpc/grpc/ OTel gRPC semantic conventions>.
--
-- No specific OTel SDK is required; users provide a 'GrpcTracer'
-- implementation backed by their preferred SDK (e.g. @hs-opentelemetry-sdk@),
-- a custom tracer, or the included 'noopTracer'.
--
-- Typical usage:
--
-- > let params' = otelServerParams myTracer def
-- > server <- mkGrpcServer params' handlers
module Network.GRPC.Server.Otel (
    -- * Tracer interface
    GrpcTracer(..)
  , SpanContext(..)
  , SpanStatus(..)
  , AttributeValue(..)
  , SpanAttributes(..)
    -- * No-op tracer
  , noopTracer
    -- * Server middleware
  , otelServerMiddleware
  , otelServerParams
  ) where

import Control.Exception (SomeException, catch, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS.Char8
import Data.IORef
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Encoding
import Network.HTTP.Semantics qualified as HTTP.Semantics
import Network.HTTP.Semantics.Server qualified as Server

import Network.GRPC.Server.Context (ServerParams(..))
import Network.GRPC.Server.RequestHandler.API (RequestHandler)

{-------------------------------------------------------------------------------
  Tracer interface
-------------------------------------------------------------------------------}

-- | Opaque span context, carrying tracer-specific state.
--
-- Uses an existential wrapper so that each 'GrpcTracer' implementation can
-- store whatever internal state it needs.
data SpanContext = forall s. SpanContext (IORef s)

-- | Status of a completed span.
data SpanStatus
  = SpanOk
  | SpanError Text
  deriving stock (Show, Eq)

-- | Typed attribute values following OTel conventions.
data AttributeValue
  = AVText   Text
  | AVInt    Int64
  | AVBool   Bool
  | AVDouble Double
  deriving stock (Show, Eq)

-- | A bag of key-value attributes attached to a span at creation time.
newtype SpanAttributes = SpanAttributes [(Text, AttributeValue)]
  deriving stock (Show, Eq)

-- | Minimal tracer interface.
--
-- Users provide an implementation backed by their preferred OTel SDK
-- (@hs-opentelemetry-sdk@, etc.), a custom tracer, or 'noopTracer'.
data GrpcTracer = GrpcTracer
  { tracerStartSpan      :: Text -> SpanAttributes -> IO SpanContext
  , tracerEndSpan        :: SpanContext -> SpanStatus -> IO ()
  , tracerAddEvent       :: SpanContext -> Text -> IO ()
  , tracerSetAttribute   :: SpanContext -> Text -> AttributeValue -> IO ()
  , tracerInjectHeaders  :: SpanContext -> IO [(ByteString, ByteString)]
  , tracerExtractContext :: [(ByteString, ByteString)] -> IO (Maybe SpanContext)
  }

{-------------------------------------------------------------------------------
  No-op tracer
-------------------------------------------------------------------------------}

-- | A tracer that does nothing. Zero overhead when tracing is disabled.
noopTracer :: GrpcTracer
noopTracer = GrpcTracer
  { tracerStartSpan      = \_ _ -> SpanContext <$> newIORef ()
  , tracerEndSpan        = \_ _ -> return ()
  , tracerAddEvent       = \_ _ -> return ()
  , tracerSetAttribute   = \_ _ _ -> return ()
  , tracerInjectHeaders  = \_ -> return []
  , tracerExtractContext = \_ -> return Nothing
  }

{-------------------------------------------------------------------------------
  Server middleware
-------------------------------------------------------------------------------}

-- | OTel middleware that wraps every RPC in a span following the
-- <https://opentelemetry.io/docs/specs/semconv/rpc/grpc/ gRPC semantic conventions>.
--
-- Span name: @\{service\}\/\{method\}@ (from the request path)
--
-- Attributes set at span start:
--
-- * @rpc.system@   = @\"grpc\"@
-- * @rpc.service@  = the gRPC service name
-- * @rpc.method@   = the gRPC method name
--
-- After the handler completes:
--
-- * @rpc.grpc.status_code@ = @0@ (OK) or @2@ (UNKNOWN) for unhandled errors
-- * Span status is set via 'tracerEndSpan'
--
-- The middleware calls 'tracerEndSpan' in a @finally@ block so spans are
-- always closed, even on exceptions. It also attempts to extract a parent
-- context from request headers (W3C @traceparent@).
otelServerMiddleware :: GrpcTracer -> RequestHandler a -> RequestHandler a
otelServerMiddleware tracer handler unmask req respond = do
    let (service, method) = parsePathFromRequest req
        spanName = service <> "/" <> method
        attrs = SpanAttributes
          [ ("rpc.system" , AVText "grpc")
          , ("rpc.service", AVText service)
          , ("rpc.method" , AVText method)
          ]

    _parentCtx <- tracerExtractContext tracer (extractHeaders req)
    spanCtx    <- tracerStartSpan tracer spanName attrs

    result <- handler unmask req respond `catch` \(exc :: SomeException) -> do
      tracerSetAttribute tracer spanCtx "rpc.grpc.status_code" (AVInt 2)
      tracerEndSpan tracer spanCtx (SpanError (textShow exc))
      throwIO exc

    tracerSetAttribute tracer spanCtx "rpc.grpc.status_code" (AVInt 0)
    tracerEndSpan tracer spanCtx SpanOk
    return result

-- | Convenience function to install OTel middleware into 'ServerParams'.
--
-- Composes the OTel middleware with any existing 'serverTopLevel' wrapper,
-- so that the OTel span is the outermost layer.
otelServerParams :: GrpcTracer -> ServerParams -> ServerParams
otelServerParams tracer params = params
  { serverTopLevel = \h -> otelServerMiddleware tracer (serverTopLevel params h)
  }

{-------------------------------------------------------------------------------
  Internal helpers
-------------------------------------------------------------------------------}

-- | Extract the gRPC service and method names from the request path.
--
-- The gRPC path format is @\/{service}\/{method}@. If parsing fails we
-- return placeholder values so the middleware never crashes.
parsePathFromRequest :: Server.Request -> (Text, Text)
parsePathFromRequest req =
    case Server.requestPath req of
      Nothing   -> ("<unknown>", "<unknown>")
      Just path ->
        case BS.Char8.split '/' path of
          ["", service, method] ->
            ( Text.Encoding.decodeUtf8Lenient service
            , Text.Encoding.decodeUtf8Lenient method
            )
          _ -> (Text.Encoding.decodeUtf8Lenient path, "<unknown>")

-- | Pull raw headers from an http-semantics 'Server.Request' as
-- key-value pairs suitable for trace context extraction.
extractHeaders :: Server.Request -> [(ByteString, ByteString)]
extractHeaders req =
    map (\(tok, val) -> (HTTP.Semantics.tokenCIKey tok, val))
      . fst
      $ Server.requestHeaders req

textShow :: Show a => a -> Text
textShow = Text.Encoding.decodeUtf8Lenient . BS.Char8.pack . show
