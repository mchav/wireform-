{- | OpenTelemetry tracing middleware.

Implements 'withTracing', a 'Middleware' that wraps each
'Network.HTTP.Client.Send.send' call in an OTel client span with the
standard semantic-convention attributes and optional caller hooks.

Conventions:

* The span name defaults to @\"HTTP \" \<\> method@ (the OTel semconv
  default for HTTP client spans), and can be overridden.
* The span @kind@ is 'Client'.
* @http.request.method@, @url.full@, @server.address@,
  @server.port@, and @http.request.body.size@ (when known) are
  attached at span start.
* @http.response.status_code@ is attached when the response comes
  back.
* Request and response headers are __not__ captured by default — the
  OTel HTTP semantic conventions require operator-controlled
  allowlists, because headers can contain auth tokens, cookies, and
  PII. Set 'requestHeaderAllowlist' \/ 'responseHeaderAllowlist'
  to opt in for specific header names.
* The active 'Context' is injected into outgoing headers via the
  configured 'Propagator' on the 'TracerProvider'. By default that
  propagator emits W3C @traceparent@ \/ @tracestate@.
* If the response status is >= 500 the span status is set to
  'Error'; any exception thrown by the inner transport is recorded
  on the span via 'recordException' before being rethrown.

The instrumentation library name advertised to OTel is
@wireform-http@.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.Client.Tracing
  ( -- * Configuration
    TracingConfig (..)
  , TracingOptions (..)
  , defaultTracingConfig
  , defaultTracingOptions
    -- * Middleware
  , withTracing
    -- * Header capture
  , captureHeaders
    -- * Re-exports for hook callers
  , Trace.Span
  , Trace.Attribute
  , Trace.ToAttribute (..)
  , Trace.addAttribute
  , Trace.addAttributes
  , Trace.setStatus
  , Trace.SpanStatus (..)
  ) where

import Control.Exception (SomeException)
import Control.Monad (unless, when)
import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified UnliftIO.Exception as U

import qualified Network.HTTP.Types.Header as WH
import qualified Network.HTTP.Types.Method as WM
import qualified Network.HTTP.Types.Status as WS

import qualified OpenTelemetry.Attributes as Attr
import qualified OpenTelemetry.Context.ThreadLocal as Ctx
import qualified OpenTelemetry.Propagator as Prop
import qualified OpenTelemetry.Trace.Core as Trace

import Network.HTTP.Client.BodyStream
import Network.HTTP.Client.Protocol (ProtocolInfo (..))
import qualified Network.HTTP.Client.Request as Req
import Network.HTTP.Client.Response (RawResponse (..))
import Network.HTTP.Client.Response
import Network.HTTP.Client.Transport
import qualified Network.HTTP.Client.URI as WURI

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Top-level toggle. 'TracingEnabled' uses the supplied
-- 'TracingOptions'; 'TracingDisabled' is a no-op middleware.
data TracingConfig
  = TracingEnabled !TracingOptions
  | TracingDisabled

-- | The default: enabled, using the globally-configured
-- 'Trace.TracerProvider' and empty header allowlists.
defaultTracingConfig :: TracingConfig
defaultTracingConfig = TracingEnabled defaultTracingOptions

-- | Knobs for the tracing middleware. See 'defaultTracingOptions'.
data TracingOptions = TracingOptions
  { tracerProvider          :: !(Maybe Trace.TracerProvider)
    -- ^ When 'Nothing', the middleware reads
    --   'Trace.getGlobalTracerProvider' once at construction time.
  , requestHeaderAllowlist  :: !(HashSet WH.HeaderName)
    -- ^ Request header names to attach as
    --   @http.request.header.\<name\>@ attributes. Default: empty.
  , responseHeaderAllowlist :: !(HashSet WH.HeaderName)
    -- ^ Response header names to attach as
    --   @http.response.header.\<name\>@ attributes. Default: empty.
  , spanNameOverride        :: !(Maybe (Req.Request BodyStream -> Text))
    -- ^ Replace the default span name. The default is
    --   @\"HTTP \" \<\> method@.
  , requestHook             :: !(Trace.Span -> Req.Request BodyStream -> IO ())
    -- ^ Called inside the span just before the inner transport runs.
    --   Use this to attach call-site-specific attributes derived
    --   from the request.
  , responseHook            :: !(Trace.Span -> RawResponse -> IO ())
    -- ^ Called inside the span after a successful inner transport
    --   call, before the span ends.
  }

defaultTracingOptions :: TracingOptions
defaultTracingOptions = TracingOptions
  { tracerProvider          = Nothing
  , requestHeaderAllowlist  = HashSet.empty
  , responseHeaderAllowlist = HashSet.empty
  , spanNameOverride        = Nothing
  , requestHook             = \_ _ -> pure ()
  , responseHook            = \_ _ -> pure ()
  }

-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

-- | OTel client-span middleware.
--
-- The span is kept open until the response body popper hits EOF —
-- so streaming responses get accurate end-to-end timings without
-- closing the span the moment the headers come back. Non-streaming
-- responses end the span as soon as their popper is exhausted,
-- which is typically the next call after 'sendRaw'.
--
-- @
-- withClient defaultClientConfig
--   { ccExtra = [withTracing defaultTracingConfig] }
--   ...
-- @
withTracing :: TracingConfig -> Middleware IO
withTracing TracingDisabled        inner = inner
withTracing (TracingEnabled opts) inner = Transport $ \req -> do
  tp     <- maybe Trace.getGlobalTracerProvider pure (tracerProvider opts)
  let tracer = Trace.makeTracer tp instrumentationLib Trace.tracerOptions
      sname  = case spanNameOverride opts of
        Just f  -> f req
        Nothing -> "HTTP " <> T.pack (show (Req.method req))

  -- Open the span via the non-bracketed API so we can keep it alive
  -- past the 'sendRaw' return — the body popper closes it on EOF.
  ctx0  <- Ctx.getContext
  span_ <- Trace.createSpan tracer ctx0 sname (spanArguments opts req)

  -- Standard semconv attributes
  addRequestAttributes opts span_ req

  -- Caller hook
  requestHook opts span_ req

  -- Carry user-supplied per-request attributes through
  unless (null (Req.spanAttributes req)) $
    Trace.addAttributes span_
      (HashMap.fromList (map (\(k, v) -> (k, toOtelAttribute v))
                                        (Req.spanAttributes req)))

  -- Inject W3C trace context into outgoing headers via the
  -- TracerProvider's configured propagator.
  let propagator = Trace.getTracerProviderPropagators tp
  hdrs' <- Prop.inject propagator ctx0 (Req.headers req)
  let req' = req { Req.headers = hdrs' }

  -- Run the inner transport. If sendRaw itself throws (transport
  -- error, decode error, etc.) record the exception, end the span,
  -- and rethrow.
  raw <- sendRaw inner req' `U.withException` \(e :: SomeException) -> do
    Trace.recordException span_ mempty Nothing e
    Trace.setStatus span_ (Trace.Error (T.pack (show e)))
    Trace.endSpan span_ Nothing

  -- Response-side attributes
  addResponseAttributes opts span_ raw

  -- Span status per semconv: 5xx is Error, everything else
  -- inherits the default (Unset).
  when (WS.statusCode (statusCode raw) >= 500) $
    Trace.setStatus span_ (Trace.Error
      (T.pack (show (WS.statusCode (statusCode raw)))))

  responseHook opts span_ raw

  -- End the span when the body popper hits EOF. For materialised
  -- responses (where the popper yields the full body in one go,
  -- then EOF) this fires on the consumer's second read; for
  -- streaming responses it fires after the last chunk.
  popper' <- attachOnEOF (Trace.endSpan span_ Nothing) (bodyPopper raw)
  pure raw { bodyPopper = popper' }

instrumentationLib :: Trace.InstrumentationLibrary
instrumentationLib = Trace.InstrumentationLibrary
  { Trace.libraryName       = "wireform-http"
  , Trace.libraryVersion    = "0.1.0.0"
  , Trace.librarySchemaUrl  = ""
  , Trace.libraryAttributes = Attr.emptyAttributes
  }

spanArguments :: TracingOptions -> Req.Request BodyStream -> Trace.SpanArguments
spanArguments _opts _req = Trace.defaultSpanArguments
  { Trace.kind = Trace.Client
  }

addRequestAttributes
  :: TracingOptions
  -> Trace.Span
  -> Req.Request BodyStream
  -> IO ()
addRequestAttributes opts span_ req = do
  let methodTxt = TE.decodeUtf8 (WM.fromMethod (Req.method req))
      urlTxt    = WURI.requestURIToText (Req.requestURI req)
      mUri      = either (const Nothing) Just
                    (WURI.renderRequestURI (Req.requestURI req))
      base =
        [ ("http.request.method", Trace.toAttribute methodTxt)
        , ("url.full",            Trace.toAttribute urlTxt)
        ]
      hostPort = case mUri of
        Nothing -> []
        Just u  ->
          [ ("server.address", Trace.toAttribute (TE.decodeUtf8 (WURI.uriHost u)))
          , ("server.port",    Trace.toAttribute (WURI.uriPort u))
          ]
      bodySize = case knownSize (Req.body req) of
        Just n  -> [("http.request.body.size", Trace.toAttribute (fromIntegral n :: Int))]
        Nothing -> []
      hdrs = captureHeaders (requestHeaderAllowlist opts) "http.request.header"
                            (Req.headers req)
  Trace.addAttributes span_ (HashMap.fromList (base <> hostPort <> bodySize <> hdrs))

addResponseAttributes
  :: TracingOptions
  -> Trace.Span
  -> RawResponse
  -> IO ()
addResponseAttributes opts span_ raw = do
  let statusAttr =
        ( "http.response.status_code"
        , Trace.toAttribute (fromIntegral (WS.statusCode (statusCode raw)) :: Int)
        )
      hdrs = captureHeaders (responseHeaderAllowlist opts) "http.response.header"
                            (Network.HTTP.Client.Response.headers raw)
  Trace.addAttributes span_ (HashMap.fromList (statusAttr : hdrs))

-- ---------------------------------------------------------------------------
-- Header capture (semconv-compliant)
-- ---------------------------------------------------------------------------

-- | Build OTel-semconv-style header attributes from a header list,
-- filtered to a caller-supplied allowlist of header names.
--
-- Result keys are @\<prefix\>.\<lowercased-name\>@; values are lists
-- of the header values (RFC 9110 lets a header repeat, the OTel
-- semconv recommends a list attribute).
captureHeaders
  :: HashSet WH.HeaderName
  -> Text                   -- ^ attribute key prefix
                            --   (e.g. @"http.request.header"@)
  -> WH.Headers
  -> [(Text, Trace.Attribute)]
captureHeaders allowlist prefix hdrs =
  [ (prefix <> "." <> lowerName name, Trace.toAttribute values)
  | name   <- HashSet.toList allowlist
  , let values = map TE.decodeUtf8 (WH.lookupHeaders name hdrs)
  , not (null values)
  ]
  where
    lowerName = TE.decodeUtf8 . CI.foldedCase

-- ---------------------------------------------------------------------------
-- Internal conversions
-- ---------------------------------------------------------------------------

toOtelAttribute :: Req.SpanAttribute -> Trace.Attribute
toOtelAttribute = \case
  Req.AttrText   t -> Trace.toAttribute t
  Req.AttrBytes  b -> Trace.toAttribute (TE.decodeUtf8 b)
  Req.AttrInt    n -> Trace.toAttribute (fromIntegral n :: Int)
  Req.AttrDouble d -> Trace.toAttribute d
  Req.AttrBool   b -> Trace.toAttribute b
