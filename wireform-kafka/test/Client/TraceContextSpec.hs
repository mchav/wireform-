{-# LANGUAGE OverloadedStrings #-}

-- | Round-trip + edge-case tests for
-- 'Kafka.Telemetry.TraceContext'. The W3C Trace Context spec
-- (<https://www.w3.org/TR/trace-context/>) prescribes very
-- specific behaviour for malformed headers, all-zeros IDs, and
-- @tracestate@ entry truncation; this suite locks down the
-- critical paths so we don't drift away from the spec on
-- subsequent refactors.
module Client.TraceContextSpec (tests) where

import qualified Data.ByteString    as BS
import           Data.IORef
import           Data.List          (sortOn)
import qualified Data.Map.Strict    as Map
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           Test.Tasty          (TestTree, testGroup)
import           Test.Tasty.HUnit    (testCase, (@?=), assertBool, assertFailure)

import qualified Kafka.Telemetry.OpenTelemetry as OT
import qualified Kafka.Telemetry.TraceContext  as TC

tests :: TestTree
tests = testGroup "Telemetry: W3C Trace Context"
  [ testGroup "traceparent"
      [ testCase "parse + render round-trip a valid header"
          tp_round_trip
      , testCase "render uses lower-case hex"
          tp_lowercase
      , testCase "rejects all-zeros trace-id"
          tp_zero_traceid
      , testCase "rejects all-zeros span-id"
          tp_zero_spanid
      , testCase "rejects unsupported version"
          tp_bad_version
      , testCase "rejects wrong field count"
          tp_wrong_field_count
      , testCase "rejects malformed hex"
          tp_bad_hex
      , testCase "isSampled tracks bit 0 of the flags"
          tp_sampled_flag
      ]
  , testGroup "tracestate"
      [ testCase "parses simple key=value pairs"
          ts_basic
      , testCase "drops empty entries + entries without '='"
          ts_drops_garbage
      , testCase "trims whitespace around keys + values"
          ts_trims
      , testCase "respects 32-entry cap"
          ts_caps_at_32
      , testCase "render . parse is identity for clean input"
          ts_round_trip
      ]
  , testGroup "header injection"
      [ testCase "inject + extract round-trips a SpanContext"
          headers_round_trip
      , testCase "extract returns Nothing when no traceparent"
          headers_no_traceparent
      , testCase "extract surfaces parse errors"
          headers_bad_traceparent
      , testCase "empty tracestate suppresses the header"
          headers_empty_tracestate_no_header
      ]
  , testGroup "OpenTelemetry producer/consumer bridge"
      [ testCase "injectIntoProducerHeaders preserves unrelated headers"
          ot_inject_preserves
      , testCase "injectIntoProducerHeaders replaces existing trace headers"
          ot_inject_replaces
      , testCase "extractFromConsumerHeaders round-trips after inject"
          ot_round_trip
      , testCase "extractFromConsumerHeaders returns Nothing without traceparent"
          ot_extract_missing
      , testCase "tracingProducerInterceptor with Nothing pull is a no-op"
          ot_interceptor_noop
      , testCase "tracingProducerInterceptor injects when pull returns Just"
          ot_interceptor_injects
      ]
  ]

----------------------------------------------------------------------
-- traceparent
----------------------------------------------------------------------

-- A canonical W3C example from the spec, §3.2.2.5.
exampleTraceparent :: Text
exampleTraceparent =
  "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"

tp_round_trip :: IO ()
tp_round_trip =
  case TC.parseTraceparent exampleTraceparent of
    Left err -> assertFailure ("unexpected parse error: " ++ show err)
    Right sc -> TC.renderTraceparent sc @?= exampleTraceparent

tp_lowercase :: IO ()
tp_lowercase = do
  tid <- expectRight (TC.mkTraceId (BS.pack [0xAB, 0xCD, 0xEF, 0x01,
                                             0x02, 0x03, 0x04, 0x05,
                                             0x06, 0x07, 0x08, 0x09,
                                             0x0A, 0x0B, 0x0C, 0x0D]))
  sid <- expectRight (TC.mkSpanId  (BS.pack [0xFF, 0xEE, 0xDD, 0xCC,
                                             0xBB, 0xAA, 0x99, 0x88]))
  let sc       = TC.mkSpanContext tid sid True []
      rendered = TC.renderTraceparent sc
  rendered @?= "00-abcdef0102030405060708090a0b0c0d-ffeeddccbbaa9988-01"

tp_zero_traceid :: IO ()
tp_zero_traceid =
  case TC.parseTraceparent
         "00-00000000000000000000000000000000-b7ad6b7169203331-01" of
    Left TC.TraceContextZeroTraceId -> pure ()
    other -> assertFailure ("expected ZeroTraceId, got " ++ show other)

tp_zero_spanid :: IO ()
tp_zero_spanid =
  case TC.parseTraceparent
         "00-0af7651916cd43dd8448eb211c80319c-0000000000000000-01" of
    Left TC.TraceContextZeroSpanId -> pure ()
    other -> assertFailure ("expected ZeroSpanId, got " ++ show other)

tp_bad_version :: IO ()
tp_bad_version =
  case TC.parseTraceparent
         "ff-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01" of
    Left (TC.TraceContextVersionUnsupported 0xff) -> pure ()
    other -> assertFailure ("expected version-unsupported, got " ++ show other)

tp_wrong_field_count :: IO ()
tp_wrong_field_count =
  case TC.parseTraceparent "00-abc-def" of
    Left (TC.TraceContextWrongFieldCount 3) -> pure ()
    other -> assertFailure ("expected WrongFieldCount, got " ++ show other)

tp_bad_hex :: IO ()
tp_bad_hex =
  case TC.parseTraceparent
         "00-zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz-b7ad6b7169203331-01" of
    Left (TC.TraceContextInvalidTraceId _) -> pure ()
    other -> assertFailure ("expected InvalidTraceId, got " ++ show other)

tp_sampled_flag :: IO ()
tp_sampled_flag = do
  unsampled <- expectRight (TC.parseTraceparent
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-00")
  sampled   <- expectRight (TC.parseTraceparent
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TC.isSampled unsampled @?= False
  TC.isSampled sampled   @?= True

----------------------------------------------------------------------
-- tracestate
----------------------------------------------------------------------

ts_basic :: IO ()
ts_basic =
  TC.parseTracestate "vendor1=value1,vendor2=value2" @?=
    [("vendor1", "value1"), ("vendor2", "value2")]

ts_drops_garbage :: IO ()
ts_drops_garbage =
  TC.parseTracestate "a=1,,bogus,=empty,b=2,c=" @?=
    [("a", "1"), ("b", "2")]

ts_trims :: IO ()
ts_trims =
  TC.parseTracestate "  a = 1 ,  b = 2  " @?=
    [("a", "1"), ("b", "2")]

ts_caps_at_32 :: IO ()
ts_caps_at_32 = do
  let raw =
        T.intercalate ","
          [ T.pack ("k" <> show i <> "=v" <> show i)
          | i <- [0 :: Int .. 39]
          ]
      parsed = TC.parseTracestate raw
  length parsed @?= TC.maxTraceStateEntries

ts_round_trip :: IO ()
ts_round_trip =
  let entries = [("a", "1"), ("b", "2")]
  in TC.parseTracestate (TC.renderTracestate entries) @?= entries

----------------------------------------------------------------------
-- Header injection / extraction
----------------------------------------------------------------------

sampleSpanContext :: TC.SpanContext
sampleSpanContext =
  case TC.parseTraceparent exampleTraceparent of
    Right sc -> sc { TC.spanContextTraceState = [("rojo", "00f067aa0ba902b7")] }
    Left  _  -> error "sampleSpanContext: example must parse"

headers_round_trip :: IO ()
headers_round_trip = do
  let injected = TC.injectIntoHeaders sampleSpanContext Map.empty
  Map.lookup TC.traceparentHeader injected @?= Just exampleTraceparent
  Map.lookup TC.tracestateHeader  injected @?= Just "rojo=00f067aa0ba902b7"
  case TC.extractFromHeaders injected of
    Just (Right sc) -> sc @?= sampleSpanContext
    other           -> assertFailure ("expected Right, got " ++ show other)

headers_no_traceparent :: IO ()
headers_no_traceparent =
  case TC.extractFromHeaders Map.empty of
    Nothing -> pure ()
    other   -> assertFailure ("expected Nothing, got " ++ show other)

headers_bad_traceparent :: IO ()
headers_bad_traceparent =
  case TC.extractFromHeaders (Map.singleton TC.traceparentHeader "garbage") of
    Just (Left _) -> pure ()
    other -> assertFailure ("expected Left, got " ++ show other)

headers_empty_tracestate_no_header :: IO ()
headers_empty_tracestate_no_header =
  let sc       = sampleSpanContext { TC.spanContextTraceState = [] }
      injected = TC.injectIntoHeaders sc Map.empty
  in Map.lookup TC.tracestateHeader injected @?= Nothing

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

expectRight :: Show e => Either e a -> IO a
expectRight (Right a)  = pure a
expectRight (Left err) = do
  assertFailure ("unexpected Left: " ++ show err)
  error "unreachable"

----------------------------------------------------------------------
-- OpenTelemetry producer / consumer bridges
----------------------------------------------------------------------

userHeader :: (Text, BS.ByteString)
userHeader = ("x-user-id", TE.encodeUtf8 "alice")

stringHeader :: TC.SpanContext -> Text -> (Text, BS.ByteString)
stringHeader _ k = (k, TE.encodeUtf8 ("stale-" <> k))

ot_inject_preserves :: IO ()
ot_inject_preserves =
  let injected = OT.injectIntoProducerHeaders sampleSpanContext [userHeader]
  in lookup (fst userHeader) injected @?= Just (snd userHeader)

ot_inject_replaces :: IO ()
ot_inject_replaces = do
  let stale     = [ stringHeader sampleSpanContext TC.traceparentHeader
                  , stringHeader sampleSpanContext TC.tracestateHeader
                  , userHeader
                  ]
      injected  = OT.injectIntoProducerHeaders sampleSpanContext stale
      tpEntries = filter ((== TC.traceparentHeader) . fst) injected
      tsEntries = filter ((== TC.tracestateHeader)  . fst) injected
  -- Exactly one of each, and they're the freshly-injected values.
  length tpEntries @?= 1
  length tsEntries @?= 1
  fmap snd tpEntries @?=
    [TE.encodeUtf8 (TC.renderTraceparent sampleSpanContext)]
  fmap snd tsEntries @?=
    [TE.encodeUtf8 (TC.renderTracestate (TC.spanContextTraceState sampleSpanContext))]
  -- Unrelated header still there, unchanged.
  lookup (fst userHeader) injected @?= Just (snd userHeader)

ot_round_trip :: IO ()
ot_round_trip = do
  let injected = OT.injectIntoProducerHeaders sampleSpanContext [userHeader]
  case OT.extractFromConsumerHeaders injected of
    Just (Right sc) ->
      -- Compare structurally; the order of the trace-state list
      -- inside the SpanContext is preserved.
      ( TC.spanContextTraceId    sc
      , TC.spanContextSpanId     sc
      , TC.spanContextTraceFlags sc
      , sortOn fst (TC.spanContextTraceState sc)
      )
        @?=
      ( TC.spanContextTraceId    sampleSpanContext
      , TC.spanContextSpanId     sampleSpanContext
      , TC.spanContextTraceFlags sampleSpanContext
      , sortOn fst (TC.spanContextTraceState sampleSpanContext)
      )
    other -> assertFailure ("expected Right SpanContext, got " ++ show other)

ot_extract_missing :: IO ()
ot_extract_missing =
  case OT.extractFromConsumerHeaders [userHeader] of
    Nothing -> pure ()
    other   -> assertFailure ("expected Nothing, got " ++ show other)

ot_interceptor_noop :: IO ()
ot_interceptor_noop = do
  out <- OT.tracingProducerInterceptor (pure Nothing) [userHeader]
  out @?= [userHeader]

ot_interceptor_injects :: IO ()
ot_interceptor_injects = do
  -- Verify the pull is invoked exactly once per call.
  callCount <- newIORef (0 :: Int)
  let pull = do
        modifyIORef' callCount (+1)
        pure (Just sampleSpanContext)
  out <- OT.tracingProducerInterceptor pull [userHeader]
  count <- readIORef callCount
  count @?= 1
  -- traceparent injected.
  lookup TC.traceparentHeader out @?=
    Just (TE.encodeUtf8 (TC.renderTraceparent sampleSpanContext))
