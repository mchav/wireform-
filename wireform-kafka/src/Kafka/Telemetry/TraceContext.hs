{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Telemetry.TraceContext
Description : W3C Trace Context parsing + rendering for distributed tracing

Implements <https://www.w3.org/TR/trace-context/ W3C Trace Context>
@traceparent@ + @tracestate@ header parse / render. This is the
on-the-wire substrate every modern OpenTelemetry / OpenTracing
exporter emits, so it's also what we propagate across producer →
consumer hops via Kafka record headers.

The module is deliberately /SDK-free/: parsing + rendering live
here so the rest of the client can do context propagation without
pulling in @hs-opentelemetry-api@. If you have a real tracing SDK
in scope, you can convert between its @SpanContext@ and ours via
the constructors / accessors below.

== @traceparent@ wire format (version 00)

@
00-{trace-id}-{parent-id}-{flags}
@

  * @version@: two lower-case hex digits — currently always
    @"00"@. Higher versions /must/ be ignored by parsers per spec
    §3.2.2.5; we surface them as 'TraceContextVersionUnsupported'.
  * @trace-id@: 32 lower-case hex digits (16 bytes). All-zeros is
    invalid per spec §3.2.2.3.
  * @parent-id@ (a.k.a. span-id): 16 lower-case hex digits (8
    bytes). All-zeros is invalid per spec §3.2.2.4.
  * @flags@: two lower-case hex digits encoding an 8-bit field.
    Bit 0 is the @sampled@ flag; bits 1-7 are reserved and
    /must/ be propagated unchanged per spec §3.3.

== @tracestate@ wire format

A comma-separated list of @key=value@ pairs, /up to 32 entries/
per spec §3.3.1.4. Keys + values are vendor-defined; we treat
them as opaque 'Text' here. Order matters — entries are
left-to-right oldest-vendor → newest-vendor.

== Round-tripping

@'parseTraceparent' . 'renderTraceparent' = Right@ for any
'SpanContext' built from valid components ('mkSpanContext'). The
inverse direction holds for any well-formed @traceparent@.

The 'Eq' / 'Show' instances on 'SpanContext' compare structural
content, not header-string identity — two contexts that print to
the same @traceparent@ + @tracestate@ pair compare equal even
when their 'TraceState' lists differ in /trailing whitespace
inside values/ (which we strip during 'parseTracestate').
-}
module Kafka.Telemetry.TraceContext
  ( -- * Types
    TraceId(..)
  , SpanId(..)
  , TraceFlags(..)
  , SpanContext(..)
  , TraceContextError(..)
    -- * Constants
  , currentVersion
  , flagSampled
  , maxTraceStateEntries
    -- * Construction
  , mkTraceId
  , mkSpanId
  , mkSpanContext
    -- * Predicates
  , isSampled
  , isValidTraceId
  , isValidSpanId
    -- * traceparent
  , parseTraceparent
  , renderTraceparent
    -- * tracestate
  , parseTracestate
  , renderTracestate
    -- * Convenience: Map<Text,Text> headers
  , injectIntoHeaders
  , extractFromHeaders
  , traceparentHeader
  , tracestateHeader
  ) where

import           Data.Bits       (testBit)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Char       (chr, ord)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Word       (Word8)

----------------------------------------------------------------------
-- Header names + spec constants
----------------------------------------------------------------------

-- | Wire name of the @traceparent@ header (lower-case per spec).
traceparentHeader :: Text
traceparentHeader = "traceparent"

-- | Wire name of the @tracestate@ header (lower-case per spec).
tracestateHeader :: Text
tracestateHeader = "tracestate"

-- | Currently supported @traceparent@ version. Future versions
-- have to opt-in via a new 'parseTraceparent' branch.
currentVersion :: Word8
currentVersion = 0x00

-- | Bit position of the @sampled@ flag within 'TraceFlags' (bit 0
-- per spec §3.3).
flagSampled :: Word8
flagSampled = 0x01

-- | Per spec §3.3.1.4: a parser /must/ accept up to 32
-- @tracestate@ entries; anything beyond that is dropped (we
-- truncate from the right, keeping the left-most / oldest
-- entries — which is what the spec recommends, though it
-- doesn't prescribe the truncation direction).
maxTraceStateEntries :: Int
maxTraceStateEntries = 32

----------------------------------------------------------------------
-- Types
----------------------------------------------------------------------

-- | 16-byte trace identifier. Compared / shown structurally.
newtype TraceId = TraceId { unTraceId :: ByteString }
  deriving (Eq, Ord)

instance Show TraceId where
  show = T.unpack . hexEncode . unTraceId

-- | 8-byte span identifier. Compared / shown structurally.
newtype SpanId = SpanId { unSpanId :: ByteString }
  deriving (Eq, Ord)

instance Show SpanId where
  show = T.unpack . hexEncode . unSpanId

-- | 8-bit trace-flags field. Bit 0 is @sampled@; bits 1–7 are
-- reserved by the spec and must be preserved on the wire.
newtype TraceFlags = TraceFlags { unTraceFlags :: Word8 }
  deriving (Eq, Ord, Show)

-- | A parsed (or freshly-constructed) trace context, ready to be
-- injected into outbound message headers or extracted from
-- inbound ones.
data SpanContext = SpanContext
  { spanContextTraceId    :: !TraceId
  , spanContextSpanId     :: !SpanId
  , spanContextTraceFlags :: !TraceFlags
  , spanContextTraceState :: ![(Text, Text)]
    -- ^ Vendor key=value pairs. /Order matters/ — left-most
    -- entries are oldest. Capped at 'maxTraceStateEntries'
    -- when injected.
  } deriving (Eq, Show)

-- | Reasons 'parseTraceparent' / 'parseTracestate' might reject
-- an input.
data TraceContextError
  = TraceContextWrongFieldCount    !Int
  | TraceContextVersionUnsupported !Word8
  | TraceContextInvalidVersion     !Text
  | TraceContextInvalidTraceId     !Text
  | TraceContextInvalidSpanId      !Text
  | TraceContextInvalidFlags       !Text
  | TraceContextZeroTraceId
  | TraceContextZeroSpanId
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Construction
----------------------------------------------------------------------

-- | Smart constructor: a 'TraceId' must be exactly 16 bytes and
-- not all-zeros.
mkTraceId :: ByteString -> Either TraceContextError TraceId
mkTraceId bs
  | BS.length bs /= 16 =
      Left (TraceContextInvalidTraceId
              (T.pack ("expected 16 bytes, got " <> show (BS.length bs))))
  | BS.all (== 0) bs   = Left TraceContextZeroTraceId
  | otherwise          = Right (TraceId bs)

-- | Smart constructor: a 'SpanId' must be exactly 8 bytes and
-- not all-zeros.
mkSpanId :: ByteString -> Either TraceContextError SpanId
mkSpanId bs
  | BS.length bs /= 8 =
      Left (TraceContextInvalidSpanId
              (T.pack ("expected 8 bytes, got " <> show (BS.length bs))))
  | BS.all (== 0) bs  = Left TraceContextZeroSpanId
  | otherwise         = Right (SpanId bs)

-- | Convenience constructor with sampled-flag wiring + tracestate
-- truncation to 'maxTraceStateEntries'.
mkSpanContext
  :: TraceId
  -> SpanId
  -> Bool                -- ^ Sampled?
  -> [(Text, Text)]      -- ^ tracestate entries (truncated)
  -> SpanContext
mkSpanContext tid sid sampled ts = SpanContext
  { spanContextTraceId    = tid
  , spanContextSpanId     = sid
  , spanContextTraceFlags = TraceFlags (if sampled then flagSampled else 0)
  , spanContextTraceState = take maxTraceStateEntries ts
  }

----------------------------------------------------------------------
-- Predicates
----------------------------------------------------------------------

-- | Is the @sampled@ bit set on the trace flags?
isSampled :: SpanContext -> Bool
isSampled = (`testBit` 0) . unTraceFlags . spanContextTraceFlags

-- | Quick check that a 'TraceId' satisfies the spec's structural
-- rules (length + non-zero). Always 'True' for an ID built via
-- 'mkTraceId'; useful to re-validate after manual construction.
isValidTraceId :: TraceId -> Bool
isValidTraceId (TraceId bs) = BS.length bs == 16 && not (BS.all (== 0) bs)

-- | Quick check that a 'SpanId' satisfies the spec's structural
-- rules (length + non-zero).
isValidSpanId :: SpanId -> Bool
isValidSpanId (SpanId bs) = BS.length bs == 8 && not (BS.all (== 0) bs)

----------------------------------------------------------------------
-- traceparent
----------------------------------------------------------------------

-- | Parse a @traceparent@ header value into a 'SpanContext'. The
-- 'spanContextTraceState' field is left empty — call
-- 'parseTracestate' separately and merge the result.
--
-- Validation follows W3C Trace Context §3.2.2 strictly: wrong
-- field count, unsupported version, all-zeros IDs, and
-- malformed-hex inputs all return 'Left'.
parseTraceparent
  :: Text
  -> Either TraceContextError SpanContext
parseTraceparent raw =
  case T.splitOn "-" raw of
    [verT, traceT, spanT, flagsT] -> do
      version <- parseVersion verT
      if version /= currentVersion
        then Left (TraceContextVersionUnsupported version)
        else do
          tid <- decodeTraceIdHex traceT
          sid <- decodeSpanIdHex  spanT
          flags <- parseFlags flagsT
          pure SpanContext
            { spanContextTraceId    = tid
            , spanContextSpanId     = sid
            , spanContextTraceFlags = flags
            , spanContextTraceState = []
            }
    parts ->
      Left (TraceContextWrongFieldCount (length parts))

-- | Render a 'SpanContext' back to its on-the-wire @traceparent@
-- form. Inverse of 'parseTraceparent' for any context built with
-- 'mkSpanContext'.
renderTraceparent :: SpanContext -> Text
renderTraceparent sc = T.intercalate "-"
  [ hexByte currentVersion
  , hexEncode (unTraceId (spanContextTraceId sc))
  , hexEncode (unSpanId  (spanContextSpanId  sc))
  , hexByte (unTraceFlags (spanContextTraceFlags sc))
  ]

----------------------------------------------------------------------
-- tracestate
----------------------------------------------------------------------

-- | Parse a @tracestate@ header value. Per spec §3.3.1.4 we
-- silently drop:
--
--   * empty entries (e.g. @"a=1,,b=2"@ → @[("a","1"),("b","2")]@),
--   * entries without a @"="@ separator,
--   * entries whose key or value would otherwise be empty,
--
-- and we cap the result at 'maxTraceStateEntries'. Whitespace
-- around keys + values is stripped (leading + trailing).
parseTracestate :: Text -> [(Text, Text)]
parseTracestate raw
  | T.null raw = []
  | otherwise  =
      let pieces = T.splitOn "," raw
          parsed = foldr step [] pieces
      in take maxTraceStateEntries parsed
  where
    step piece acc =
      case T.breakOn "=" (T.strip piece) of
        (k, eqRest)
          | T.null eqRest -> acc            -- no '='
          | otherwise     ->
              let v = T.strip (T.drop 1 eqRest)
              in if T.null k || T.null v
                   then acc
                   else (T.strip k, v) : acc

-- | Render a list of @tracestate@ entries to its on-the-wire
-- form. Empty input produces the empty string; the caller decides
-- whether to emit a header at all in that case.
renderTracestate :: [(Text, Text)] -> Text
renderTracestate =
  T.intercalate ","
    . map (\(k, v) -> k <> "=" <> v)
    . take maxTraceStateEntries

----------------------------------------------------------------------
-- Map<Text,Text> header convenience
----------------------------------------------------------------------

-- | Inject a 'SpanContext' into a 'Map' of headers (overwriting
-- any existing @traceparent@ / @tracestate@). Empty
-- 'spanContextTraceState' suppresses the @tracestate@ header
-- entirely, matching the spec's recommendation.
injectIntoHeaders
  :: SpanContext
  -> Map Text Text
  -> Map Text Text
injectIntoHeaders sc =
  let withParent = Map.insert traceparentHeader (renderTraceparent sc)
      withState =
        case spanContextTraceState sc of
          [] -> Map.delete tracestateHeader
          ts -> Map.insert tracestateHeader (renderTracestate ts)
  in withState . withParent

-- | Extract a 'SpanContext' from a 'Map' of headers, returning
-- 'Nothing' when no @traceparent@ is present and 'Left' when one
-- is present but malformed. The companion @tracestate@ is parsed
-- with 'parseTracestate'; if it's missing the result has an empty
-- 'spanContextTraceState'.
extractFromHeaders
  :: Map Text Text
  -> Maybe (Either TraceContextError SpanContext)
extractFromHeaders headers =
  case Map.lookup traceparentHeader headers of
    Nothing  -> Nothing
    Just raw ->
      case parseTraceparent raw of
        Left  err -> Just (Left err)
        Right sc  ->
          let ts = maybe [] parseTracestate (Map.lookup tracestateHeader headers)
          in Just (Right sc { spanContextTraceState = ts })

----------------------------------------------------------------------
-- Internal: hex / version / flags helpers
----------------------------------------------------------------------

parseVersion :: Text -> Either TraceContextError Word8
parseVersion t
  | T.length t /= 2 = Left (TraceContextInvalidVersion t)
  | otherwise       =
      case decodeHexBytes t of
        Just bs | BS.length bs == 1 -> Right (BS.head bs)
        _                           -> Left (TraceContextInvalidVersion t)

parseFlags :: Text -> Either TraceContextError TraceFlags
parseFlags t
  | T.length t /= 2 = Left (TraceContextInvalidFlags t)
  | otherwise       =
      case decodeHexBytes t of
        Just bs | BS.length bs == 1 -> Right (TraceFlags (BS.head bs))
        _                           -> Left (TraceContextInvalidFlags t)

decodeTraceIdHex :: Text -> Either TraceContextError TraceId
decodeTraceIdHex t
  | T.length t /= 32 =
      Left (TraceContextInvalidTraceId
              (T.pack ("expected 32 hex chars, got " <> show (T.length t))))
  | otherwise =
      case decodeHexBytes t of
        Just bs -> mkTraceId bs
        Nothing -> Left (TraceContextInvalidTraceId t)

decodeSpanIdHex :: Text -> Either TraceContextError SpanId
decodeSpanIdHex t
  | T.length t /= 16 =
      Left (TraceContextInvalidSpanId
              (T.pack ("expected 16 hex chars, got " <> show (T.length t))))
  | otherwise =
      case decodeHexBytes t of
        Just bs -> mkSpanId bs
        Nothing -> Left (TraceContextInvalidSpanId t)

----------------------------------------------------------------------
-- Internal: lower-case hex codec
--
-- We hand-roll instead of pulling in 'base16-bytestring' because
-- (a) the dependency is just for two tiny functions, and (b) the
-- spec mandates /lower-case/ hex on output and case-insensitive
-- input — base16-bytestring's policy isn't a perfect fit. The
-- inner loops are tight + non-allocating per byte.
----------------------------------------------------------------------

hexEncode :: ByteString -> Text
hexEncode = T.pack . concatMap byteToHex . BS.unpack
  where
    byteToHex b = [hexNibble (b `div` 16), hexNibble (b `mod` 16)]

hexByte :: Word8 -> Text
hexByte w = T.pack [hexNibble (w `div` 16), hexNibble (w `mod` 16)]

hexNibble :: Word8 -> Char
hexNibble n
  | n < 10    = chr (ord '0' + fromIntegral n)
  | otherwise = chr (ord 'a' + fromIntegral n - 10)

decodeHexBytes :: Text -> Maybe ByteString
decodeHexBytes t
  | odd (T.length t) = Nothing
  | otherwise        = BS.pack <$> goPairs (T.unpack t)
  where
    goPairs []           = Just []
    goPairs [_]          = Nothing
    goPairs (a : b : xs) = do
      hi <- hexDigit a
      lo <- hexDigit b
      rest <- goPairs xs
      Just (hi * 16 + lo : rest)

    hexDigit c
      | c >= '0' && c <= '9' = Just (fromIntegral (ord c - ord '0'))
      | c >= 'a' && c <= 'f' = Just (fromIntegral (ord c - ord 'a' + 10))
      | c >= 'A' && c <= 'F' = Just (fromIntegral (ord c - ord 'A' + 10))
      | otherwise            = Nothing
