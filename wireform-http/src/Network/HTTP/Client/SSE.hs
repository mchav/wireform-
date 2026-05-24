{- | Server-Sent Events (the WHATWG @EventSource@ wire format) for
the wireform HTTP client and server.

The on-wire format is text-based and stream-oriented: lines
terminated by CR, LF, or CRLF; @field: value@ pairs; blank lines
dispatch the accumulated fields as a single event. See
<https://html.spec.whatwg.org/multipage/server-sent-events.html>
for the normative grammar. This module implements that grammar as
an incremental parser that can be fed arbitrary chunk boundaries
(line splits, mid-CRLF, mid-field-name) without losing or
duplicating events.

The shapes:

* 'ServerSentEvent' is a dispatched event — what the spec calls a
  @MessageEvent@: a payload, an optional event type, and the
  @lastEventId@ that was in scope at dispatch time.
* 'SseFrame' adds the two side-channel directives the parser also
  surfaces: 'SseRetry' (the @retry:@ field's reconnection-time
  hint) and 'SseComment' (lines starting with @:@).
* 'SseParser' is the incremental parser state — opaque so the
  carry shape can change.

The two consumer popper helpers, 'sseFramePopper' and
'sseEventPopper', adapt a wire-level 'Popper' to a stream of
frames or just dispatched events respectively. They return
'Nothing' on EOF and never re-dispatch.

The 'EventStream' tag plugs SSE into the existing media-type
machinery: callers can decode a one-shot body with
@as \@EventStream \@[ServerSentEvent]@, or encode a list of
events as a server-side response body. For the (vastly more
common) streaming case, use 'withSSE' on the client side.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Network.HTTP.Client.SSE
  ( -- * Events and frames
    ServerSentEvent (..)
  , SseFrame (..)
    -- * Incremental parser
  , SseParser
  , newSseParser
  , feedSseParser
  , endSseParser
    -- * One-shot parsing
  , parseEventStream
  , parseEventStreamEvents
    -- * Popper integration
  , sseFramePopper
  , sseEventPopper
    -- * High-level client entry point
  , withSSE
  , withSSEFrames
  , SseError (..)
    -- * Server-side encoder
  , renderSseFrame
  , renderServerSentEvent
  , buildSseFrame
  , buildServerSentEvent
  , defaultSseEvent
    -- * Content-type tag
  , EventStream
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.IORef
import Data.Maybe (mapMaybe)

import qualified Wireform.Builder as WB
import Wireform.Builder (Builder)

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Status as S

import Network.HTTP.Client.Body (Body)
import Network.HTTP.Client.BodyStream (Popper)
import Network.HTTP.Client.Media
  ( Decode (..)
  , Encode (..)
  , HasMediaType (..)
  , MediaType (..)
  , Quality
  , contentTypeOf
  , maxQuality
  )
import Network.HTTP.Client.Request (Request, setHeader)
import Network.HTTP.Client.Response
  (RawResponse (bodyPopper, statusCode), headers)
import Network.HTTP.Client.Send (withResponse)
import Network.HTTP.Client.Transport (Transport)

-- ---------------------------------------------------------------------------
-- Public types
-- ---------------------------------------------------------------------------

-- | A dispatched server-sent event.
--
-- The @sseEventType@ field is 'Nothing' when no @event:@ line preceded
-- the dispatch — per the spec the implicit event type is then
-- @\"message\"@. We surface the explicit absence so callers can
-- distinguish \"the server told us this is a heartbeat\" from \"the
-- server said nothing\".
--
-- @sseEventId@ carries the last value set by an @id:@ field. The
-- EventSource spec keeps the id across events until explicitly
-- changed, so two consecutive dispatches with no intervening @id:@
-- line will see the same value here. This is also the value clients
-- echo back as @Last-Event-ID@ on reconnection.
data ServerSentEvent = ServerSentEvent
  { sseEventType :: !(Maybe ByteString)
  , sseEventId   :: !(Maybe ByteString)
  , sseData      :: !ByteString
  }
  deriving stock (Eq, Show)

-- | A single output of the parser. Most callers only care about
-- 'SseDispatch' and use 'sseEventPopper' to filter the rest out.
--
-- * 'SseDispatch' — a complete event ready for the application.
-- * 'SseRetry' — the server's @retry:@ directive in milliseconds.
--   Clients honoring reconnection backoff use this value as their
--   delay.
-- * 'SseComment' — a line that started with a literal @:@. Most
--   servers use this for keep-alives; surfacing it lets diagnostics
--   and proxies see the heartbeat without having to peek at the
--   wire.
data SseFrame
  = SseDispatch !ServerSentEvent
  | SseRetry !Int
  | SseComment !ByteString
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Parser state
-- ---------------------------------------------------------------------------

-- | The incremental parser. Opaque on purpose: the carry, BOM state,
-- and per-event buffers are implementation details we want freedom
-- to change.
data SseParser = SseParser
  { spCarry        :: !ByteString
    -- ^ Bytes seen since the last line terminator. Held as a
    --   strict 'ByteString' because lines in SSE are typically
    --   short; the worst case is one giant @data:@ line that we
    --   never finish, which is the caller's problem.
  , spJustSawCR    :: !Bool
    -- ^ Previous byte was @CR@. If the very next byte is @LF@ we
    --   fold the pair into one terminator (CRLF). This is the
    --   only piece of state that crosses chunk boundaries.
  , spEventType    :: !(Maybe ByteString)
  , spDataBuf      :: !Builder
    -- ^ Accumulated @data:@ values. Each appended chunk is
    --   @value <> \"\\n\"@. We materialise once at dispatch via
    --   'WB.toStrictByteString', which is O(total bytes) — using
    --   'Wireform.Builder' here keeps repeated @data:@ lines in
    --   one event from going quadratic the way @BS.append@ would.
  , spDataNonEmpty :: !Bool
    -- ^ Whether any @data:@ field at all was seen. The spec
    --   suppresses dispatch if the data buffer is empty; we track
    --   the predicate directly because @data:@ with an empty value
    --   still appends @\"\\n\"@ (so the bytes-buffer is non-empty
    --   even when functionally empty).
  , spLastEventId  :: !(Maybe ByteString)
  , spBomChecked   :: !Bool
    -- ^ Whether the optional leading UTF-8 BOM has been consumed.
  }

-- | Initial parser state. No carry, no buffered fields, BOM not yet
-- considered.
newSseParser :: SseParser
newSseParser = SseParser
  { spCarry        = BS.empty
  , spJustSawCR    = False
  , spEventType    = Nothing
  , spDataBuf      = mempty
  , spDataNonEmpty = False
  , spLastEventId  = Nothing
  , spBomChecked   = False
  }

-- | Feed a chunk through the parser. Returns the updated state and
-- any frames the chunk completed (in stream order).
--
-- The function is total and never throws — even malformed lines just
-- get ignored per spec.
feedSseParser :: SseParser -> ByteString -> (SseParser, [SseFrame])
feedSseParser p0 chunk0 =
  let (p1, chunk)                  = stripBomIfNeeded p0 chunk0
      (lines_, carry', sawCR')     = splitLines (spCarry p1) (spJustSawCR p1) chunk
      pAfter                       = p1 { spCarry = carry', spJustSawCR = sawCR' }
      (pFinal, framesRev)          = foldLines pAfter [] lines_
  in (pFinal, reverse framesRev)

-- | Signal EOF to the parser. Per the EventSource spec, a partial
-- event without its terminating blank line is /not/ dispatched at
-- EOF; this function returns the parser with its in-flight buffers
-- intact and no frames. It's exposed so callers building their own
-- popper wrapper have a place to plumb \"the stream ended\" through.
endSseParser :: SseParser -> (SseParser, [SseFrame])
endSseParser p = (p, [])

-- ---------------------------------------------------------------------------
-- One-shot helpers
-- ---------------------------------------------------------------------------

-- | Parse a complete event-stream body into frames in one shot.
--
-- Convenience wrapper for tests and recorded fixtures. For live
-- streams use 'sseFramePopper' (the parser is incremental and
-- chunk-safe; the one-shot form just feeds the whole buffer at
-- once).
parseEventStream :: ByteString -> [SseFrame]
parseEventStream bs = snd (feedSseParser newSseParser bs)

-- | 'parseEventStream' filtered to dispatched events only.
parseEventStreamEvents :: ByteString -> [ServerSentEvent]
parseEventStreamEvents = mapMaybe pickDispatch . parseEventStream
  where
    pickDispatch (SseDispatch ev) = Just ev
    pickDispatch _                = Nothing

-- ---------------------------------------------------------------------------
-- Popper integration
-- ---------------------------------------------------------------------------

-- | Wrap a wire-level 'Popper' as a frame popper.
--
-- The returned action pulls additional bytes from the underlying
-- popper as needed to surface the next frame. It returns
-- 'Nothing' on EOF and continues to return 'Nothing' on every
-- subsequent call.
sseFramePopper :: Popper -> IO (IO (Maybe SseFrame))
sseFramePopper popper = do
  -- State: parser + a small queue of frames already extracted but
  -- not yet returned. We need the queue because one chunk can
  -- produce more than one frame (consider a server flushing many
  -- buffered events into a single TCP segment).
  ref <- newIORef (newSseParser, [] :: [SseFrame], False)
  let loop = do
        (p, queued, eofSeen) <- readIORef ref
        case queued of
          (f : rest) -> do
            writeIORef ref (p, rest, eofSeen)
            pure (Just f)
          [] ->
            if eofSeen
              then pure Nothing
              else do
                chunk <- popper
                if BS.null chunk
                  then do
                    writeIORef ref (p, [], True)
                    pure Nothing
                  else do
                    let (p', newFrames) = feedSseParser p chunk
                    case newFrames of
                      (f : rest) -> do
                        writeIORef ref (p', rest, False)
                        pure (Just f)
                      [] -> do
                        writeIORef ref (p', [], False)
                        loop
  pure loop

-- | Like 'sseFramePopper' but filters to dispatched events,
-- silently swallowing comments and retry directives. This is what
-- most application code wants; reach for the frame popper if you
-- need to honour reconnection backoff or trace heartbeats.
sseEventPopper :: Popper -> IO (IO (Maybe ServerSentEvent))
sseEventPopper popper = do
  framePopper <- sseFramePopper popper
  let loop = do
        mf <- framePopper
        case mf of
          Nothing                -> pure Nothing
          Just (SseDispatch ev)  -> pure (Just ev)
          Just _                 -> loop
  pure loop

-- ---------------------------------------------------------------------------
-- High-level client helpers
-- ---------------------------------------------------------------------------

-- | Open an SSE stream against a transport.
--
-- Adds @Accept: text\/event-stream@ and @Cache-Control: no-store@
-- to the outgoing request (both per the EventSource spec). The
-- callback receives a popper of dispatched events; the underlying
-- connection is owned for the lifetime of the callback by whatever
-- 'Transport' was passed in (use 'Network.HTTP.Client.Streaming.streamedTransport'
-- for connection-per-request lifetime).
--
-- Throws 'SseError' if the response status is non-2xx or the
-- response @Content-Type@ isn't @text\/event-stream@.
withSSE
  :: forall m body a. (MonadUnliftIO m, Body body)
  => Transport IO
  -> Request body
  -> (IO (Maybe ServerSentEvent) -> m a)
  -> m a
withSSE t req k =
  withSSEFrames t req $ \framePopper -> k (filterFrames framePopper)
  where
    filterFrames fp = do
      mf <- fp
      case mf of
        Nothing               -> pure Nothing
        Just (SseDispatch ev) -> pure (Just ev)
        Just _                -> filterFrames fp

-- | Same as 'withSSE' but the callback sees every frame the parser
-- surfaces, including 'SseRetry' and 'SseComment'. Use this if you
-- need to react to server-controlled reconnection delays or want
-- to observe keep-alive heartbeats explicitly.
withSSEFrames
  :: forall m body a. (MonadUnliftIO m, Body body)
  => Transport IO
  -> Request body
  -> (IO (Maybe SseFrame) -> m a)
  -> m a
withSSEFrames t req k =
  withResponse t (sseRequestHeaders req) acceptList $ \raw -> do
    liftIO (assertSseResponse raw)
    framePopper <- liftIO (sseFramePopper (bodyPopper raw))
    k framePopper
  where
    acceptList :: [(MediaType, Quality)]
    acceptList = [(mediaType @EventStream, maxQuality)]

sseRequestHeaders :: Request body -> Request body
sseRequestHeaders =
    setHeader H.hAccept       "text/event-stream"
  . setHeader H.hCacheControl "no-store"

-- | Surface of failure modes for 'withSSE' \/ 'withSSEFrames'.
data SseError
  = SseUnexpectedStatus !S.Status
  | SseUnexpectedContentType !MediaType
  deriving stock (Show)

instance Exception SseError

assertSseResponse :: RawResponse -> IO ()
assertSseResponse raw = do
  let s    = statusCode raw
      code = S.statusCode s
  unless (code >= 200 && code < 300) $
    throwIO (SseUnexpectedStatus s)
  let ct = contentTypeOf (headers raw)
  unless (mtType ct == "text" && mtSubType ct == "event-stream") $
    throwIO (SseUnexpectedContentType ct)

-- ---------------------------------------------------------------------------
-- Content-type tag and instances
-- ---------------------------------------------------------------------------

-- | Phantom tag for @text\/event-stream@. Provides
-- 'HasMediaType' so it slots into the generic media negotiation
-- pipeline, plus 'Encode' \/ 'Decode' for the one-shot list form
-- (useful when fixtures pre-record a full stream and want to feed
-- it through 'send').
data EventStream

instance HasMediaType EventStream where
  mediaType = "text/event-stream"

instance Decode EventStream [ServerSentEvent] where
  decode = Right . parseEventStreamEvents

instance Decode EventStream [SseFrame] where
  decode = Right . parseEventStream

instance Encode EventStream [ServerSentEvent] where
  encode = WB.toStrictByteString . foldMap buildServerSentEvent

instance Encode EventStream [SseFrame] where
  encode = WB.toStrictByteString . foldMap buildSseFrame

-- ---------------------------------------------------------------------------
-- Server-side rendering
-- ---------------------------------------------------------------------------

-- | An empty 'ServerSentEvent' with default values. Useful as a base
-- for record-update syntax.
defaultSseEvent :: ServerSentEvent
defaultSseEvent = ServerSentEvent
  { sseEventType = Nothing
  , sseEventId   = Nothing
  , sseData      = BS.empty
  }

-- | Render a single dispatched event to the wire. Each line of
-- 'sseData' becomes a separate @data:@ line; the final blank line
-- terminating the event is included. The data payload must not
-- contain @CR@ (the spec disallows it inside data lines and a
-- naive render would produce two events at the consumer);
-- this function does /not/ check.
renderServerSentEvent :: ServerSentEvent -> ByteString
renderServerSentEvent = WB.toStrictByteString . buildServerSentEvent

-- | Render any single frame. 'SseDispatch' delegates to
-- 'renderServerSentEvent'; 'SseRetry' becomes a single @retry:@
-- line followed by the trailing blank; 'SseComment' becomes a
-- @:@-prefixed line followed by the trailing blank.
renderSseFrame :: SseFrame -> ByteString
renderSseFrame = WB.toStrictByteString . buildSseFrame

-- | 'Wireform.Builder' versions of the renderers. The byte-returning
-- 'renderServerSentEvent' \/ 'renderSseFrame' just materialise these,
-- but composing many frames stays O(n) when callers stay in
-- @Builder@-land and only collapse with 'WB.toStrictByteString' (or
-- 'WB.hPutBuilder') once at the end.
buildServerSentEvent :: ServerSentEvent -> Builder
buildServerSentEvent ev =
  optField "event: " (sseEventType ev)
    <> optField "id: "    (sseEventId   ev)
    <> buildDataPayload (sseData ev)
    <> WB.word8 0x0A
  where
    optField _    Nothing  = mempty
    optField name (Just v) = WB.byteString name <> WB.byteString v <> WB.word8 0x0A

buildSseFrame :: SseFrame -> Builder
buildSseFrame (SseDispatch ev) = buildServerSentEvent ev
buildSseFrame (SseRetry n)     =
  WB.byteString "retry: " <> WB.intDec n <> WB.byteString "\n\n"
buildSseFrame (SseComment c)   =
  WB.word8 0x3A <> WB.byteString c <> WB.byteString "\n\n"

-- | One @data: <line>\\n@ per LF-separated chunk in @d@. We split
-- /once/ here and feed the resulting slices straight into the
-- builder; each slice is O(1) (strict ByteString slices share the
-- underlying ForeignPtr), so no per-line copy happens.
buildDataPayload :: ByteString -> Builder
buildDataPayload d
  | BS.null d = mempty
  | otherwise = foldMap oneLine (BS.split 0x0A d)
  where
    oneLine l = WB.byteString "data: " <> WB.byteString l <> WB.word8 0x0A

-- ---------------------------------------------------------------------------
-- Internal: BOM handling
-- ---------------------------------------------------------------------------

-- | If the first three bytes of the very first chunk are the UTF-8
-- BOM (EF BB BF), drop them. The BOM check fires once per parser;
-- a partial BOM that arrives split across chunks still works,
-- because we only mark the BOM as handled once at least one
-- non-BOM byte has been seen (carrying any partial prefix
-- forward).
stripBomIfNeeded :: SseParser -> ByteString -> (SseParser, ByteString)
stripBomIfNeeded p chunk
  | spBomChecked p = (p, chunk)
  | otherwise =
      let combined = spCarry p <> chunk
          len = BS.length combined
      in if len < 3 && combined `BS.isPrefixOf` bom
           then -- entirely a BOM prefix so far: keep it in carry, do
                -- not flip the BOM flag yet.
                (p { spCarry = combined }, BS.empty)
           else if bom `BS.isPrefixOf` combined
             then (p { spBomChecked = True, spCarry = BS.empty }
                  , BS.drop 3 combined)
             else (p { spBomChecked = True, spCarry = BS.empty }
                  , combined)
  where
    bom :: ByteString
    bom = BS.pack [0xEF, 0xBB, 0xBF]

-- ---------------------------------------------------------------------------
-- Internal: line splitting
-- ---------------------------------------------------------------------------

-- | Split a chunk into completed lines + leftover carry, honouring
-- CR \/ LF \/ CRLF line terminators and the CRLF-folding state
-- carried in from the previous chunk.
--
-- The returned line list is in stream order. The boolean is the
-- new \"just saw CR\" flag.
splitLines
  :: ByteString          -- ^ carry from previous chunk
  -> Bool                -- ^ whether the previous chunk ended on a bare CR
  -> ByteString          -- ^ new chunk
  -> ([ByteString], ByteString, Bool)
splitLines carry0 sawCR0 input
  | BS.null input = ([], carry0, sawCR0)
  | otherwise = go [] carry0 sawCR0 0
  where
    n = BS.length input
    -- Find the next index >= i that is CR or LF, plus whether it
    -- was a CR.
    nextTerm i = case BS.findIndex isTerm (BS.drop i input) of
      Nothing -> Nothing
      Just j  ->
        let idx = i + j
            isCr = BS.index input idx == 0x0D
        in Just (idx, isCr)
    isTerm b = b == 0x0D || b == 0x0A
    go framesRev carry sawCR i
      | i >= n = (reverse framesRev, carry, sawCR)
      | sawCR && BS.index input i == 0x0A =
          -- LF after CR is the second half of a CRLF; skip it
          -- without emitting a (second) empty line.
          go framesRev carry False (i + 1)
      | otherwise = case nextTerm i of
          Nothing ->
            let tail_ = BS.drop i input
                carry' = if BS.null carry then tail_ else carry <> tail_
            in (reverse framesRev, carry', False)
          Just (termIdx, isCR) ->
            let pre  = BS.take (termIdx - i) (BS.drop i input)
                line = if BS.null carry then pre else carry <> pre
            in go (line : framesRev) BS.empty isCR (termIdx + 1)

-- ---------------------------------------------------------------------------
-- Internal: line processing
-- ---------------------------------------------------------------------------

-- | Fold a batch of completed lines through the parser, accumulating
-- emitted frames in reverse order.
foldLines :: SseParser -> [SseFrame] -> [ByteString] -> (SseParser, [SseFrame])
foldLines p acc []       = (p, acc)
foldLines p acc (l : ls) =
  let (p', emitted) = processLine p l
      acc'          = prependReverse emitted acc
  in foldLines p' acc' ls

-- | @prependReverse xs acc@ pushes @xs@ onto @acc@ keeping the
-- per-line emission order. We push @x1, x2, x3@ as
-- @x3 : x2 : x1 : acc@ so the final 'reverse' gives the right
-- order.
prependReverse :: [a] -> [a] -> [a]
prependReverse []       acc = acc
prependReverse (x : xs) acc = prependReverse xs (x : acc)

-- | Classify a completed line and apply it to the parser state,
-- returning any frames that result.
processLine :: SseParser -> ByteString -> (SseParser, [SseFrame])
processLine p line
  -- Blank line: dispatch any in-flight event.
  | BS.null line = dispatch p
  -- Comment line (literal ':' first). Spec doesn't strip a
  -- leading space; we mirror that exactly so callers see the raw
  -- payload.
  | BS.head line == 0x3A =
      (p, [SseComment (BS.tail line)])
  | otherwise = case BS.elemIndex 0x3A line of
      Nothing ->
        -- No colon: per spec the whole line is the field name
        -- and the value is the empty string.
        applyField p line BS.empty
      Just i ->
        let name  = BS.take i line
            after = BS.drop (i + 1) line
            value = stripOneLeadingSpace after
        in applyField p name value

stripOneLeadingSpace :: ByteString -> ByteString
stripOneLeadingSpace bs = case BS.uncons bs of
  Just (0x20, rest) -> rest
  _                 -> bs

applyField
  :: SseParser
  -> ByteString    -- ^ field name (case-sensitive per spec)
  -> ByteString    -- ^ field value (already left-trimmed of a
                   --   single space)
  -> (SseParser, [SseFrame])
applyField p name value = case name of
  "event" ->
    let v = if BS.null value then Nothing else Just value
    in (p { spEventType = v }, [])
  "data" ->
    ( p { spDataBuf      = spDataBuf p <> WB.byteString value <> WB.word8 0x0A
        , spDataNonEmpty = True
        }
    , []
    )
  "id" ->
    -- Spec: ignore the field entirely if the value contains a NUL.
    if 0x00 `BS.elem` value
      then (p, [])
      else (p { spLastEventId = Just value }, [])
  "retry" -> case parseAsciiDigits value of
    Just n  -> (p, [SseRetry n])
    Nothing -> (p, [])
  _ -> (p, [])

dispatch :: SseParser -> (SseParser, [SseFrame])
dispatch p
  -- No @data:@ field at all: the spec says to drop the in-flight
  -- event /and/ clear the event-type buffer, but preserve
  -- lastEventId.
  | not (spDataNonEmpty p) =
      ( p { spEventType = Nothing
          , spDataBuf   = mempty
          }
      , []
      )
  | otherwise =
      let raw      = WB.toStrictByteString (spDataBuf p)
          payload  = stripTrailingLF raw
          ev       = ServerSentEvent
            { sseEventType = spEventType p
            , sseEventId   = spLastEventId p
            , sseData      = payload
            }
          p'       = p
            { spEventType    = Nothing
            , spDataBuf      = mempty
            , spDataNonEmpty = False
            }
      in (p', [SseDispatch ev])

stripTrailingLF :: ByteString -> ByteString
stripTrailingLF bs = case BS.unsnoc bs of
  Just (rest, 0x0A) -> rest
  _                 -> bs

-- | Parse a byte string of ASCII digits (0..9) into a non-negative
-- 'Int'. Returns 'Nothing' for an empty string or any non-digit
-- byte. The EventSource spec demands this exact predicate for the
-- @retry:@ field.
parseAsciiDigits :: ByteString -> Maybe Int
parseAsciiDigits bs
  | BS.null bs = Nothing
  | otherwise  = go 0 0
  where
    n = BS.length bs
    go !acc !i
      | i >= n    = Just acc
      | otherwise =
          let !b = BS.index bs i
          in if b >= 0x30 && b <= 0x39
               then go (acc * 10 + fromIntegral (b - 0x30)) (i + 1)
               else Nothing
