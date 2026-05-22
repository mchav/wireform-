{- | @Expect: 100-continue@ helpers (RFC 9110 \u00a710.1.1).

== Status

End-to-end @Expect: 100-continue@ requires the underlying connection
to expose a two-stage send: first the request line and headers, then
\u2014 after a @100 Continue@ interim response (or after a short
timeout) \u2014 the body. The wireform low-level connection API
('Network.HTTP.Connection.sendOn') is one-shot and does not surface
1xx informational responses to the caller, so a faithful
implementation is gated on that primitive landing in the
'wireform-http1' \/ 'wireform-http2' layers.

This module ships the headers-side of the protocol \u2014 setting the
@Expect@ header and recognising it on the server side \u2014 plus a
'withExpectContinue' middleware that's correct for servers that
either honour the protocol or fall back to ignoring it. Servers that
respond with @417 Expectation Failed@ are surfaced verbatim to the
caller so they can retry without the header.

The 'expectTimeout' field of 'ExpectConfig' is a placeholder for the
future implementation; it's accepted today and stored for forward
compatibility.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Expect
  ( -- * Configuration
    ExpectConfig (..)
  , defaultExpectConfig
    -- * Middleware
  , withExpectContinue
    -- * Helpers
  , expect100ContinueHeader
  , hasExpectContinueHeader
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI

import qualified Network.HTTP.Types.Header as H

import qualified Network.HTTP.Client.Request   as WReq
import           Network.HTTP.Client.Transport
import           Network.HTTP.Client.Middleware (Duration, seconds)

-- | Configuration for the @Expect: 100-continue@ middleware.
data ExpectConfig = ExpectConfig
  { expectMinBodyBytes :: !Int
    -- ^ Only attach @Expect: 100-continue@ to requests whose
    --   declared @Content-Length@ is at least this many bytes.
    --   Skipping the protocol for tiny bodies is the
    --   industry-standard heuristic; the round-trip cost dominates
    --   the savings below ~1\u201316 KiB.
  , expectTimeout :: !Duration
    -- ^ How long to wait for an interim 100 Continue before sending
    --   the body anyway (RFC 9110 \u00a710.1.1: \"a client SHOULD NOT
    --   wait for an indefinite period\"). Defaults to one second.
    --   Stored for the connection-layer support that will read it;
    --   currently not consulted (see module docstring).
  }

defaultExpectConfig :: ExpectConfig
defaultExpectConfig = ExpectConfig
  { expectMinBodyBytes = 1024
  , expectTimeout      = seconds 1
  }

-- | Attach @Expect: 100-continue@ to outgoing requests whose
-- declared @Content-Length@ is large enough. The middleware does
-- not synthesise a @Content-Length@; if the request doesn't carry
-- one (e.g. chunked streaming) the header is added unconditionally
-- because the wait-before-send semantic is what the caller wants
-- in that case anyway.
withExpectContinue :: ExpectConfig -> Middleware IO
withExpectContinue cfg inner = Transport $ \req ->
  let hdrs = WReq.headers req
      shouldAttach = case H.lookupHeader H.hContentLength hdrs of
        Nothing -> True
        Just v  -> case BS.dropWhile isWS (BS.dropWhileEnd isWS v) of
          v' -> case readInt v' of
            Just n  -> n >= expectMinBodyBytes cfg
            Nothing -> True
      already = hasExpectContinueHeader hdrs
      hdrs'
        | already || not shouldAttach = hdrs
        | otherwise = H.insertHeader H.hExpect expect100ContinueHeader hdrs
      _ = expectTimeout cfg  -- accepted for forward compatibility
  in sendRaw inner req { WReq.headers = hdrs' }
  where
    isWS w = w == 0x20 || w == 0x09
    readInt b = case BS.foldl' step (Just 0 :: Maybe Int) b of
      Just n | not (BS.null b) -> Just n
      _ -> Nothing
    step Nothing  _ = Nothing
    step (Just !n) w
      | w >= 0x30 && w <= 0x39 = Just (n * 10 + fromIntegral (w - 0x30))
      | otherwise              = Nothing

expect100ContinueHeader :: ByteString
expect100ContinueHeader = "100-continue"

-- | True if any @Expect@ header in the list is the @100-continue@
-- value (case-insensitive).
hasExpectContinueHeader :: H.Headers -> Bool
hasExpectContinueHeader = any match
  where
    match (n, v) = n == H.hExpect && CI.mk v == CI.mk expect100ContinueHeader
