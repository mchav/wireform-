{- | @Expect: 100-continue@ helpers (RFC 9110 §10.1.1).

== Two-stage send (§1.2 audit fix)

'withExpectContinue' sets the @Expect: 100-continue@ header on
qualifying requests.  The actual two-stage protocol — send headers
only, wait for @100 Continue@, then send the body — is implemented
at the connection layer in 'Network.HTTP.Connection.sendOn'.

When 'sendOn' detects an @Expect: 100-continue@ header it calls
'Network.HTTP1.Client.sendRequestOnWithExpect' which:

1. Sends the request line + headers (no body).
2. Waits up to 'expectTimeout' (default 1 s) for an interim
   response.
3. On @100 Continue@: sends the body and reads the final response.
4. On non-1xx (e.g. @417 Expectation Failed@): returns the
   server's response without sending the body.
5. On timeout: sends the body unconditionally (RFC 9110 §10.1.1
   permits a client to proceed after a reasonable timeout).

HTTP\/2 sends the body in DATA frames after HEADERS, and a
server-initiated @HEADERS@ with status @100@ on the same stream
signals continue; the wireform-http2 send path handles this
transparently.

The 'expectTimeout' field is forwarded to the connection layer
(currently wired as a constant 1 s; fine-grained per-request
timeout plumbing is a follow-up).
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
