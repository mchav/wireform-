{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Tests for the reconnecting EventSource client added to
"Network.HTTP.Client.SSE": 'withReconnectingSSE',
'ReconnectPolicy', and the per-attempt 'Last-Event-ID' header.

The harness is a mock 'Transport' that returns a different
canned response per call so we can simulate "first attempt drops
mid-stream, second attempt resumes from the recorded id".
-}
module Test.SSEReconnect (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef
import Network.HTTP.Client.BodyStream qualified as BSm
import Network.HTTP.Client.Protocol (ProtocolInfo (..))
import Network.HTTP.Client.Request (Request, get)
import Network.HTTP.Client.Request qualified as WReq
import Network.HTTP.Client.Response (RawResponse (..))
import Network.HTTP.Client.Response qualified as Resp
import Network.HTTP.Client.SSE
import Network.HTTP.Client.Send (prepareRequest)
import Network.HTTP.Client.Transport
import Network.HTTP.Client.URI qualified as WURI
import Network.HTTP.Types.Header qualified as H
import Network.HTTP.Types.Status qualified as S
import Test.Syd


-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

mkRequest :: IO (Request BSm.BodyStream)
mkRequest = case WURI.parseTemplate "http://example.com/sse" of
  Left e -> error (show e)
  Right t -> prepareRequest [] (get t)


eventStreamHeaders :: [(H.HeaderName, H.HeaderValue)]
eventStreamHeaders = [(H.hContentType, "text/event-stream")]


{- | A canned-responses transport that also records every
request's @Last-Event-ID@ header (if any) for assertions.
-}
mkTransport
  :: [(S.Status, [(H.HeaderName, H.HeaderValue)], ByteString)]
  -> IO (Transport IO, IORef [Maybe ByteString])
mkTransport script = do
  scriptRef <- newIORef script
  idsRef <- newIORef ([] :: [Maybe ByteString])
  let t = Transport $ \req -> do
        atomicModifyIORef' idsRef $ \xs ->
          (H.lookupHeader H.hLastEventID (WReq.headers req) : xs, ())
        next <- atomicModifyIORef' scriptRef $ \s -> case s of
          (x : rest) -> (rest, x)
          [] -> ([], (S.status204, eventStreamHeaders, ""))
        let (st, hdrs, body) = next
        popper <- BSm.popperFromStrict body
        pure
          RawResponse
            { Resp.statusCode = st
            , Resp.headers = hdrs
            , Resp.bodyPopper = popper
            , Resp.protocolInfo = HTTP1_1
            }
  pure (t, idsRef)


drainAt :: IO (Maybe ServerSentEvent) -> Int -> IO [ServerSentEvent]
drainAt pop n = go (0 :: Int) []
  where
    go i acc
      | i >= n = pure (reverse acc)
      | otherwise = do
          mev <- pop
          case mev of
            Nothing -> pure (reverse acc)
            Just ev -> go (i + 1) (ev : acc)


-- ---------------------------------------------------------------------------
-- The reconnect loop
-- ---------------------------------------------------------------------------

{- | First connection delivers 2 events with ids \"1\" and \"2\",
then EOF; second connection delivers 1 more event with id
\"3\" then EOF; third connection returns @204 No Content@
which terminates the reconnect loop.
-}
sseScript :: [(S.Status, [(H.HeaderName, H.HeaderValue)], ByteString)]
sseScript =
  [
    ( S.status200
    , eventStreamHeaders
    , "id: 1\ndata: hello\n\nid: 2\ndata: world\n\n"
    )
  ,
    ( S.status200
    , eventStreamHeaders
    , "id: 3\ndata: again\n\n"
    )
  , (S.status204, eventStreamHeaders, "")
  ]


unit_resumes_with_last_event_id :: Spec
unit_resumes_with_last_event_id = it
  "reconnect attaches Last-Event-ID from the most recent id"
  $ do
    (t, idsRef) <- mkTransport sseScript
    req <- mkRequest
    let policy = defaultReconnectPolicy {rpInitialRetryMs = 1}
    evs <- withReconnectingSSE t req policy $ \pop -> drainAt pop 5
    (length evs) `shouldBe` 3
    (sseEventId (evs !! 0)) `shouldBe` (Just "1")
    (sseEventId (evs !! 1)) `shouldBe` (Just "2")
    (sseEventId (evs !! 2)) `shouldBe` (Just "3")
    -- Three connection attempts: first carries no Last-Event-ID;
    -- second carries "2" (the most recent id from connection 1);
    -- third carries "3".
    ids <- reverse <$> readIORef idsRef
    (length ids) `shouldBe` 3
    (ids !! 0) `shouldBe` Nothing
    (ids !! 1) `shouldBe` (Just "2")
    (ids !! 2) `shouldBe` (Just "3")


unit_204_stops :: Spec
unit_204_stops = it
  "204 No Content from the first attempt stops reconnecting"
  $ do
    (t, idsRef) <-
      mkTransport
        [(S.status204, eventStreamHeaders, "")]
    req <- mkRequest
    let policy = defaultReconnectPolicy {rpInitialRetryMs = 1}
    _ <- withReconnectingSSE t req policy $ \pop -> drainAt pop 5
    ids <- readIORef idsRef
    -- We don't try a reconnect after a 204.
    (length ids) `shouldBe` 1


unit_max_attempts :: Spec
unit_max_attempts = it
  "rpMaxAttempts stops the loop after N consecutive empty attempts"
  $ do
    -- Every attempt returns an immediate-EOF stream (no events),
    -- so failCount climbs by 1 per attempt.
    (t, idsRef) <-
      mkTransport
        [(S.status200, eventStreamHeaders, "") | _ <- [(1 :: Int) .. 10]]
    req <- mkRequest
    let policy =
          defaultReconnectPolicy
            { rpInitialRetryMs = 1
            , rpMaxAttempts = Just 3
            }
    _ <- withReconnectingSSE t req policy $ \pop -> drainAt pop 1
    -- The consumer's popper only sees Nothing once the worker
    -- signals stop — by which time the worker has already
    -- incremented idsRef for every attempt it made.
    ids <- readIORef idsRef
    (if (length ids <= 3) then pure () else expectationFailure ("attempts " <> show (length ids) <> " <= 3"))


-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: Spec
tests =
  describe "Network.HTTP.Client.SSE.Reconnect" $
    sequence_
      [ unit_resumes_with_last_event_id
      , unit_204_stops
      , unit_max_attempts
      ]
