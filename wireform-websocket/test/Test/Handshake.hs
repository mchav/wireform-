{-# LANGUAGE OverloadedStrings #-}

module Test.Handshake (tests) where

import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI

import Test.Tasty
import Test.Tasty.HUnit

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Method as M
import qualified Network.HTTP.Types.Version as V
import Network.HTTP.Types.Body (Body (..))
import Network.HTTP.Message

import Network.WebSocket.Handshake

tests :: TestTree
tests = testGroup "Handshake"
  [ rfcAcceptVector
  , acceptsValidRequest
  , rejectsMissingKey
  , clientHandshakeRoundTrip
  ]

-- The canonical RFC 6455 sec 1.3 / sec 4.2.2 vector:
--   key    = "dGhlIHNhbXBsZSBub25jZQ=="
--   accept = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
rfcAcceptVector :: TestTree
rfcAcceptVector = testCase "RFC 6455 sec 4.2.2 Sec-WebSocket-Accept vector" $
  computeAccept "dGhlIHNhbXBsZSBub25jZQ=="
    @?= "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="

acceptsValidRequest :: TestTree
acceptsValidRequest = testCase "parses a valid handshake request" $ do
  let req = mkReq
        [ (H.hHost,                "example.com")
        , (H.hUpgrade,             "websocket")
        , (H.hConnection,          "Upgrade")
        , (CI.mk "Sec-WebSocket-Key",     "dGhlIHNhbXBsZSBub25jZQ==")
        , (CI.mk "Sec-WebSocket-Version", "13")
        ]
  case parseWebSocketRequest req of
    Right ws -> wsReqKey ws @?= "dGhlIHNhbXBsZSBub25jZQ=="
    Left e   -> assertFailure ("rejected valid request: " <> show e)

rejectsMissingKey :: TestTree
rejectsMissingKey = testCase "rejects missing Sec-WebSocket-Key" $ do
  let req = mkReq
        [ (H.hHost,                "example.com")
        , (H.hUpgrade,             "websocket")
        , (H.hConnection,          "Upgrade")
        , (CI.mk "Sec-WebSocket-Version", "13")
        ]
  case parseWebSocketRequest req of
    Left _  -> pure ()
    Right _ -> assertFailure "accepted a request without Sec-WebSocket-Key"

clientHandshakeRoundTrip :: TestTree
clientHandshakeRoundTrip = testCase "client handshake -> server verifies" $ do
  let opts = (defaultWebSocketHandshakeOpts "/chat" "example.com")
        { wsOptProtocols = ["chat"] }
  (reqBytes, key) <- buildClientHandshake opts
  -- Sanity check: GET line is correct.
  assertBool "starts with GET /chat" (BS.isPrefixOf "GET /chat HTTP/1.1" reqBytes)
  -- Server replies with computed accept; client verifies.
  let acceptVal = computeAccept key
      hdrs = [ (H.hUpgrade,    "websocket")
             , (H.hConnection, "Upgrade")
             , (CI.mk "Sec-WebSocket-Accept", acceptVal)
             ]
  case verifyServerHandshake key 101 hdrs of
    Right () -> pure ()
    Left e   -> assertFailure ("client verify failed: " <> show e)

  -- Negative case: wrong accept value rejected.
  let hdrsBad = [ (H.hUpgrade,    "websocket")
                , (H.hConnection, "Upgrade")
                , (CI.mk "Sec-WebSocket-Accept", "wrong=")
                ]
  case verifyServerHandshake key 101 hdrsBad of
    Left _  -> pure ()
    Right _ -> assertFailure "accepted a bogus Sec-WebSocket-Accept"

mkReq :: H.Headers -> Request
mkReq hdrs = Request
  { requestMethod    = M.Method "GET"
  , requestTarget    = "/chat"
  , requestAuthority = lookup H.hHost hdrs
  , requestScheme    = SchemeHttp
  , requestHeaders   = hdrs
  , requestBody      = BodyEmpty
  , requestVersion   = V.HTTP1_1
  , requestTrailers  = pure []
  }
