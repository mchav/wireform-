{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.PerMessageDeflate (tests) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Test.Tasty
import Test.Tasty.HUnit

import Network.WebSocket.Connection.Role
import Network.WebSocket.PerMessageDeflate

tests :: TestTree
tests = testGroup "PerMessageDeflate"
  [ testGroup "offer parsing"
      [ testCase "vanilla permessage-deflate" $ do
          let r = parseOffers ["permessage-deflate"]
          length r @?= 1
          offClientMaxWindowBitsHint (head r) @?= ClientWindowBitsAbsent
      , testCase "all four parameters" $ do
          let r = parseOffers
                [ "permessage-deflate; client_no_context_takeover; \
                  \server_no_context_takeover; server_max_window_bits=14; \
                  \client_max_window_bits=12"
                ]
          length r @?= 1
          let o = head r
          offClientNoContextTakeover o @?= True
          offServerNoContextTakeover o @?= True
          offServerMaxWindowBits     o @?= Just 14
          offClientMaxWindowBitsHint o @?= ClientWindowBitsSet 12
      , testCase "bare client_max_window_bits is treated as a hint" $ do
          let r = parseOffers ["permessage-deflate; client_max_window_bits"]
          length r @?= 1
          offClientMaxWindowBitsHint (head r) @?= ClientWindowBitsHinted
      , testCase "non-PMD extensions are ignored" $ do
          let r = parseOffers ["x-foo; bar", "permessage-deflate"]
          length r @?= 1
      , testCase "rejects window_bits out of range" $ do
          let r = parseOffers ["permessage-deflate; server_max_window_bits=16"]
          r @?= []
      ]

  , testGroup "selectOffer"
      [ testCase "default server policy + default offer picks 15/15" $ do
          let Just p = selectOffer defaultPmdParams [defaultPmdOffer]
          pmdServerMaxWindowBits p @?= 15
          pmdClientMaxWindowBits p @?= 15
      , testCase "client_no_context_takeover propagates" $ do
          let off = defaultPmdOffer { offClientNoContextTakeover = True }
              Just p = selectOffer defaultPmdParams [off]
          pmdClientNoContextTakeover p @?= True
      , testCase "server policy can demand no_context_takeover" $ do
          let pol = defaultPmdParams { pmdServerNoContextTakeover = True }
              Just p = selectOffer pol [defaultPmdOffer]
          pmdServerNoContextTakeover p @?= True
      , testCase "client window bits ceiling clamps to server policy" $ do
          let off = defaultPmdOffer { offClientMaxWindowBitsHint = ClientWindowBitsSet 14 }
              pol = defaultPmdParams { pmdClientMaxWindowBits = 12 }
              Just p = selectOffer pol [off]
          pmdClientMaxWindowBits p @?= 12
      ]

  , testGroup "response header"
      [ testCase "round-trip default params" $ do
          let p = defaultPmdParams
          parseResponseParams (responseHeader p) @?= Just p
      , testCase "round-trip with smaller server window" $ do
          let p = defaultPmdParams { pmdServerMaxWindowBits = 12 }
          parseResponseParams (responseHeader p) @?= Just p
      , testCase "round-trip with both no_context_takeover flags" $ do
          let p = defaultPmdParams
                { pmdServerNoContextTakeover = True
                , pmdClientNoContextTakeover = True
                }
          parseResponseParams (responseHeader p) @?= Just p
      ]

  , testGroup "compress / decompress round-trip"
      [ testCase "ascii small" $ roundTrip defaultPmdParams
          "the quick brown fox jumps over the lazy dog"
      , testCase "highly compressible repetition" $ roundTrip defaultPmdParams
          (BS.replicate 16384 (fromIntegral (fromEnum 'A')))
      , testCase "incompressible (random-ish)" $ roundTrip defaultPmdParams $
          BS.pack (cycle [0, 1, 2, 3, 4, 5, 6, 7] `takeBytes` 4096)
      , testCase "many small messages with context takeover" $
          manyMessages defaultPmdParams 50
      , testCase "many small messages with no_context_takeover" $
          manyMessages defaultPmdParams
            { pmdServerNoContextTakeover = True
            , pmdClientNoContextTakeover = True
            }
            50
      , testCase "smaller window bits still round-trips" $ roundTrip
          defaultPmdParams { pmdServerMaxWindowBits = 9, pmdClientMaxWindowBits = 9 }
          "smaller window bits force more frequent dictionary reset"
      ]
  ]

-- Test that one Server-side compress + Client-side decompress
-- (and vice-versa) recovers the input.  Uses a pair of contexts:
-- the server's deflate / client's inflate handle one direction,
-- the client's deflate / server's inflate handle the other.
roundTrip :: PmdParams -> BS.ByteString -> Assertion
roundTrip params input =
  bracket (newPmdContext Server params) freePmdContext $ \server ->
    bracket (newPmdContext Client params) freePmdContext $ \client -> do
      -- server -> client direction
      compressed <- compressMessage server input
      pmdMaybeReset server Outbound
      inflated   <- decompressMessage client compressed
      pmdMaybeReset client Inbound
      inflated @?= input
      -- client -> server direction
      compressed2 <- compressMessage client input
      pmdMaybeReset client Outbound
      inflated2   <- decompressMessage server compressed2
      pmdMaybeReset server Inbound
      inflated2 @?= input

-- Drive n round-trips on the same pair of contexts to exercise the
-- dictionary-takeover path.
manyMessages :: PmdParams -> Int -> Assertion
manyMessages params n =
  bracket (newPmdContext Server params) freePmdContext $ \server ->
    bracket (newPmdContext Client params) freePmdContext $ \client ->
      mapM_ (oneRound server client) [1 .. n]
  where
    oneRound server client i = do
      let msg = BS8.pack ("hello there number " <> show i)
      compressed <- compressMessage server msg
      pmdMaybeReset server Outbound
      inflated   <- decompressMessage client compressed
      pmdMaybeReset client Inbound
      inflated @?= msg

takeBytes :: [a] -> Int -> [a]
takeBytes = flip take
