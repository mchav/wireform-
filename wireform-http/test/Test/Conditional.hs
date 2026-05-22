{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "Network.HTTP.Client.Conditional".
--
-- ETag and If-Match grammars are parsed and rendered by hermes;
-- these tests lock down the shape of what wireform users see.
module Test.Conditional (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP.Client.Conditional

tests :: TestTree
tests = testGroup "Network.HTTP.Client.Conditional"
  [ testGroup "ETag"
      [ testCase "strong tag round-trips through render/parse" $ do
          let t = strongETag "abc123"
          renderETag t @?= "\"abc123\""
          parseETag (renderETag t) @?= Just t
      , testCase "weak tag carries the W/ prefix" $
          renderETag (weakETag "v2") @?= "W/\"v2\""
      , testCase "parseETag rejects malformed input" $
          parseETag "not an etag" @?= Nothing
      ]
  , testGroup "If-Match"
      [ testCase "empty list collapses to wildcard" $
          ifMatchHeader [] @?= "*"
      , testCase "non-empty list comma-joins entity tags" $
          ifMatchHeader [strongETag "a", weakETag "b"]
            @?= "\"a\", W/\"b\""
      , testCase "If-None-Match wildcard" $
          ifNoneMatchHeader [] @?= "*"
      ]
  , testGroup "If-Modified-Since"
      [ testCase "renders an IMF-fixdate" $
          let bs = ifModifiedSinceHeader (read "1994-11-06 08:49:37 UTC")
          in bs @?= "Sun, 06 Nov 1994 08:49:37 GMT"
      ]
  ]
