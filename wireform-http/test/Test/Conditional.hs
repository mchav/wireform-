{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "Network.HTTP.Client.Conditional".
--
-- ETag and If-Match grammars are parsed and rendered by hermes;
-- these tests lock down the shape of what wireform users see.
module Test.Conditional (tests) where

import Test.Syd

import Network.HTTP.Client.Conditional

tests :: Spec
tests = describe "Network.HTTP.Client.Conditional" $ sequence_
  [ describe "ETag" $ sequence_
      [ it "strong tag round-trips through render/parse" $ do
          let t = strongETag "abc123"
          renderETag t `shouldBe` "\"abc123\""
          parseETag (renderETag t) `shouldBe` Just t
      , it "weak tag carries the W/ prefix" $
          renderETag (weakETag "v2") `shouldBe` "W/\"v2\""
      , it "parseETag rejects malformed input" $
          parseETag "not an etag" `shouldBe` Nothing
      ]
  , describe "If-Match" $ sequence_
      [ it "empty list collapses to wildcard" $
          ifMatchHeader [] `shouldBe` "*"
      , it "non-empty list comma-joins entity tags" $
          ifMatchHeader [strongETag "a", weakETag "b"]
            `shouldBe` "\"a\", W/\"b\""
      , it "If-None-Match wildcard" $
          ifNoneMatchHeader [] `shouldBe` "*"
      ]
  , describe "If-Modified-Since" $ sequence_
      [ it "renders an IMF-fixdate" $
          let bs = ifModifiedSinceHeader (read "1994-11-06 08:49:37 UTC")
          in bs `shouldBe` "Sun, 06 Nov 1994 08:49:37 GMT"
      ]
  , describe "Validator comparison (RFC 9110 §8.8.3)" $ sequence_
      [ it "strongMatch: identical strong tags match" $
          strongMatch (strongETag "abc") (strongETag "abc") `shouldBe` True
      , it "strongMatch: strong vs weak with same bytes does NOT match" $
          strongMatch (strongETag "abc") (weakETag "abc") `shouldBe` False
      , it "strongMatch: weak vs weak does NOT match" $
          strongMatch (weakETag "abc") (weakETag "abc") `shouldBe` False
      , it "strongMatch: different opaque bytes do not match" $
          strongMatch (strongETag "abc") (strongETag "def") `shouldBe` False
      , it "weakMatch: strong-strong matches when bytes agree" $
          weakMatch (strongETag "abc") (strongETag "abc") `shouldBe` True
      , it "weakMatch: strong-weak matches when bytes agree" $
          weakMatch (strongETag "abc") (weakETag "abc") `shouldBe` True
      , it "weakMatch: weak-weak matches when bytes agree" $
          weakMatch (weakETag "abc") (weakETag "abc") `shouldBe` True
      , it "weakMatch: different bytes do not match" $
          weakMatch (strongETag "abc") (weakETag "abd") `shouldBe` False
      ]
  ]
