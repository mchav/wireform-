{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the IDN integration in "Network.HTTP.Client.URI".
--
-- The actual punycode work is provided by the @idn@ library; the
-- tests here lock down the bridge from that library through the
-- wireform host-validation surface.
module Test.IDN (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP.Client.URI

tests :: TestTree
tests = testGroup "Network.HTTP.Client.URI IDN"
  [ testGroup "validateHost"
      [ testCase "ASCII host passes through unchanged" $
          validateHost "example.com" @?= Right "example.com"
      , testCase "IPv4 literal passes through" $
          validateHost "192.0.2.1" @?= Right "192.0.2.1"
      , testCase "IPv6 literal (no brackets, post-parseURI form)" $
          validateHost "::1" @?= Right "::1"
      , testCase "non-ASCII host gets IDNA A-label form" $
          -- München -> xn--mnchen-3ya
          validateHost "m\xc3\xbcnchen.de"
            @?= Right "xn--mnchen-3ya.de"
      , testCase "Japanese host -> A-label" $
          validateHost "\xe6\x97\xa5\xe6\x9c\xac.example"
            @?= Right "xn--wgv71a.example"
      ]
  , testGroup "parseURIIdna"
      [ testCase "ASCII URL is structurally identical to parseURI" $
          fmap uriHost (parseURIIdna "https://example.com/foo")
            @?= Right "example.com"
      , testCase "non-ASCII host is converted on parse" $
          fmap uriHost (parseURIIdna "https://m\xfcnchen.de/foo")
            @?= Right "xn--mnchen-3ya.de"
      , testCase "non-ASCII path survives unchanged (only host is IDN-encoded)" $
          fmap uriPath (parseURIIdna "https://m\xfcnchen.de/foo")
            @?= Right "/foo"
      ]
  ]
