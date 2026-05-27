{-# LANGUAGE OverloadedStrings #-}
{- |
Tests for "Network.HTTP.Client.Proxy" — the routing decisions
(no full CONNECT-tunnel integration test; that needs a real
network listener and lives in 'Test.Http1Integration' once a
proxy harness is set up).
-}
module Test.Proxy (tests) where

import qualified Network.HTTP.Client.Proxy as P
import qualified Network.HTTP.Client.URI as WURI

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, (@?=))

proxyHttp :: P.Proxy
proxyHttp = P.Proxy
  { P.proxyScheme = WURI.SchemeHttp
  , P.proxyHost   = "proxy.internal"
  , P.proxyPort   = 3128
  }

proxyHttps :: P.Proxy
proxyHttps = P.Proxy
  { P.proxyScheme = WURI.SchemeHttps
  , P.proxyHost   = "secure-proxy.internal"
  , P.proxyPort   = 8443
  }

cfg :: P.ProxyConfig
cfg = P.ProxyConfig
  { P.proxyForHttp  = Just proxyHttp
  , P.proxyForHttps = Just proxyHttps
  , P.proxyBypass   =
      [ "localhost"
      , "127.0.0.1"
      , ".internal"          -- subdomain-only via leading dot
      , "metrics.example.com"
      ]
  }

tests :: TestTree
tests = testGroup "Network.HTTP.Client.Proxy"
  [ testGroup "shouldBypass"
      [ testCase "exact host bypass" $
          P.shouldBypass cfg "localhost" @?= True
      , testCase "exact host bypass (IP)" $
          P.shouldBypass cfg "127.0.0.1" @?= True
      , testCase "subdomain-only pattern matches a subdomain" $
          P.shouldBypass cfg "host.internal" @?= True
      , testCase "subdomain-only pattern does NOT match the bare suffix" $
          P.shouldBypass cfg "internal" @?= False
      , testCase "literal pattern matches an exact host" $
          P.shouldBypass cfg "metrics.example.com" @?= True
      , testCase "literal pattern matches a subdomain too" $
          P.shouldBypass cfg "api.metrics.example.com" @?= True
      , testCase "different host is not bypassed" $
          P.shouldBypass cfg "api.example.com" @?= False
      ]
  , testGroup "proxy resolution"
      [ testCase "HTTP target uses proxyForHttp" $
          P.proxyForHttp cfg @?= Just proxyHttp
      , testCase "HTTPS target uses proxyForHttps" $
          P.proxyForHttps cfg @?= Just proxyHttps
      ]
  , testGroup "ProxyConfig defaults"
      [ testCase "noProxyConfig has no proxies set" $ do
          P.proxyForHttp  P.noProxyConfig @?= Nothing
          P.proxyForHttps P.noProxyConfig @?= Nothing
          assertEqual "empty bypass list" [] (P.proxyBypass P.noProxyConfig)
      ]
  ]
