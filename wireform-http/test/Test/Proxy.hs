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

import Test.Syd

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

tests :: Spec
tests = describe "Network.HTTP.Client.Proxy" $ sequence_
  [ describe "shouldBypass" $ sequence_
      [ it "exact host bypass" $
          P.shouldBypass cfg "localhost" `shouldBe` True
      , it "exact host bypass (IP)" $
          P.shouldBypass cfg "127.0.0.1" `shouldBe` True
      , it "subdomain-only pattern matches a subdomain" $
          P.shouldBypass cfg "host.internal" `shouldBe` True
      , it "subdomain-only pattern does NOT match the bare suffix" $
          P.shouldBypass cfg "internal" `shouldBe` False
      , it "literal pattern matches an exact host" $
          P.shouldBypass cfg "metrics.example.com" `shouldBe` True
      , it "literal pattern matches a subdomain too" $
          P.shouldBypass cfg "api.metrics.example.com" `shouldBe` True
      , it "different host is not bypassed" $
          P.shouldBypass cfg "api.example.com" `shouldBe` False
      ]
  , describe "proxy resolution" $ sequence_
      [ it "HTTP target uses proxyForHttp" $
          P.proxyForHttp cfg `shouldBe` Just proxyHttp
      , it "HTTPS target uses proxyForHttps" $
          P.proxyForHttps cfg `shouldBe` Just proxyHttps
      ]
  , describe "ProxyConfig defaults" $ sequence_
      [ it "noProxyConfig has no proxies set" $ do
          P.proxyForHttp  P.noProxyConfig `shouldBe` Nothing
          P.proxyForHttps P.noProxyConfig `shouldBe` Nothing
          P.proxyBypass P.noProxyConfig `shouldBe` []
      ]
  ]
