{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the IDN integration in "Network.HTTP.Client.URI".

The actual punycode work is provided by the @idn@ library; the
tests here lock down the bridge from that library through the
wireform host-validation surface.
-}
module Test.IDN (tests) where

import Network.HTTP.Client.URI
import Test.Syd


tests :: Spec
tests =
  describe "Network.HTTP.Client.URI IDN" $
    sequence_
      [ describe "validateHost" $
          sequence_
            [ it "ASCII host passes through unchanged" $
                validateHost "example.com" `shouldBe` Right "example.com"
            , it "IPv4 literal passes through" $
                validateHost "192.0.2.1" `shouldBe` Right "192.0.2.1"
            , it "IPv6 literal (no brackets, post-parseURI form)" $
                validateHost "::1" `shouldBe` Right "::1"
            , it "non-ASCII host gets IDNA A-label form" $
                -- München -> xn--mnchen-3ya
                validateHost "m\xc3\xbcnchen.de"
                  `shouldBe` Right "xn--mnchen-3ya.de"
            , it "Japanese host -> A-label" $
                validateHost "\xe6\x97\xa5\xe6\x9c\xac.example"
                  `shouldBe` Right "xn--wgv71a.example"
            ]
      , describe "parseURIIdna" $
          sequence_
            [ it "ASCII URL is structurally identical to parseURI" $
                fmap uriHost (parseURIIdna "https://example.com/foo")
                  `shouldBe` Right "example.com"
            , it "non-ASCII host is converted on parse" $
                fmap uriHost (parseURIIdna "https://m\xfcnchen.de/foo")
                  `shouldBe` Right "xn--mnchen-3ya.de"
            , it "non-ASCII path survives unchanged (only host is IDN-encoded)" $
                fmap uriPath (parseURIIdna "https://m\xfcnchen.de/foo")
                  `shouldBe` Right "/foo"
            ]
      ]
