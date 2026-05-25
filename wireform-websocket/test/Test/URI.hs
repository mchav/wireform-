{-# LANGUAGE OverloadedStrings #-}

module Test.URI (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Network.WebSocket.URI

tests :: TestTree
tests = testGroup "URI"
  [ parseHappyPath
  , defaultPorts
  , ipv6Literals
  , badCases
  , roundTrip
  ]

parseHappyPath :: TestTree
parseHappyPath = testGroup "parses canonical URIs"
  [ testCase "ws with port and path" $
      parseWebSocketURI "ws://example.com:8080/chat?room=42"
        @?= Right WebSocketURI
              { wsuScheme = WsScheme
              , wsuHost   = "example.com"
              , wsuPort   = 8080
              , wsuTarget = "/chat?room=42"
              }

  , testCase "wss with default port" $
      parseWebSocketURI "wss://example.com/"
        @?= Right WebSocketURI
              { wsuScheme = WssScheme
              , wsuHost   = "example.com"
              , wsuPort   = 443
              , wsuTarget = "/"
              }

  , testCase "ws with no path" $
      parseWebSocketURI "ws://example.com"
        @?= Right WebSocketURI
              { wsuScheme = WsScheme
              , wsuHost   = "example.com"
              , wsuPort   = 80
              , wsuTarget = "/"
              }

  , testCase "case-insensitive scheme" $
      parseWebSocketURI "WSS://Example.com/path"
        @?= Right WebSocketURI
              { wsuScheme = WssScheme
              , wsuHost   = "Example.com"
              , wsuPort   = 443
              , wsuTarget = "/path"
              }

  , testCase "fragment is dropped" $
      parseWebSocketURI "ws://example.com/path#hash"
        @?= Right WebSocketURI
              { wsuScheme = WsScheme
              , wsuHost   = "example.com"
              , wsuPort   = 80
              , wsuTarget = "/path"
              }
  ]

defaultPorts :: TestTree
defaultPorts = testCase "scheme picks the right default port" $ do
  fmap wsuPort (parseWebSocketURI "ws://example.com/")  @?= Right 80
  fmap wsuPort (parseWebSocketURI "wss://example.com/") @?= Right 443

ipv6Literals :: TestTree
ipv6Literals = testGroup "IPv6 literals"
  [ testCase "no port" $
      parseWebSocketURI "ws://[::1]/echo"
        @?= Right WebSocketURI
              { wsuScheme = WsScheme
              , wsuHost   = "::1"
              , wsuPort   = 80
              , wsuTarget = "/echo"
              }
  , testCase "with port" $
      parseWebSocketURI "wss://[2001:db8::1]:9443/echo"
        @?= Right WebSocketURI
              { wsuScheme = WssScheme
              , wsuHost   = "2001:db8::1"
              , wsuPort   = 9443
              , wsuTarget = "/echo"
              }
  ]

badCases :: TestTree
badCases = testGroup "rejects malformed URIs"
  [ testCase "http scheme" $ case parseWebSocketURI "http://example.com" of
      Left (URIBadScheme _) -> pure ()
      other -> assertFailure ("expected URIBadScheme, got " <> show other)
  , testCase "missing host" $ case parseWebSocketURI "ws://" of
      Left URIMissingHost -> pure ()
      other -> assertFailure ("expected URIMissingHost, got " <> show other)
  , testCase "non-numeric port" $ case parseWebSocketURI "ws://example.com:abc/" of
      Left (URIBadPort _) -> pure ()
      other -> assertFailure ("expected URIBadPort, got " <> show other)
  , testCase "port out of range" $ case parseWebSocketURI "ws://example.com:99999/" of
      Left (URIBadPort _) -> pure ()
      other -> assertFailure ("expected URIBadPort, got " <> show other)
  , testCase "unbalanced IPv6 brackets" $ case parseWebSocketURI "ws://[::1/" of
      Left (URIUnbalancedV6 _) -> pure ()
      other -> assertFailure ("expected URIUnbalancedV6, got " <> show other)
  ]

roundTrip :: TestTree
roundTrip = testGroup "parse . render = id"
  [ testCase "ws default port" $
      check (WebSocketURI WsScheme "example.com" 80 "/chat")
  , testCase "ws custom port" $
      check (WebSocketURI WsScheme "example.com" 8080 "/chat?x=1")
  , testCase "wss default port" $
      check (WebSocketURI WssScheme "example.com" 443 "/secure")
  , testCase "wss IPv6 + custom port" $
      check (WebSocketURI WssScheme "::1" 9443 "/echo")
  ]
  where
    check u = parseWebSocketURI (renderWebSocketURI u) @?= Right u
