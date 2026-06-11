{-# LANGUAGE OverloadedStrings #-}

module Test.URI (tests) where

import Network.WebSocket.URI
import Test.Syd


tests :: Spec
tests =
  describe "URI" $
    sequence_
      [ parseHappyPath
      , defaultPorts
      , ipv6Literals
      , badCases
      , roundTrip
      ]


parseHappyPath :: Spec
parseHappyPath =
  describe "parses canonical URIs" $
    sequence_
      [ it "ws with port and path" $
          parseWebSocketURI "ws://example.com:8080/chat?room=42"
            `shouldBe` Right
              WebSocketURI
                { wsuScheme = WsScheme
                , wsuHost = "example.com"
                , wsuPort = 8080
                , wsuTarget = "/chat?room=42"
                }
      , it "wss with default port" $
          parseWebSocketURI "wss://example.com/"
            `shouldBe` Right
              WebSocketURI
                { wsuScheme = WssScheme
                , wsuHost = "example.com"
                , wsuPort = 443
                , wsuTarget = "/"
                }
      , it "ws with no path" $
          parseWebSocketURI "ws://example.com"
            `shouldBe` Right
              WebSocketURI
                { wsuScheme = WsScheme
                , wsuHost = "example.com"
                , wsuPort = 80
                , wsuTarget = "/"
                }
      , it "case-insensitive scheme" $
          parseWebSocketURI "WSS://Example.com/path"
            `shouldBe` Right
              WebSocketURI
                { wsuScheme = WssScheme
                , wsuHost = "Example.com"
                , wsuPort = 443
                , wsuTarget = "/path"
                }
      , it "fragment is dropped" $
          parseWebSocketURI "ws://example.com/path#hash"
            `shouldBe` Right
              WebSocketURI
                { wsuScheme = WsScheme
                , wsuHost = "example.com"
                , wsuPort = 80
                , wsuTarget = "/path"
                }
      ]


defaultPorts :: Spec
defaultPorts = it "scheme picks the right default port" $ do
  fmap wsuPort (parseWebSocketURI "ws://example.com/") `shouldBe` Right 80
  fmap wsuPort (parseWebSocketURI "wss://example.com/") `shouldBe` Right 443


ipv6Literals :: Spec
ipv6Literals =
  describe "IPv6 literals" $
    sequence_
      [ it "no port" $
          parseWebSocketURI "ws://[::1]/echo"
            `shouldBe` Right
              WebSocketURI
                { wsuScheme = WsScheme
                , wsuHost = "::1"
                , wsuPort = 80
                , wsuTarget = "/echo"
                }
      , it "with port" $
          parseWebSocketURI "wss://[2001:db8::1]:9443/echo"
            `shouldBe` Right
              WebSocketURI
                { wsuScheme = WssScheme
                , wsuHost = "2001:db8::1"
                , wsuPort = 9443
                , wsuTarget = "/echo"
                }
      ]


badCases :: Spec
badCases =
  describe "rejects malformed URIs" $
    sequence_
      [ it "http scheme" $ case parseWebSocketURI "http://example.com" of
          Left (URIBadScheme _) -> pure ()
          other -> expectationFailure ("expected URIBadScheme, got " <> show other)
      , it "missing host" $ case parseWebSocketURI "ws://" of
          Left URIMissingHost -> pure ()
          other -> expectationFailure ("expected URIMissingHost, got " <> show other)
      , it "non-numeric port" $ case parseWebSocketURI "ws://example.com:abc/" of
          Left (URIBadPort _) -> pure ()
          other -> expectationFailure ("expected URIBadPort, got " <> show other)
      , it "port out of range" $ case parseWebSocketURI "ws://example.com:99999/" of
          Left (URIBadPort _) -> pure ()
          other -> expectationFailure ("expected URIBadPort, got " <> show other)
      , it "unbalanced IPv6 brackets" $ case parseWebSocketURI "ws://[::1/" of
          Left (URIUnbalancedV6 _) -> pure ()
          other -> expectationFailure ("expected URIUnbalancedV6, got " <> show other)
      ]


roundTrip :: Spec
roundTrip =
  describe "parse . render = id" $
    sequence_
      [ it "ws default port" $
          check (WebSocketURI WsScheme "example.com" 80 "/chat")
      , it "ws custom port" $
          check (WebSocketURI WsScheme "example.com" 8080 "/chat?x=1")
      , it "wss default port" $
          check (WebSocketURI WssScheme "example.com" 443 "/secure")
      , it "wss IPv6 + custom port" $
          check (WebSocketURI WssScheme "::1" 9443 "/echo")
      ]
  where
    check u = parseWebSocketURI (renderWebSocketURI u) `shouldBe` Right u
