{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | @ws:\/\/@ and @wss:\/\/@ URI parsing.

The high-level client takes a 'WebSocketURI' (or a raw
'ByteString' parsed into one) and lifts it into a
'Network.WebSocket.Client.WebSocketClientConfig'.  The parser is
strict about what RFC 6455 \u00a73 actually defines:

* Scheme is @ws:@ or @wss:@ (case-insensitive).
* Authority is @host[:port]@.  IPv6 literals enclosed in
  square brackets are accepted (e.g. @ws:\/\/[::1]:8080\/@).
* Path + query make up the request target; @\"/\"@ if absent.
  No fragment is parsed (RFC 6455 \u00a73 forbids it on the wire
  but lots of code in the wild appends one; we ignore it).
* Default ports: 80 for @ws:@, 443 for @wss:@.

This module deliberately does /not/ pull in @uri-templater@ or
any other URI library: the WebSocket grammar is small, and the
shape of the resulting record is tightly coupled to
'WebSocketClientConfig' \u2014 keep the parser close to its
consumer.
-}
module Network.WebSocket.URI (
  -- * URI ADT
  WebSocketURI (..),
  WebSocketScheme (..),

  -- * Parsing
  parseWebSocketURI,
  URIError (..),

  -- * Round-trip
  renderWebSocketURI,
) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8


------------------------------------------------------------------------
-- ADT
------------------------------------------------------------------------

-- | @ws:@ vs @wss:@.
data WebSocketScheme = WsScheme | WssScheme
  deriving stock (Eq, Show)


data WebSocketURI = WebSocketURI
  { wsuScheme :: !WebSocketScheme
  , wsuHost :: !ByteString
  -- ^ Hostname (no brackets, even for IPv6).
  , wsuPort :: !Int
  -- ^ Port number, defaulted from scheme when absent.
  , wsuTarget :: !ByteString
  {- ^ Request target (path + query, no fragment).
  Always non-empty; defaults to @"/"@.
  -}
  }
  deriving stock (Eq, Show)


-- | Default port for a scheme (RFC 6455 \u00a73).
defaultPort :: WebSocketScheme -> Int
defaultPort WsScheme = 80
defaultPort WssScheme = 443


------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

data URIError
  = URIBadScheme !ByteString
  | URIMissingHost
  | URIBadPort !ByteString
  | URIUnbalancedV6 !ByteString
  deriving stock (Eq, Show)


instance Exception URIError


------------------------------------------------------------------------
-- Parse
------------------------------------------------------------------------

{- | Parse a WebSocket URI.  Strict per RFC 6455 \u00a73 (returns
'Left' on anything outside the allowed grammar) with two
pragmatic tolerances: a trailing @#fragment@ is dropped silently
(RFC 6455 says fragments are forbidden but they appear in the
wild), and the scheme match is case-insensitive ("WS:" / "WSS:"
are accepted just like "ws:" / "wss:").
-}
parseWebSocketURI :: ByteString -> Either URIError WebSocketURI
parseWebSocketURI input = do
  (scheme, afterScheme) <- splitScheme input
  let (authPath, _frag) = case BS.break (== 0x23 {- '#' -}) afterScheme of
        (l, r)
          | BS.null r -> (l, BS.empty)
          | otherwise -> (l, BS.drop 1 r)
      (authority, pathQuery) = BS.break isPathStart authPath
      isPathStart b = b == 0x2F {- '/' -} || b == 0x3F {- '?' -}
  (host, mPortBs) <- splitAuthority authority
  port <- case mPortBs of
    Nothing -> Right (defaultPort scheme)
    Just p -> parsePort p
  let target = if BS.null pathQuery then "/" else pathQuery
  Right
    WebSocketURI
      { wsuScheme = scheme
      , wsuHost = host
      , wsuPort = port
      , wsuTarget = target
      }


splitScheme
  :: ByteString
  -> Either URIError (WebSocketScheme, ByteString)
splitScheme bs
  | "ws://" `isPrefixCI` bs = Right (WsScheme, BS.drop 5 bs)
  | "wss://" `isPrefixCI` bs = Right (WssScheme, BS.drop 6 bs)
  | otherwise =
      let (s, _) = BS.break (== 0x3A {- ':' -}) bs
      in Left (URIBadScheme s)
  where
    isPrefixCI p haystack =
      BS.length haystack >= BS.length p
        && asciiLower (BS.take (BS.length p) haystack) == p
    asciiLower = BS.map toLowerB
    toLowerB b
      | b >= 0x41 && b <= 0x5A = b + 0x20
      | otherwise = b


splitAuthority
  :: ByteString
  -> Either URIError (ByteString, Maybe ByteString)
splitAuthority bs
  | BS.null bs = Left URIMissingHost
  | BS.head bs == 0x5B {- '[' -} = do
      -- IPv6 literal: '[' ... ']' [ ':' port ]
      case BS.elemIndex 0x5D {- ']' -} bs of
        Nothing -> Left (URIUnbalancedV6 bs)
        Just ix ->
          let host = BS.drop 1 (BS.take ix bs)
              afterBracket = BS.drop (ix + 1) bs
          in case BS.uncons afterBracket of
               Nothing -> Right (host, Nothing)
               Just (0x3A {- ':' -}, p) -> Right (host, Just p)
               _ -> Left URIMissingHost
  | otherwise = case BS.elemIndex 0x3A {- ':' -} bs of
      Just ix ->
        let host = BS.take ix bs
            port = BS.drop (ix + 1) bs
        in if BS.null host then Left URIMissingHost else Right (host, Just port)
      Nothing -> Right (bs, Nothing)


parsePort :: ByteString -> Either URIError Int
parsePort bs
  | BS.null bs = Left (URIBadPort bs)
  | otherwise = case BS8.readInt bs of
      Just (n, leftover) | BS.null leftover && n > 0 && n <= 65535 -> Right n
      _ -> Left (URIBadPort bs)


------------------------------------------------------------------------
-- Render
------------------------------------------------------------------------

{- | Render a parsed URI back to its canonical form.  Inverse of
'parseWebSocketURI' (modulo the case-insensitivity tolerance).
-}
renderWebSocketURI :: WebSocketURI -> ByteString
renderWebSocketURI u =
  BS.concat
    [ case wsuScheme u of WsScheme -> "ws://"; WssScheme -> "wss://"
    , wrapHost (wsuHost u)
    , if wsuPort u == defaultPort (wsuScheme u)
        then BS.empty
        else ":" <> BS8.pack (show (wsuPort u))
    , wsuTarget u
    ]
  where
    -- Wrap IPv6 literals back in brackets so the round-trip is
    -- syntactically valid.  Heuristic: any ':' in the host means
    -- it's an IPv6 literal (DNS names cannot contain ':').
    wrapHost h
      | 0x3A `BS.elem` h = "[" <> h <> "]"
      | otherwise = h
