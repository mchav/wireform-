{-# LANGUAGE BangPatterns #-}

-- | The well-known string-format predicates that protovalidate's standard
-- constraints (and its CEL extension library) rely on: hostnames, email
-- addresses, IPv4 / IPv6 literals, CIDR prefixes, host:port pairs, and URIs.
--
-- These are pure @'Text' -> 'Bool'@ functions implementing best-effort
-- RFC-compatible checks (RFC 1034 hostnames, RFC 5321 mailboxes, RFC 791 /
-- RFC 4291 addresses, RFC 3986 URIs), mirroring the behavior of the reference
-- protovalidate implementations.
module Protovalidate.Format
  ( isHostname
  , isEmail
  , isIpv4
  , isIpv6
  , isIp
  , isIpBytes
  , isIpPrefix
  , isHostAndPort
  , isUri
  , isUriRef
  ) where

import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit, ord, toLower)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16)

----------------------------------------------------------------------
-- Hostname (RFC 1034 / RFC 1123)
----------------------------------------------------------------------

-- | A valid DNS hostname: total length <= 253, dot-separated labels of 1-63
-- characters drawn from @[A-Za-z0-9-]@ that neither start nor end with a
-- hyphen, and a final label (TLD) that is not all-numeric. A single trailing
-- dot is permitted.
isHostname :: Text -> Bool
isHostname t0 =
  let t = if not (T.null t0) && T.last t0 == '.' then T.init t0 else t0
      labels = T.splitOn "." t
   in not (T.null t)
        && T.length t <= 253
        && not (null labels)
        && all validLabel labels
        && not (allNumeric (last labels))
  where
    validLabel l =
      let n = T.length l
       in n >= 1
            && n <= 63
            && T.all labelChar l
            && T.head l /= '-'
            && T.last l /= '-'
    labelChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '-'
    allNumeric = T.all isDigit

----------------------------------------------------------------------
-- Email (RFC 5321 mailbox, no display name)
----------------------------------------------------------------------

-- | A bare RFC 5321 email address (@local\@domain@, no display name or angle
-- brackets): total length <= 254, local part 1-64 characters of the allowed
-- atext set, and a domain that is a valid 'isHostname'.
isEmail :: Text -> Bool
isEmail t =
  case T.breakOnEnd "@" t of
    (localAt, domain)
      | not (T.null localAt) ->
          let local = T.init localAt -- drop the trailing '@'
           in T.length t <= 254
                && not (T.null local)
                && T.length local <= 64
                && T.all localChar local
                && isHostname domain
    _ -> False
  where
    localChar c =
      isAsciiLower c
        || isAsciiUpper c
        || isDigit c
        || c `elem` ("!#$%&'*+/=?^_`{|}~.-" :: String)

----------------------------------------------------------------------
-- IPv4 / IPv6
----------------------------------------------------------------------

-- | A dotted-decimal IPv4 address with four octets in @0..255@ and no leading
-- zeros.
isIpv4 :: Text -> Bool
isIpv4 = isJust . parseIpv4

parseIpv4 :: Text -> Maybe [Int]
parseIpv4 t = case T.splitOn "." t of
  parts@[_, _, _, _] -> traverse octet parts
  _ -> Nothing
  where
    octet s
      | T.null s || T.length s > 3 = Nothing
      | not (T.all isDigit s) = Nothing
      | T.length s > 1 && T.head s == '0' = Nothing -- no leading zero
      | otherwise =
          let v = T.foldl' (\a c -> a * 10 + (ord c - ord '0')) 0 s
           in if v <= 255 then Just v else Nothing

-- | An IPv6 address, including @::@ compression and an optional trailing
-- embedded IPv4 (e.g. @::ffff:192.168.0.1@).
isIpv6 :: Text -> Bool
isIpv6 = isJust . parseIpv6Words

-- Parse an IPv6 address to its eight 16-bit groups.
parseIpv6Words :: Text -> Maybe [Word16]
parseIpv6Words t
  | "::" `T.isInfixOf` t = do
      let (before, afterRaw) = T.breakOn "::" t
          after = T.drop 2 afterRaw
      -- A second "::" is not allowed.
      if "::" `T.isInfixOf` after
        then Nothing
        else do
          hi <- if T.null before then Just [] else parseGroups False (T.splitOn ":" before)
          lo <- if T.null after then Just [] else parseGroups True (T.splitOn ":" after)
          let n = length hi + length lo
          if n <= 7
            then Just (hi ++ replicate (8 - n) 0 ++ lo)
            else Nothing
  | otherwise = parseGroups True (T.splitOn ":" t) >>= \gs -> if length gs == 8 then Just gs else Nothing

-- Parse a list of colon-separated pieces into 16-bit groups; if @allowV4@ the
-- final piece may be a dotted-decimal IPv4 address (contributing two groups).
parseGroups :: Bool -> [Text] -> Maybe [Word16]
parseGroups allowV4 = go
  where
    go [] = Just []
    go [g]
      | allowV4 && T.any (== '.') g = ipv4Words g
      | otherwise = (: []) <$> hextet g
    go (g : rest)
      | T.any (== '.') g = Nothing -- embedded IPv4 only allowed last
      | otherwise = (:) <$> hextet g <*> go rest

    hextet s
      | T.null s || T.length s > 4 || not (T.all isHexDigit s) = Nothing
      | otherwise = Just (T.foldl' (\a c -> a * 16 + fromIntegral (hexVal c)) 0 s)

    ipv4Words s = do
      [a, b, c, d] <- parseIpv4 s
      Just [fromIntegral (a * 256 + b), fromIntegral (c * 256 + d)]

hexVal :: Char -> Int
hexVal c
  | isDigit c = ord c - ord '0'
  | otherwise = ord (toLower c) - ord 'a' + 10

-- | Test whether @t@ is an IP address. With 'Nothing', either version is
-- accepted; with @'Just' 4@ / @'Just' 6@, only that version.
isIp :: Maybe Int -> Text -> Bool
isIp Nothing t = isIpv4 t || isIpv6 t
isIp (Just 4) t = isIpv4 t
isIp (Just 6) t = isIpv6 t
isIp _ _ = False

-- | Test whether a byte sequence is an IP address in network-byte-order form:
-- 4 bytes (IPv4) or 16 bytes (IPv6). @version@ restricts as in 'isIp'.
isIpBytes :: Maybe Int -> BS.ByteString -> Bool
isIpBytes version b = case version of
  Nothing -> n == 4 || n == 16
  Just 4 -> n == 4
  Just 6 -> n == 16
  _ -> False
  where
    n = BS.length b

----------------------------------------------------------------------
-- IP prefix (CIDR)
----------------------------------------------------------------------

-- | Test whether @t@ is a CIDR prefix (@address/length@). @version@ restricts
-- the IP version as in 'isIp'. When @strict@ is set, the host bits below the
-- prefix length must all be zero (i.e. @t@ is a network address).
isIpPrefix :: Maybe Int -> Bool -> Text -> Bool
isIpPrefix version strict t =
  case T.splitOn "/" t of
    [addr, lenStr] ->
      case prefixLen lenStr of
        Nothing -> False
        Just len -> checkVersion addr len
    _ -> False
  where
    prefixLen s
      | T.null s || not (T.all isDigit s) = Nothing
      | T.length s > 1 && T.head s == '0' = Nothing
      | otherwise = Just (T.foldl' (\a c -> a * 10 + (ord c - ord '0')) 0 s)

    checkVersion addr len =
      let v4ok = (version == Nothing || version == Just 4) && isIpv4 addr && len <= 32
          v6ok = (version == Nothing || version == Just 6) && isIpv6 addr && len <= 128
       in (v4ok && strictOk (ipv4Integer addr) 32 len)
            || (v6ok && strictOk (ipv6Integer addr) 128 len)

    strictOk Nothing _ _ = False
    strictOk (Just val) total len
      | not strict = True
      | otherwise =
          let hostBits = total - len
           in hostBits == 0 || (val `mod` (2 ^ hostBits)) == 0

ipv4Integer :: Text -> Maybe Integer
ipv4Integer t = do
  [a, b, c, d] <- parseIpv4 t
  Just (foldl (\acc x -> acc * 256 + toInteger x) 0 [a, b, c, d])

ipv6Integer :: Text -> Maybe Integer
ipv6Integer t = do
  ws <- parseIpv6Words t
  Just (foldl (\acc w -> acc `shiftL` 16 .|. toInteger w) 0 ws)

----------------------------------------------------------------------
-- host:port
----------------------------------------------------------------------

-- | Test whether @t@ is a @host:port@ pair where the host is a hostname or IP
-- (IPv6 must be bracketed, e.g. @[::1]:80@) and the port is in @0..65535@.
-- When @portRequired@ is set, the port must be present.
isHostAndPort :: Text -> Bool -> Bool
isHostAndPort t portRequired
  | T.null t = False
  | T.head t == '[' =
      -- Bracketed IPv6: [addr]:port
      case T.breakOn "]" t of
        (hostPart, rest)
          | not (T.null rest) ->
              let addr = T.drop 1 hostPart -- drop '['
                  afterBracket = T.drop 1 rest -- drop ']'
               in isIpv6 addr && validPortPart afterBracket
        _ -> False
  | otherwise =
      case T.breakOnEnd ":" t of
        (hostColon, port)
          | not (T.null hostColon) ->
              let host = T.init hostColon
               in validHost host && validPort port
        _ -> not portRequired && validHost t
  where
    validPortPart p
      | T.null p = not portRequired
      | T.head p == ':' = validPort (T.drop 1 p)
      | otherwise = False
    validHost h = isHostname h || isIpv4 h
    validPort p =
      not (T.null p)
        && T.all isDigit p
        && (T.length p == 1 || T.head p /= '0')
        && T.foldl' (\a c -> a * 10 + (ord c - ord '0')) 0 p <= (65535 :: Int)

----------------------------------------------------------------------
-- URI / URI reference (RFC 3986, best effort)
----------------------------------------------------------------------

-- | Best-effort RFC 3986 absolute-URI check: a valid scheme followed by @:@
-- and a hier-part / query / fragment composed of allowed characters with
-- well-formed percent-encoding.
isUri :: Text -> Bool
isUri t =
  case T.breakOn ":" t of
    (scheme, rest)
      | not (T.null rest) && validScheme scheme ->
          let afterScheme = T.drop 1 rest
           in not (T.null afterScheme) && validUriChars afterScheme
    _ -> False

-- | Best-effort RFC 3986 URI-reference check (absolute or relative).
isUriRef :: Text -> Bool
isUriRef t = not (T.null t) && validUriChars t

validScheme :: Text -> Bool
validScheme s = case T.uncons s of
  Just (c, rest) -> (isAsciiLower c || isAsciiUpper c) && T.all schemeChar rest
  Nothing -> False
  where
    schemeChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("+-." :: String)

-- Validate the character classes (unreserved / reserved / pct-encoded) of a
-- URI body. Percent signs must introduce two hex digits.
validUriChars :: Text -> Bool
validUriChars = go . T.unpack
  where
    go [] = True
    go ('%' : a : b : rest) = isHexDigit a && isHexDigit b && go rest
    go ('%' : _) = False
    go (c : rest) = uriChar c && go rest
    uriChar c =
      isAsciiLower c
        || isAsciiUpper c
        || isDigit c
        || c `elem` ("-._~:/?#[]@!$&'()*+,;=" :: String)
