{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the well-known string-format predicates.
module Test.Protovalidate.Format (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Protovalidate.Format

yes :: (Text -> Bool) -> Text -> TestTree
yes f s = testCase (T.unpack s) (assertBool "expected valid" (f s))

no :: (Text -> Bool) -> Text -> TestTree
no f s = testCase (T.unpack s ++ " [invalid]") (assertBool "expected invalid" (not (f s)))

tests :: TestTree
tests =
  testGroup
    "format"
    [ testGroup
        "hostname"
        [ yes isHostname "example.com"
        , yes isHostname "a.b.c.example.com"
        , yes isHostname "localhost"
        , yes isHostname "EXAMPLE.com"
        , yes isHostname "example.com." -- trailing dot allowed
        , no isHostname ""
        , no isHostname "-example.com"
        , no isHostname "example-.com"
        , no isHostname "exa mple.com"
        , no isHostname "example..com"
        , no isHostname "123.456" -- all-numeric TLD
        ]
    , testGroup
        "email"
        [ yes isEmail "user@example.com"
        , yes isEmail "first.last@sub.example.com"
        , yes isEmail "a+b@example.com"
        , no isEmail "user@@example.com"
        , no isEmail "user"
        , no isEmail "@example.com"
        , no isEmail "user@"
        , no isEmail "user@-bad.com"
        ]
    , testGroup
        "ipv4"
        [ yes isIpv4 "0.0.0.0"
        , yes isIpv4 "192.168.1.1"
        , yes isIpv4 "255.255.255.255"
        , no isIpv4 "256.0.0.1"
        , no isIpv4 "1.2.3"
        , no isIpv4 "01.2.3.4" -- leading zero
        , no isIpv4 "1.2.3.4.5"
        ]
    , testGroup
        "ipv6"
        [ yes isIpv6 "::"
        , yes isIpv6 "::1"
        , yes isIpv6 "2001:db8::1"
        , yes isIpv6 "fe80::1ff:fe23:4567:890a"
        , yes isIpv6 "0:0:0:0:0:0:0:1"
        , yes isIpv6 "::ffff:192.168.0.1" -- embedded IPv4
        , no isIpv6 "2001:db8::1::2" -- two ::
        , no isIpv6 "abcd"
        , no isIpv6 "12345::" -- group too long
        , no isIpv6 "1:2:3:4:5:6:7:8:9"
        ]
    , testGroup
        "ip prefix"
        [ yes (isIpPrefix Nothing False) "192.168.0.0/16"
        , yes (isIpPrefix (Just 4) False) "10.0.0.5/8"
        , yes (isIpPrefix (Just 4) True) "10.0.0.0/8" -- strict: host bits zero
        , no (isIpPrefix (Just 4) True) "10.0.0.5/8" -- strict: host bits set
        , yes (isIpPrefix (Just 6) False) "2001:db8::/32"
        , no (isIpPrefix Nothing False) "10.0.0.0/33" -- prefix too long
        , no (isIpPrefix Nothing False) "10.0.0.0"
        ]
    , testGroup
        "host and port"
        [ yes (\s -> isHostAndPort s True) "example.com:8080"
        , yes (\s -> isHostAndPort s True) "127.0.0.1:80"
        , yes (\s -> isHostAndPort s True) "[::1]:443"
        , no (\s -> isHostAndPort s True) "example.com" -- port required
        , yes (\s -> isHostAndPort s False) "example.com"
        , no (\s -> isHostAndPort s True) "example.com:99999"
        ]
    , testGroup
        "uri"
        [ yes isUri "https://example.com/path?q=1#frag"
        , yes isUri "mailto:user@example.com"
        , yes isUri "urn:isbn:0451450523"
        , no isUri "//example.com/path" -- no scheme
        , no isUri "not a uri"
        , yes isUriRef "/relative/path"
        , yes isUriRef "https://example.com"
        , no isUriRef "bad uri ref"
        ]
    ]
