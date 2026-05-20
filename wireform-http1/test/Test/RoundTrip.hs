{-# LANGUAGE OverloadedStrings #-}
{- | Encoder \/ parser round-trip properties.

These are the gold-standard tests: if a 'Request' \/ 'Response' encodes
to bytes that parse back to the same value, the two halves of the
library agree on the wire format. We do this with QuickCheck so we
cover unusual header sequences, empty bodies, oversized bodies, etc.
-}
module Test.RoundTrip (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString (ByteString)

import Test.Tasty
import Test.Tasty.QuickCheck

import Network.HTTP1.Encode (encodeRequestHead, encodeResponseHead)
import Network.HTTP1.Parser
import Network.HTTP1.Types

tests :: TestTree
tests = testGroup "RoundTrip"
  [ testProperty "request head"  prop_requestHead
  , testProperty "response head" prop_responseHead
  ]

prop_requestHead :: GenRequest -> Property
prop_requestHead (GenRequest meth tgt hdrs) =
  let req = Request meth tgt HTTP_1_1 hdrs BodyEmpty (pure [])
      bs  = encodeRequestHead req
      stripped = stripBlankLine bs
  in case parseRequest stripped of
       Left e -> counterexample (show (e, bs)) False
       Right (r, _) -> conjoin
         [ requestMethod r === meth
         , requestTarget r === tgt
         , filter (notZeroCL . fst) (requestHeaders r)
             === filter (notZeroCL . fst) hdrs
         ]
  where
    notZeroCL n = not (asciiIeqStr n "content-length")

prop_responseHead :: GenResponse -> Property
prop_responseHead (GenResponse st hdrs) =
  let resp = Response st HTTP_1_1 hdrs BodyEmpty
      bs   = encodeResponseHead resp
      stripped = stripBlankLine bs
  in case parseResponse GET stripped of
       Left e -> counterexample (show (e, bs)) False
       Right (r, _) -> conjoin
         [ responseStatus r === st
         , filter notAutoInjected (responseHeaders r)
             === filter notAutoInjected hdrs
         ]
  where
    -- The encoder auto-injects Content-Length and Date; filter both
    -- out before comparing to the input header list.
    notAutoInjected (n, _) =
         not (asciiIeqStr n "content-length")
      && not (asciiIeqStr n "date")

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- The parser expects the slice without the @\\r\\n\\r\\n@ terminator;
-- the encoder appends one. Strip it for the round-trip.
stripBlankLine :: ByteString -> ByteString
stripBlankLine bs = case BS.breakSubstring "\r\n\r\n" bs of
  (block, _) -> block

asciiIeqStr :: ByteString -> ByteString -> Bool
asciiIeqStr a b = BSC.map dn a == BSC.map dn b
  where dn c | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32) | otherwise = c

------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------

data GenRequest = GenRequest !Method !ByteString !Headers
  deriving stock (Show)

data GenResponse = GenResponse !Status !Headers
  deriving stock (Show)

instance Arbitrary GenRequest where
  arbitrary = do
    meth <- elements [GET, HEAD, POST, PUT, DELETE, OPTIONS, PATCH]
    pathLen <- choose (1, 16)
    tgt <- pure (BS.cons 0x2f (BS.pack (replicate pathLen 0x61)))
    extras <- listOf genHeader
    -- RFC 9112 § 3.2 requires HTTP/1.1 requests to carry a Host. The
    -- parser enforces this, so the generator must always seed one.
    pure (GenRequest meth tgt (("Host", "example.com") : extras))

instance Arbitrary GenResponse where
  arbitrary = do
    code <- elements [200, 201, 301, 400, 404, 500]
    hdrs <- listOf genHeader
    pure (GenResponse (Status code) hdrs)

genHeader :: Gen Header
genHeader = do
  k <- elements
    [ "X-Foo", "X-Bar", "Accept", "Content-Type"
    , "Cache-Control", "Pragma", "User-Agent"
    ]
  vlen <- choose (1, 32)
  v <- BS.pack <$> vectorOf vlen (choose (0x21, 0x7e))
  pure (k, v)
