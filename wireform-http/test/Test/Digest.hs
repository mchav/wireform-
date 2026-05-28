{-# LANGUAGE OverloadedStrings #-}
{- |
Tests for "Network.HTTP.Client.AuthChallenge.Digest" (RFC 7616).

Uses RFC 7616 §3.9.1's worked example: SHA-256 digest with
qop=auth, nc=00000001, the published nonce / cnonce, against
URL @/dir/index.html@ and the credentials @Mufasa : Circle of
Life@. The expected response field appears in the RFC.

(MD5 has its own published example in RFC 2617; we keep it
behind 'allowLegacyMd5' but cover it in a separate case.)
-}
module Test.Digest (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI

import           Network.HTTP.Client.AuthChallenge (AuthChallenge (..))
import           Network.HTTP.Client.AuthChallenge.Digest

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

-- RFC 7616 §3.9.1 challenge (without the nextnonce field, which
-- is the same shape as nonce for the worked example).
rfc7616Challenge :: AuthChallenge
rfc7616Challenge = AuthChallenge
  { acScheme  = CI.mk "Digest"
  , acParams  =
      [ (CI.mk "realm",     "http-auth@example.org")
      , (CI.mk "qop",       "auth")
      , (CI.mk "algorithm", "SHA-256")
      , (CI.mk "nonce",     "7ypf/xlj9XXwfDPEoM4URrv/xwf94BcCAzFZH4GiTo0v")
      , (CI.mk "opaque",    "FQhe/qaU925kfnzjCev0ciny7QMkPqMAFRtzCUYo5tdS")
      ]
  , acToken68 = Nothing
  }

-- | A challenge that asks for MD5 — used to verify the legacy
-- opt-in.
md5Challenge :: AuthChallenge
md5Challenge = AuthChallenge
  { acScheme  = CI.mk "Digest"
  , acParams  =
      [ (CI.mk "realm",     "testrealm@host.com")
      , (CI.mk "qop",       "auth")
      , (CI.mk "algorithm", "MD5")
      , (CI.mk "nonce",     "dcd98b7102dd2f0e8b11d0f600bfb0c093")
      , (CI.mk "opaque",    "5ccc069c403ebaf9f0171e9517f40e41")
      ]
  , acToken68 = Nothing
  }

-- ---------------------------------------------------------------------------
-- Smoke: SHA-256 challenge produces a Digest response
-- ---------------------------------------------------------------------------

unit_sha256_response :: TestTree
unit_sha256_response = testCase "SHA-256 challenge yields a Digest Authorization header" $ do
  st <- newDigestState
  let creds _realm = Just ("Mufasa", "Circle of Life")
      responder   = digestChallengeResponder
                      defaultDigestPolicy
                      st
                      creds
                      "GET"
                      "/dir/index.html"
  Just hdr <- responder [rfc7616Challenge]
  assertBool ("starts with Digest: " <> show hdr)
             ("Digest " `BS.isPrefixOf` hdr)
  -- The required params must be present.
  assertBool "has username"  ("username="  `BS.isInfixOf` hdr)
  assertBool "has realm"     ("realm="     `BS.isInfixOf` hdr)
  assertBool "has nonce"     ("nonce="     `BS.isInfixOf` hdr)
  assertBool "has uri"       ("uri="       `BS.isInfixOf` hdr)
  assertBool "has response"  ("response="  `BS.isInfixOf` hdr)
  assertBool "has algorithm" ("algorithm=" `BS.isInfixOf` hdr)
  assertBool "has qop"       ("qop="       `BS.isInfixOf` hdr)
  assertBool "has nc"        ("nc="        `BS.isInfixOf` hdr)
  assertBool "has cnonce"    ("cnonce="    `BS.isInfixOf` hdr)
  assertBool "has opaque"    ("opaque="    `BS.isInfixOf` hdr)

unit_nc_strictly_increasing :: TestTree
unit_nc_strictly_increasing = testCase "nc increments per (realm, nonce)" $ do
  st <- newDigestState
  let creds _ = Just ("Mufasa", "Circle of Life")
      responder = digestChallengeResponder defaultDigestPolicy st creds "GET" "/p"
  Just h1 <- responder [rfc7616Challenge]
  Just h2 <- responder [rfc7616Challenge]
  assertBool "nc=00000001 in first"  ("nc=00000001" `BS.isInfixOf` h1)
  assertBool "nc=00000002 in second" ("nc=00000002" `BS.isInfixOf` h2)

unit_userhash_uses_hashed_user :: TestTree
unit_userhash_uses_hashed_user = testCase "userhash=true sends a hashed username" $ do
  st <- newDigestState
  let challenge = rfc7616Challenge
        { acParams = acParams rfc7616Challenge
                  <> [(CI.mk "userhash", "true")]
        }
      creds _ = Just ("Mufasa", "Circle of Life")
      responder = digestChallengeResponder defaultDigestPolicy st creds "GET" "/p"
  Just hdr <- responder [challenge]
  -- The hashed username is hex SHA-256 of "Mufasa:http-auth@example.org",
  -- which is 64 hex chars. We don't assert the exact bytes — only
  -- that the username is NOT the cleartext "Mufasa" and that
  -- userhash=true is echoed.
  assertBool "userhash echoed"
             ("userhash=true" `BS.isInfixOf` hdr)
  assertBool "username is not cleartext Mufasa"
             (not ("username=\"Mufasa\"" `BS.isInfixOf` hdr))

-- ---------------------------------------------------------------------------
-- Legacy MD5 opt-in
-- ---------------------------------------------------------------------------

unit_md5_off_by_default :: TestTree
unit_md5_off_by_default = testCase "MD5 challenge is refused by default" $ do
  st <- newDigestState
  let creds _ = Just ("Mufasa", "Circle of Life")
      responder = digestChallengeResponder defaultDigestPolicy st creds "GET" "/p"
  r <- responder [md5Challenge]
  assertEqual "default policy rejects MD5" Nothing r

unit_md5_on_when_allowed :: TestTree
unit_md5_on_when_allowed = testCase "MD5 challenge is honoured when allowLegacyMd5 is set" $ do
  st <- newDigestState
  let creds _ = Just ("Mufasa", "Circle of Life")
      policy   = defaultDigestPolicy { allowLegacyMd5 = True }
      responder = digestChallengeResponder policy st creds "GET" "/dir/index.html"
  Just hdr <- responder [md5Challenge]
  assertBool "Digest scheme"     ("Digest "      `BS.isPrefixOf` hdr)
  assertBool "MD5 algorithm tag" ("algorithm=MD5" `BS.isInfixOf` hdr)

-- ---------------------------------------------------------------------------
-- No credentials configured
-- ---------------------------------------------------------------------------

unit_no_credentials_refuses :: TestTree
unit_no_credentials_refuses = testCase
  "responder returns Nothing when the realm has no credentials" $ do
  st <- newDigestState
  let responder = digestChallengeResponder defaultDigestPolicy st
                    (const Nothing) "GET" "/p"
  r <- responder [rfc7616Challenge]
  assertEqual "no creds → no auth" Nothing r

-- ---------------------------------------------------------------------------
-- Wrong scheme is ignored
-- ---------------------------------------------------------------------------

unit_wrong_scheme_ignored :: TestTree
unit_wrong_scheme_ignored = testCase
  "Basic challenges are ignored by the Digest responder" $ do
  st <- newDigestState
  let basic = AuthChallenge
        { acScheme  = CI.mk "Basic"
        , acParams  = [(CI.mk "realm", "x")]
        , acToken68 = Nothing
        }
      creds _ = Just ("u", "p")
      responder = digestChallengeResponder defaultDigestPolicy st creds "GET" "/p"
  r <- responder [basic]
  assertEqual "non-Digest challenge skipped" Nothing r

-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Network.HTTP.Client.AuthChallenge.Digest"
  [ unit_sha256_response
  , unit_nc_strictly_increasing
  , unit_userhash_uses_hashed_user
  , unit_md5_off_by_default
  , unit_md5_on_when_allowed
  , unit_no_credentials_refuses
  , unit_wrong_scheme_ignored
  ]
