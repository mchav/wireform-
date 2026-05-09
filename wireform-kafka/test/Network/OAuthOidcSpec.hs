{-# LANGUAGE OverloadedStrings #-}

module Network.OAuthOidcSpec (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Kafka.Network.Auth.OAuthOidc as O

tests :: TestTree
tests = testGroup "OAuth/OIDC + PKCE (KIP-768 / KIP-1169)"
  [ testCase "PKCE plain method passes the verifier through"
      pkce_plain
  , testCase "PKCE S256 produces a non-empty url-safe digest"
      pkce_s256
  , testCase "tokenRefreshDeadlineMs uses 75% of remaining lifetime"
      refresh_deadline
  , testCase "shouldRefreshToken trips before expiry"
      should_refresh
  , testCase "TokenCache stores + retrieves"
      cache_round_trip
  ]

pkce_plain :: IO ()
pkce_plain = do
  let v = O.mkPkceVerifier (BSC.pack "alphabet-soup")
  O.pkceChallenge O.PkcePlain v @?= O.unPkceVerifier v

pkce_s256 :: IO ()
pkce_s256 = do
  let v = O.mkPkceVerifier (BS.replicate 32 0)
      c = O.pkceChallenge O.PkceS256 v
  -- Non-empty + url-safe (no '+' / '/' / '=')
  assertBool "non-empty"   (not (T.null c))
  assertBool "url-safe"    (T.all (\ch -> ch /= '+' && ch /= '/' && ch /= '=') c)

refresh_deadline :: IO ()
refresh_deadline =
  -- issued at 0, expires at 1000 -> 75% threshold = 750.
  O.tokenRefreshDeadlineMs (mkToken 0 1000) @?= 750

should_refresh :: IO ()
should_refresh = do
  let t = mkToken 0 1000
  O.shouldRefreshToken 500 t  @?= False
  O.shouldRefreshToken 800 t  @?= True
  O.shouldRefreshToken 1100 t @?= True

cache_round_trip :: IO ()
cache_round_trip = do
  c <- O.newTokenCache
  let t = mkToken 0 1000
  O.storeToken c "client-1" t
  m <- O.lookupToken c "client-1"
  m @?= Just t
  m2 <- O.lookupToken c "missing"
  m2 @?= Nothing

mkToken :: Int -> Int -> O.OidcToken
mkToken issued expires = O.OidcToken
  { O.otAccessToken  = "abc"
  , O.otTokenType    = "Bearer"
  , O.otIssuedAtMs   = fromIntegral issued
  , O.otExpiresAtMs  = fromIntegral expires
  , O.otRefreshToken = Nothing
  , O.otScope        = Nothing
  }
