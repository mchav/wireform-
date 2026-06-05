{-# LANGUAGE OverloadedStrings #-}

module Network.OAuthOidcSpec (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import qualified Data.Text as T
import Test.Syd

import qualified Kafka.Network.Auth.OAuthBearer as OAuth
import qualified Kafka.Network.Auth.OAuthOidc as O
import qualified Kafka.Time as KafkaTime

tests :: Spec
tests = describe "OAuth/OIDC + PKCE (KIP-768 / KIP-1169)" $ sequence_
  [ it "PKCE plain method passes the verifier through"
      pkce_plain
  , it "PKCE S256 produces a non-empty url-safe digest"
      pkce_s256
  , it "tokenRefreshDeadlineMs uses 75% of remaining lifetime"
      refresh_deadline
  , it "shouldRefreshToken trips before expiry"
      should_refresh
  , it "TokenCache stores + retrieves"
      cache_round_trip
  , it "oidcTokenProvider reuses a fresh cached token"
      provider_uses_fresh_cache
  , it "oidcTokenProvider fetches and stores a stale token"
      provider_refreshes_stale_cache
  , it "oidcTokenProvider requires a PKCE verifier when enabled"
      provider_requires_pkce_verifier
  ]

pkce_plain :: IO ()
pkce_plain = do
  let v = O.mkPkceVerifier (BSC.pack "alphabet-soup")
  O.pkceChallenge O.PkcePlain v `shouldBe` O.unPkceVerifier v

pkce_s256 :: IO ()
pkce_s256 = do
  let v = O.mkPkceVerifier (BS.replicate 32 0)
      c = O.pkceChallenge O.PkceS256 v
  -- Non-empty + url-safe (no '+' / '/' / '=')
  (not (T.null c)) `shouldBe` True
  (T.all (\ch -> ch /= '+' && ch /= '/' && ch /= '=') c) `shouldBe` True

refresh_deadline :: IO ()
refresh_deadline =
  -- issued at 0, expires at 1000 -> 75% threshold = 750.
  O.tokenRefreshDeadlineMs (mkToken 0 1000) `shouldBe` 750

should_refresh :: IO ()
should_refresh = do
  let t = mkToken 0 1000
  O.shouldRefreshToken 500 t  `shouldBe` False
  O.shouldRefreshToken 800 t  `shouldBe` True
  O.shouldRefreshToken 1100 t `shouldBe` True

cache_round_trip :: IO ()
cache_round_trip = do
  c <- O.newTokenCache
  let t = mkToken 0 1000
  O.storeToken c "client-1" t
  m <- O.lookupToken c "client-1"
  m `shouldBe` Just t
  m2 <- O.lookupToken c "missing"
  m2 `shouldBe` Nothing

provider_uses_fresh_cache :: IO ()
provider_uses_fresh_cache = do
  now <- KafkaTime.currentTimeMillis
  cache <- O.newTokenCache
  calls <- newIORef (0 :: Int)
  let cfg = testConfig False
      token = (mkToken 0 1000)
        { O.otAccessToken = "cached"
        , O.otIssuedAtMs = now - 1_000
        , O.otExpiresAtMs = now + 60_000
        }
      fetcher = O.OidcTokenFetcher
        { O.otfFetchToken = \_ _ -> do
            modifyIORef' calls (+ 1)
            pure (Right ((mkToken 0 1000) { O.otAccessToken = "fetched" }))
        }
  O.storeToken cache (O.oidcClientId cfg) token
  resolved <- OAuth.resolveOAuthToken (O.oidcTokenProvider cfg cache fetcher)
  case resolved of
    Right tok -> OAuth.oauthTokenBytes tok `shouldBe` "cached"
    Left err -> (if (False) then pure () else expectationFailure (err))
  readIORef calls >>= (`shouldBe` 0)

provider_refreshes_stale_cache :: IO ()
provider_refreshes_stale_cache = do
  now <- KafkaTime.currentTimeMillis
  cache <- O.newTokenCache
  calls <- newIORef (0 :: Int)
  let cfg = testConfig False
      stale = (mkToken 0 1000)
        { O.otAccessToken = "stale"
        , O.otIssuedAtMs = now - 10_000
        , O.otExpiresAtMs = now - 1_000
        }
      fresh = (mkToken 0 1000)
        { O.otAccessToken = "fresh"
        , O.otIssuedAtMs = now
        , O.otExpiresAtMs = now + 120_000
        }
      fetcher = O.OidcTokenFetcher
        { O.otfFetchToken = \_ _ -> do
            modifyIORef' calls (+ 1)
            pure (Right fresh)
        }
  O.storeToken cache (O.oidcClientId cfg) stale
  resolved <- OAuth.resolveOAuthToken (O.oidcTokenProvider cfg cache fetcher)
  case resolved of
    Right tok -> OAuth.oauthTokenBytes tok `shouldBe` "fresh"
    Left err -> (if (False) then pure () else expectationFailure (err))
  readIORef calls >>= (`shouldBe` 1)
  cached <- O.lookupToken cache (O.oidcClientId cfg)
  cached `shouldBe` Just fresh

provider_requires_pkce_verifier :: IO ()
provider_requires_pkce_verifier = do
  cache <- O.newTokenCache
  calls <- newIORef (0 :: Int)
  let cfg = testConfig True
      fetcher = O.OidcTokenFetcher
        { O.otfFetchToken = \_ _ -> do
            modifyIORef' calls (+ 1)
            pure (Right (mkToken 0 1000))
        }
  resolved <- OAuth.resolveOAuthToken (O.oidcTokenProvider cfg cache fetcher)
  case resolved of
    Left err -> ("PKCE" `T.isInfixOf` T.pack err) `shouldBe` True
    Right _ -> (False) `shouldBe` True
  readIORef calls >>= (`shouldBe` 0)

testConfig :: Bool -> O.OidcClientConfig
testConfig usePkce = O.OidcClientConfig
  { O.oidcIssuerUrl = "https://issuer.example.com"
  , O.oidcClientId = "client-1"
  , O.oidcClientSecret = Just "secret"
  , O.oidcScopes = ["openid"]
  , O.oidcAudience = Nothing
  , O.oidcUsePkce = usePkce
  }

mkToken :: Int -> Int -> O.OidcToken
mkToken issued expires = O.OidcToken
  { O.otAccessToken  = "abc"
  , O.otTokenType    = "Bearer"
  , O.otIssuedAtMs   = fromIntegral issued
  , O.otExpiresAtMs  = fromIntegral expires
  , O.otRefreshToken = Nothing
  , O.otScope        = Nothing
  }
