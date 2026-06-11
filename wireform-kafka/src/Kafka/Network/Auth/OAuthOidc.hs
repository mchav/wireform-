{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Kafka.Network.Auth.OAuthOidc
Description : KIP-768 / KIP-1169 OAuth 2.0 / OIDC client extensions

KIP-768 brought OIDC discovery + JWT validation to Kafka's
SASL/OAUTHBEARER mechanism: clients no longer need a
hand-rolled token-fetch routine, just an issuer URL + client
credentials. KIP-1169 added support for PKCE (Proof Key for
Code Exchange) so public clients (mobile / SPA-style) don't
have to embed a long-lived secret.

This module is the /pure/ data layer:

  * 'OidcClientConfig' — issuer + clientId + clientSecret +
    optional PKCE state.
  * 'PkceVerifier' / 'mkPkceVerifier' / 'pkceChallenge' — the
    SHA-256 challenge derivation + url-safe base64 encoding.
  * 'tokenRefreshDeadlineMs' — given a token's @expires_in@,
    decide when to refresh.
  * 'TokenCache' — in-memory cache keyed by client id.

The actual HTTP token fetch is intentionally pluggable:
'OidcTokenFetcher' is a record-of-IO so callers can wire
@http-client@, @wreq@, or whatever transport their org standardises
on.
-}
module Kafka.Network.Auth.OAuthOidc (
  -- * Config
  OidcClientConfig (..),

  -- * PKCE (KIP-1169)
  PkceVerifier (..),
  mkPkceVerifier,
  pkceChallenge,
  PkceMethod (..),

  -- * Token cache + refresh decision
  OidcToken (..),
  TokenCache,
  newTokenCache,
  storeToken,
  lookupToken,
  tokenRefreshDeadlineMs,
  shouldRefreshToken,
  oidcTokenProvider,
  oidcTokenProviderWithPkce,

  -- * Pluggable fetcher
  OidcTokenFetcher (..),
) where

import Control.Concurrent.STM
import Crypto.Hash qualified as Hash
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Kafka.Network.Auth.OAuthBearer qualified as OAuth
import Kafka.Time qualified as KafkaTime


----------------------------------------------------------------------
-- Config
----------------------------------------------------------------------

data OidcClientConfig = OidcClientConfig
  { oidcIssuerUrl :: !Text
  -- ^ e.g. @https://auth.acme.com@
  , oidcClientId :: !Text
  , oidcClientSecret :: !(Maybe Text)
  -- ^ 'Nothing' for public clients using PKCE.
  , oidcScopes :: ![Text]
  , oidcAudience :: !(Maybe Text)
  , oidcUsePkce :: !Bool
  {- ^ When 'True', the token request includes the PKCE
  challenge derived from the cached 'PkceVerifier'.
  -}
  }
  deriving stock (Eq, Show, Generic)


----------------------------------------------------------------------
-- PKCE
----------------------------------------------------------------------

{- | The PKCE code-verifier — a high-entropy random string
(43..128 url-safe characters per RFC 7636 §4.1). The client
generates one verifier per token request, sends the
/challenge/ on the auth request, and the /verifier/ on the
token request.
-}
newtype PkceVerifier = PkceVerifier {unPkceVerifier :: Text}
  deriving stock (Eq, Show, Generic)


data PkceMethod
  = PkceS256
  | PkcePlain
  deriving stock (Eq, Show, Generic)


{- | Build a PkceVerifier from caller-supplied entropy. We don't
shell out to a CSPRNG here — the caller passes in the bytes
so the function stays pure (OAuth-server-specific length
checks live in the actual HTTP exchange code).
-}
mkPkceVerifier :: ByteString -> PkceVerifier
mkPkceVerifier bs =
  PkceVerifier $
    TE.decodeUtf8 (BAE.convertToBase BAE.Base64URLUnpadded bs)


{- | Compute the PKCE challenge for the verifier. Returns the
@code_challenge@ string the auth-server expects.
-}
pkceChallenge :: PkceMethod -> PkceVerifier -> Text
pkceChallenge method (PkceVerifier verifier) = case method of
  PkcePlain -> verifier
  PkceS256 ->
    let !raw = TE.encodeUtf8 verifier
        !digest = Hash.hashWith Hash.SHA256 raw
        !b64 =
          BAE.convertToBase
            BAE.Base64URLUnpadded
            (BA.convert digest :: ByteString)
    in TE.decodeUtf8 b64


----------------------------------------------------------------------
-- Tokens
----------------------------------------------------------------------

data OidcToken = OidcToken
  { otAccessToken :: !Text
  , otTokenType :: !Text
  -- ^ Typically @"Bearer"@.
  , otIssuedAtMs :: !Int64
  , otExpiresAtMs :: !Int64
  , otRefreshToken :: !(Maybe Text)
  , otScope :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)


{- | At what wall-clock-ms should we proactively refresh the
token? Defaults to "75% of remaining lifetime" — same
heuristic as the JVM @OAuthBearerLoginCallbackHandler@.
-}
tokenRefreshDeadlineMs :: OidcToken -> Int64
tokenRefreshDeadlineMs t =
  let !remaining = otExpiresAtMs t - otIssuedAtMs t
      !cushion = remaining `quot` 4 -- 25% buffer
  in otExpiresAtMs t - cushion


shouldRefreshToken :: Int64 -> OidcToken -> Bool
shouldRefreshToken now t = now >= tokenRefreshDeadlineMs t


----------------------------------------------------------------------
-- Cache
----------------------------------------------------------------------

-- | In-memory cache of OIDC tokens keyed by client id.
newtype TokenCache = TokenCache (TVar (HashMap Text OidcToken))


newTokenCache :: IO TokenCache
newTokenCache = TokenCache <$> newTVarIO HashMap.empty


storeToken :: TokenCache -> Text -> OidcToken -> IO ()
storeToken (TokenCache v) clientId tok =
  atomically $
    modifyTVar' v (HashMap.insert clientId tok)


lookupToken :: TokenCache -> Text -> IO (Maybe OidcToken)
lookupToken (TokenCache v) clientId = do
  m <- readTVarIO v
  pure (HashMap.lookup clientId m)


----------------------------------------------------------------------
-- Pluggable fetcher
----------------------------------------------------------------------

{- | A pluggable token-endpoint client. The wireform-kafka
library doesn't pull in @http-client@; callers wire whatever
transport their org standardises on.
-}
data OidcTokenFetcher = OidcTokenFetcher
  { otfFetchToken
      :: !( OidcClientConfig
            -> Maybe PkceVerifier
            -> IO (Either String OidcToken)
          )
  }


{- | Build a SASL\/OAUTHBEARER token provider from an OIDC fetcher and
cache. Cached tokens are reused until they reach the proactive
refresh deadline from 'tokenRefreshDeadlineMs'.
-}
oidcTokenProvider
  :: OidcClientConfig
  -> TokenCache
  -> OidcTokenFetcher
  -> OAuth.OAuthTokenProvider
oidcTokenProvider cfg cache fetcher =
  oidcTokenProviderWithPkce cfg cache fetcher Nothing


-- | Variant of 'oidcTokenProvider' for public clients using PKCE.
oidcTokenProviderWithPkce
  :: OidcClientConfig
  -> TokenCache
  -> OidcTokenFetcher
  -> Maybe PkceVerifier
  -> OAuth.OAuthTokenProvider
oidcTokenProviderWithPkce cfg@OidcClientConfig {..} cache fetcher mVerifier =
  OAuth.OAuthTokenIO $ do
    now <- KafkaTime.currentTimeMillis
    cached <- lookupToken cache oidcClientId
    case cached of
      Just tok
        | not (shouldRefreshToken now tok) ->
            pure (Right (toOAuthToken tok))
      _ ->
        if oidcUsePkce && mVerifier == Nothing
          then pure (Left "OIDC: PKCE is enabled but no verifier was supplied")
          else do
            fetched <- otfFetchToken fetcher cfg (if oidcUsePkce then mVerifier else Nothing)
            case fetched of
              Left err -> pure (Left err)
              Right tok -> do
                storeToken cache oidcClientId tok
                pure (Right (toOAuthToken tok))


toOAuthToken :: OidcToken -> OAuth.OAuthToken
toOAuthToken OidcToken {..} =
  OAuth.OAuthToken
    { OAuth.oauthTokenBytes = otAccessToken
    , OAuth.oauthLifetimeMs = Just (fromIntegral (otExpiresAtMs - otIssuedAtMs))
    , OAuth.oauthPrincipalName = Nothing
    }
