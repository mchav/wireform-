{- | 401 \/ 407 challenge handling (RFC 9110 \u00a711).

Three pieces:

* 'parseAuthChallenges' \/ 'AuthChallenge': lex a
  @WWW-Authenticate@ \/ @Proxy-Authenticate@ header value into a list
  of @scheme@ + parameter pairs. The actual lexing delegates to
  hermes's 'Network.HTTP.Headers.Authorization.credentialsParser',
  which parses the RFC 9110 \u00a711.4 challenge grammar (auth-scheme
  + token68 \/ auth-param list) into a structured 'Credentials'
  value; we just project that into the wireform 'AuthChallenge'
  shape that fits the rest of the client API (case-insensitive
  scheme \/ name comparisons over flat 'ByteString's).
* 'withChallengeAuth' middleware: when the inner transport returns
  @401 Unauthorized@ (or @407 Proxy Authentication Required@) and a
  matching challenge is present, the supplied callback gets a chance
  to compute a follow-up @Authorization@ header and the request is
  re-issued. If the callback returns 'Nothing', the 401 \/ 407
  response is returned to the caller verbatim.
* 'basicChallengeResponder' covers RFC 7617 Basic auth: it matches
  challenges by realm and emits the right header.

Digest is intentionally not implemented \u2014 it's no longer
recommended (its MD5 \/ SHA-256 transcript is brittle in practice
and prone to downgrade) and the wireform stack would rather call
out a missing implementation than ship a half-baked one. The
plumbing here is the substrate for a Digest add-on package if
anyone wants to write one.

== Multi-challenge values

A single @WWW-Authenticate@ header value can in principle carry
several challenges separated by commas, but the grammar is
ambiguous without scheme-aware lookahead because the comma between
challenges is indistinguishable from the comma between auth-params
of the same challenge. hermes's 'credentialsParser' parses one
challenge per header value; we run it per @WWW-Authenticate@ \/
@Proxy-Authenticate@ header line and concatenate the results, which
is the right behaviour for the much more common case of a server
emitting one challenge per header line.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.AuthChallenge
  ( -- * Challenges
    AuthChallenge (..)
  , parseAuthChallenges
    -- * Middleware
  , ChallengeResponder
  , withChallengeAuth
  , withProxyChallengeAuth
    -- * Built-in responders
  , basicChallengeResponder
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.CaseInsensitive (CI)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Short as ST

import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)

import qualified Network.HTTP.Headers.Authorization as Hermes
import qualified Network.HTTP.Headers.Parsing.Util  as Hermes

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Status as S

import qualified Network.HTTP.Client.Request   as WReq
import qualified Network.HTTP.Client.Response  as Resp
import           Network.HTTP.Client.Transport
import           Network.HTTP.Client.BodyStream (drainPopper)

-- ---------------------------------------------------------------------------
-- Challenge data
-- ---------------------------------------------------------------------------

-- | A parsed authentication challenge. Mirrors hermes's 'Credentials'
-- shape projected onto the case-insensitive 'ByteString' vocabulary
-- the rest of the client uses.
data AuthChallenge = AuthChallenge
  { acScheme :: !(CI ByteString)
    -- ^ Scheme, e.g. @\"Basic\"@. Compared case-insensitively per
    --   RFC 9110 \u00a711.4.
  , acParams :: ![(CI ByteString, ByteString)]
    -- ^ Parameter list, e.g. @[(\"realm\", \"example\")]@. Names
    --   are compared case-insensitively; values are the unquoted
    --   bytes (RFC 7230 quoted-string backslash escapes are already
    --   resolved by the hermes parser).
  , acToken68 :: !(Maybe ByteString)
    -- ^ Bearer-style schemes carry a single @token68@ form instead
    --   of a parameter list (e.g. @Negotiate ABC...@).
  }
  deriving stock (Eq, Show)

-- | Parse a single header value into one challenge. Returns @[]@ if
-- the input doesn't parse as a challenge / credentials production.
parseAuthChallenges :: ByteString -> [AuthChallenge]
parseAuthChallenges raw =
  let trimmed = BS.dropWhile isWS (BS.dropWhileEnd isWS raw)
  in case runParser Hermes.credentialsParser trimmed of
       OK creds leftover
         | BS.null (BS.dropWhile isWS leftover) ->
             [fromHermesCredentials creds]
       _ -> []
  where
    isWS w = w == 0x20 || w == 0x09

-- | Project hermes's 'Credentials' onto the wireform 'AuthChallenge'.
fromHermesCredentials :: Hermes.Credentials -> AuthChallenge
fromHermesCredentials creds = AuthChallenge
  { acScheme  = CI.mk (schemeBytes (Hermes.scheme creds))
  , acParams  = case Hermes.contents creds of
      Hermes.CredentialToken _ -> []
      Hermes.CredentialParams ps ->
        [ (CI.mk (shortToBytes k), paramValueBytes v) | (k, v) <- NE.toList ps ]
  , acToken68 = case Hermes.contents creds of
      Hermes.CredentialToken bs -> Just bs
      Hermes.CredentialParams _ -> Nothing
  }
  where
    schemeBytes (Hermes.AuthScheme s) = shortToBytes s
    paramValueBytes = \case
      Hermes.CredentialParamToken t  -> shortToBytes t
      Hermes.CredentialParamString s ->
        shortToBytes (Hermes.unsafeToRFC8941String s)
    shortToBytes = TE.encodeUtf8 . ST.toText

-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

-- | A user-supplied function that turns a list of challenges into an
-- @Authorization@ (or @Proxy-Authorization@) header value, or
-- 'Nothing' if it can't satisfy any of them.
type ChallengeResponder = [AuthChallenge] -> IO (Maybe ByteString)

-- | Re-issue a request with an @Authorization@ header when the inner
-- transport returns @401@ with a parseable challenge that the
-- responder accepts. Limits to a single retry per request \u2014
-- multi-leg negotiations (Negotiate, NTLM) need a per-scheme
-- middleware; this one is the common case.
withChallengeAuth :: ChallengeResponder -> Middleware IO
withChallengeAuth = challengeAuth 401 H.hAuthorization H.hWWWAuthenticate

withProxyChallengeAuth :: ChallengeResponder -> Middleware IO
withProxyChallengeAuth = challengeAuth 407 H.hProxyAuthorization H.hProxyAuthenticate

challengeAuth
  :: Int
    -- ^ Status code that triggers a retry (401 or 407).
  -> H.HeaderName
    -- ^ Header to set on the retry (Authorization \/ Proxy-Authorization).
  -> H.HeaderName
    -- ^ Challenge header on the response (WWW-Authenticate \/ Proxy-Authenticate).
  -> ChallengeResponder
  -> Middleware IO
challengeAuth triggerCode authHdr challengeHdr responder inner = Transport $ \req -> do
  raw <- sendRaw inner req
  let code = fromIntegral (S.statusCode (Resp.statusCode raw)) :: Int
  if code /= triggerCode
    then pure raw
    else do
      let challenges =
            concatMap parseAuthChallenges
              (H.lookupHeaders challengeHdr (Resp.headers raw))
      case challenges of
        [] -> pure raw
        cs -> do
          mAuth <- responder cs
          case mAuth of
            Nothing -> pure raw
            Just header -> do
              -- Drain the failure response so the connection / mock
              -- can advance.
              drainPopper (Resp.bodyPopper raw)
              let req' = req
                    { WReq.headers =
                        H.insertHeader authHdr header (WReq.headers req)
                    }
              sendRaw inner req'

-- ---------------------------------------------------------------------------
-- Basic
-- ---------------------------------------------------------------------------

-- | A 'ChallengeResponder' that fulfils RFC 7617 Basic challenges
-- when the supplied per-realm credentials match.
basicChallengeResponder
  :: (ByteString -> Maybe (ByteString, ByteString))
    -- ^ realm \u2192 (user, password). Pass @const Nothing@ to refuse.
  -> ChallengeResponder
basicChallengeResponder lookupCreds challenges =
  pure $ firstJust
    [ case lookupCreds realm of
        Just (u, p) -> Just ("Basic " <> B64.encode (u <> ":" <> p))
        Nothing     -> Nothing
    | ch <- challenges
    , acScheme ch == basic
    , let realm = case lookup (CI.mk "realm") (acParams ch) of
            Just r  -> r
            Nothing -> ""
    ]
  where
    basic = CI.mk "Basic"

firstJust :: [Maybe a] -> Maybe a
firstJust = go
  where
    go []           = Nothing
    go (Just x : _) = Just x
    go (Nothing : r) = go r
