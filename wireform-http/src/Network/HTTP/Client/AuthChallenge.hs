{- | 401 \/ 407 challenge handling (RFC 9110 \u00a711).

Two pieces:

* 'parseAuthChallenges' \/ 'AuthChallenge': lex one or more
  challenges out of a @WWW-Authenticate@ \/ @Proxy-Authenticate@
  header value into a list of @scheme@ + parameter pairs.
* 'withChallengeAuth' middleware: when the inner transport returns
  @401 Unauthorized@ (or @407 Proxy Authentication Required@) and a
  matching challenge is present, the supplied callback gets a chance
  to compute a follow-up @Authorization@ header and the request is
  re-issued. If the callback returns 'Nothing', the 401 \/ 407
  response is returned to the caller verbatim.

The shipped 'basicChallengeResponder' covers RFC 7617 Basic auth: it
matches challenges by realm and emits the right header. Digest is
intentionally not implemented \u2014 it's no longer recommended (its
MD5 \/ SHA-256 transcript is brittle in practice and prone to
downgrade) and the wireform stack would rather call out a missing
implementation than ship a half-baked one. The plumbing here is the
substrate for a Digest add-on package if anyone wants to write one.

== Parsing limitations

The shipped 'parseAuthChallenges' is a small lexer aimed at the
shapes overwhelmingly seen in the wild: a single scheme followed by a
parameter list, or a scheme followed by a single @token68@ value
(@Bearer ABC...@). Multi-challenge values on a single header line
(e.g. @Basic realm=\"x\", Digest qop=\"auth\"@) are inherently
ambiguous without scheme-aware lookahead because the comma between
challenges is indistinguishable from the comma between parameters.
For multi-scheme servers, send each challenge on its own
@WWW-Authenticate@ header line; the lookup in 'withChallengeAuth'
runs the parser per header line and concatenates the result.
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

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Status as S

import qualified Network.HTTP.Client.Request   as WReq
import qualified Network.HTTP.Client.Response  as Resp
import           Network.HTTP.Client.Transport
import           Network.HTTP.Client.BodyStream (drainPopper)

-- ---------------------------------------------------------------------------
-- Challenge data
-- ---------------------------------------------------------------------------

-- | A parsed authentication challenge.
data AuthChallenge = AuthChallenge
  { acScheme :: !(CI ByteString)
    -- ^ Scheme, e.g. @\"Basic\"@. Compared case-insensitively per
    --   RFC 9110 \u00a711.4.
  , acParams :: ![(CI ByteString, ByteString)]
    -- ^ Parameter list, e.g. @[(\"realm\", \"example\")]@.
  , acToken68 :: !(Maybe ByteString)
    -- ^ Bearer-style schemes carry a single @token68@ form instead
    --   of a parameter list (e.g. @Negotiate ABC...@).
  }
  deriving stock (Eq, Show)

-- | Parse a header value into one challenge. The full multi-challenge
-- grammar is parser-disambiguous in scheme-aware contexts only;
-- callers wanting multi-scheme support should send each challenge on
-- its own header line and call this per line.
parseAuthChallenges :: ByteString -> [AuthChallenge]
parseAuthChallenges raw0 =
  let raw = dropWS raw0
      (scheme, rest) = BS.break isSpaceB raw
      rest' = dropWS rest
  in if BS.null scheme
       then []
       else
         let token = parseToken68 rest'
             prms  = if hasEquals rest' then parseParams rest' else []
             ch = AuthChallenge
               { acScheme  = CI.mk scheme
               , acParams  = [(CI.mk k, v) | (k, v) <- prms]
               , acToken68 = if null prms then token else Nothing
               }
         in [ch]
  where
    isSpaceB w = w == 0x20 || w == 0x09
    dropWS     = BS.dropWhile (\w -> w == 0x20 || w == 0x09 || w == 0x0D || w == 0x0A)

    -- A token68 is one bare token without an '=' before whitespace
    -- or end of input.
    parseToken68 bs =
      let t = BS.takeWhile (\w -> not (w == 0x20 || w == 0x09 || w == 0x2C)) bs
      in if BS.null t then Nothing else Just t

    -- Heuristic: there's a '=' on this line, so we treat the rest as
    -- a parameter list.
    hasEquals = BS.elem 0x3D

    parseParams = go []
      where
        go acc bs0
          | BS.null bs0 = reverse acc
          | otherwise =
              let bs = dropWS bs0
                  (k, rest) = BS.break (\w -> w == 0x3D || w == 0x2C) bs
              in case BS.uncons rest of
                   Just (0x3D, after) ->
                     let (v, leftover) = lexValue after
                     in go ((BS.dropWhileEnd isSpaceB k, v) : acc)
                           (skipComma leftover)
                   Just (0x2C, after) ->
                     -- bare token without '='; skip
                     go acc (dropWS after)
                   _ -> reverse acc

        lexValue bs0 = case BS.uncons (dropWS bs0) of
          Just (0x22, body) ->
            let (v, rest) = BS.break (== 0x22) body
            in case BS.uncons rest of
                 Just (0x22, r) -> (unescapeQuoted v, r)
                 _              -> (unescapeQuoted v, rest)
          _ ->
            let bs = dropWS bs0
                (v, rest) = BS.break (\w -> w == 0x2C || w == 0x20 || w == 0x09) bs
            in (v, rest)

        skipComma bs = case BS.uncons (dropWS bs) of
          Just (0x2C, r) -> r
          _              -> bs

    -- RFC 7230 quoted-string \: a backslash escapes the next byte.
    unescapeQuoted = BS.pack . unescape . BS.unpack
    unescape []           = []
    unescape (0x5C : c : r) = c : unescape r
    unescape (c : r)        = c : unescape r

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
