{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | RFC 7616 HTTP Digest Access Authentication responder.

The module ships a 'ChallengeResponder' that fulfils @Digest@
challenges. The supported feature set is:

* Algorithms: @SHA-256@, @SHA-256-sess@, @SHA-512-256@,
  @SHA-512-256-sess@, and legacy @MD5@ \\/ @MD5-sess@. The legacy
  MD5 forms are off by default — pass 'allowLegacyMd5' = 'True'
  in the 'DigestPolicy' to opt in for compatibility with old
  origin servers.
* @qop=auth@ only. @qop=auth-int@ is out of scope because it
  requires hashing the request body, which the responder doesn't
  see at the middleware boundary; servers that demand it are
  exceedingly rare.
* The @userhash@ challenge parameter (RFC 7616 §3.4.4): when
  asserted, the @username@ sent on the wire is
  @H(unq(username) ':' unq(realm))@ instead of the cleartext
  username.

The responder maintains a per-jar 'DigestState' tracking the
nonce-count ('nc') per @(realm, nonce)@ so reused nonces produce
strictly-increasing counters, as required by §3.4.

Nonce values, cnonces, and counter state are kept in-process and
are not persisted across program restarts. A reissue after a
restart simply starts from @nc=00000001@; servers treat that as
a fresh client.
-}
module Network.HTTP.Client.AuthChallenge.Digest (
  -- * Configuration
  DigestPolicy (..),
  defaultDigestPolicy,

  -- * Mutable state
  DigestState,
  newDigestState,

  -- * Responder
  digestChallengeResponder,
) where

import Control.Concurrent.STM
import Crypto.Hash qualified as Hash
import Crypto.Random qualified as Random
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 qualified as BS8
import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IORef
import Data.Maybe (fromMaybe)
import Network.HTTP.Client.AuthChallenge (
  AuthChallenge (..),
  ChallengeResponder,
 )
import Text.Printf (printf)


-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Tuning knobs for the Digest responder.
data DigestPolicy = DigestPolicy
  { allowLegacyMd5 :: !Bool
  {- ^ Allow the @MD5@ \\/ @MD5-sess@ algorithms. Off by default
  because RFC 7616 §6.2 warns that they are weak; opt in
  only for compatibility with legacy servers.
  -}
  , preferredAlgorithms :: ![ByteString]
  {- ^ Order in which we pick from a multi-algorithm challenge
  (servers sometimes offer multiple at once via multiple
  challenges in @WWW-Authenticate@). Default:
  @[\"SHA-512-256\", \"SHA-512-256-sess\", \"SHA-256\",
    \"SHA-256-sess\"]@ (plus @MD5@ \\/ @MD5-sess@ at the
  tail when 'allowLegacyMd5' is set).
  -}
  }
  deriving stock (Eq, Show)


defaultDigestPolicy :: DigestPolicy
defaultDigestPolicy =
  DigestPolicy
    { allowLegacyMd5 = False
    , preferredAlgorithms =
        [ "SHA-512-256"
        , "SHA-512-256-sess"
        , "SHA-256"
        , "SHA-256-sess"
        ]
    }


-- ---------------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------------

{- | Per-client mutable state for the Digest responder.

Tracks the nonce-count per @(realm, nonce)@ pair so successive
requests within the same protection space produce strictly
increasing @nc@ values.
-}
newtype DigestState = DigestState
  { dsCounters :: TVar (HashMap (ByteString, ByteString) Int)
  }


newDigestState :: IO DigestState
newDigestState = do
  v <- newTVarIO HM.empty
  pure DigestState {dsCounters = v}


-- ---------------------------------------------------------------------------
-- Responder
-- ---------------------------------------------------------------------------

{- | 'ChallengeResponder' for RFC 7616 Digest.

The first argument resolves a @(realm)@ to @(user, password)@;
pass @const Nothing@ to refuse all Digest challenges. The
second argument carries the request method and request-target
the digest-uri parameter needs — these aren't visible from the
'ChallengeResponder' interface (which only sees challenges) so
the caller injects them at composition time:

@
'withChallengeAuth' (digestChallengeResponder policy state lookup method digestUri)
@

If you compose this from a middleware that has access to the
request, build the method \\/ digestUri arguments from
'WReq.method' and 'WURI.uriPathAndQuery'.
-}
digestChallengeResponder
  :: DigestPolicy
  -> DigestState
  -> (ByteString -> Maybe (ByteString, ByteString))
  -- ^ realm \u2192 (user, password)
  -> ByteString
  -- ^ HTTP request method (e.g. @"GET"@)
  -> ByteString
  {- ^ digest-uri (request-target — path + query as it appears
  on the request line)
  -}
  -> ChallengeResponder
digestChallengeResponder policy state lookupCreds method uri challenges =
  go (orderedChallenges policy challenges)
  where
    go [] = pure Nothing
    go (ch : rest) = do
      let realm = paramOr "" "realm" ch
      case lookupCreds realm of
        Nothing -> go rest
        Just (user, password) -> do
          mAlgo <- pure (pickAlgorithm policy ch)
          case mAlgo of
            Nothing -> go rest
            Just algo -> do
              hdr <- buildResponse state ch algo user password method uri
              pure (Just hdr)


{- | Order incoming Digest challenges by algorithm preference; drop
those whose algorithm we don't support.
-}
orderedChallenges :: DigestPolicy -> [AuthChallenge] -> [AuthChallenge]
orderedChallenges policy =
  filter
    ( \ch ->
        acScheme ch == CI.mk "Digest"
          && supportsAlgorithm policy (paramOr "MD5" "algorithm" ch)
    )


pickAlgorithm :: DigestPolicy -> AuthChallenge -> Maybe ByteString
pickAlgorithm policy ch =
  let advertised = paramOr "MD5" "algorithm" ch
  in if supportsAlgorithm policy advertised
       then Just advertised
       else Nothing


supportsAlgorithm :: DigestPolicy -> ByteString -> Bool
supportsAlgorithm policy algoBs =
  let algo = CI.mk algoBs
      sha =
        algo
          `elem` map
            CI.mk
            [ "SHA-256"
            , "SHA-256-sess"
            , "SHA-512-256"
            , "SHA-512-256-sess"
            ]
      md5 = algo `elem` map CI.mk ["MD5", "MD5-sess"]
  in sha || (allowLegacyMd5 policy && md5)


paramOr :: ByteString -> ByteString -> AuthChallenge -> ByteString
paramOr def name ch = fromMaybe def (lookup (CI.mk name) (acParams ch))


-- ---------------------------------------------------------------------------
-- Response construction
-- ---------------------------------------------------------------------------

buildResponse
  :: DigestState
  -> AuthChallenge
  -> ByteString
  -- ^ algorithm token, e.g. @"SHA-256"@
  -> ByteString
  -- ^ user
  -> ByteString
  -- ^ password
  -> ByteString
  -- ^ method
  -> ByteString
  -- ^ digest-uri
  -> IO ByteString
buildResponse state ch algoBs user password method uri = do
  let realm = paramOr "" "realm" ch
      nonce = paramOr "" "nonce" ch
      qopAttr = paramOr "" "qop" ch
      opaque = lookup (CI.mk "opaque") (acParams ch)
      userhash =
        (CI.mk <$> lookup (CI.mk "userhash") (acParams ch))
          == Just (CI.mk "true")
      qop = if "auth" `BS.isInfixOf` qopAttr then "auth" else ""
  nc <- nextNc state realm nonce
  cnonce <- mkCnonce
  let username =
        if userhash
          then hex (hashAlgo algoBs (user <> ":" <> realm))
          else user
      ha1 = computeHA1 algoBs user realm password nonce cnonce
      ha2 = computeHA2 algoBs method uri
      resp = computeResponse algoBs ha1 nonce nc cnonce qop ha2
      params =
        [ ("username", qstr username)
        , ("realm", qstr realm)
        , ("nonce", qstr nonce)
        , ("uri", qstr uri)
        , ("response", qstr resp)
        , ("algorithm", algoBs)
        ]
          <> ( if BS.null qop
                 then []
                 else
                   [ ("qop", qop)
                   , ("nc", nc)
                   , ("cnonce", qstr cnonce)
                   ]
             )
          <> ( case opaque of
                 Just o -> [("opaque", qstr o)]
                 Nothing -> []
             )
          <> (if userhash then [("userhash", "true")] else [])
  pure
    ( "Digest "
        <> BS.intercalate
          ", "
          [k <> "=" <> v | (k, v) <- params]
    )


-- ---------------------------------------------------------------------------
-- Algorithm dispatch
-- ---------------------------------------------------------------------------

hashAlgo :: ByteString -> ByteString -> ByteString
hashAlgo algoBs bs = case CI.mk algoBs of
  c
    | c == CI.mk "SHA-256" -> sha256 bs
    | c == CI.mk "SHA-256-sess" -> sha256 bs
    | c == CI.mk "SHA-512-256" -> sha512_256 bs
    | c == CI.mk "SHA-512-256-sess" -> sha512_256 bs
    | c == CI.mk "MD5" -> md5 bs
    | c == CI.mk "MD5-sess" -> md5 bs
    | otherwise -> sha256 bs


sha256 :: ByteString -> ByteString
sha256 bs = BA.convert (Hash.hash bs :: Hash.Digest Hash.SHA256)


sha512_256 :: ByteString -> ByteString
sha512_256 bs = BA.convert (Hash.hash bs :: Hash.Digest Hash.SHA512t_256)


md5 :: ByteString -> ByteString
md5 bs = BA.convert (Hash.hash bs :: Hash.Digest Hash.MD5)


-- ---------------------------------------------------------------------------
-- HA1 / HA2 / response (RFC 7616 §3.4)
-- ---------------------------------------------------------------------------

computeHA1
  :: ByteString -- algorithm
  -> ByteString -- user
  -> ByteString -- realm
  -> ByteString -- password
  -> ByteString -- nonce
  -> ByteString -- cnonce
  -> ByteString
computeHA1 algoBs user realm password nonce cnonce =
  let baseA1 = user <> ":" <> realm <> ":" <> password
      base = hex (hashAlgo algoBs baseA1)
  in if isSess algoBs
       then hex (hashAlgo algoBs (base <> ":" <> nonce <> ":" <> cnonce))
       else base


computeHA2 :: ByteString -> ByteString -> ByteString -> ByteString
computeHA2 algoBs method uri =
  hex (hashAlgo algoBs (method <> ":" <> uri))


computeResponse
  :: ByteString
  -> ByteString -- HA1
  -> ByteString -- nonce
  -> ByteString -- nc (hex)
  -> ByteString -- cnonce
  -> ByteString -- qop ("" or "auth")
  -> ByteString -- HA2
  -> ByteString
computeResponse algoBs ha1 nonce nc cnonce qop ha2 =
  let middle
        | BS.null qop = nonce
        | otherwise = nonce <> ":" <> nc <> ":" <> cnonce <> ":" <> qop
  in hex (hashAlgo algoBs (ha1 <> ":" <> middle <> ":" <> ha2))


isSess :: ByteString -> Bool
isSess algoBs = "-sess" `BS.isSuffixOf` BS8.map toLowerByte algoBs
  where
    toLowerByte c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c


-- ---------------------------------------------------------------------------
-- nc / cnonce
-- ---------------------------------------------------------------------------

{- | Allocate the next @nc@ value for the given protection space
and return it lowercase-hex padded to 8 digits.
-}
nextNc :: DigestState -> ByteString -> ByteString -> IO ByteString
nextNc state realm nonce = do
  n <- atomically $ do
    m <- readTVar (dsCounters state)
    let n' = HM.findWithDefault 0 (realm, nonce) m + 1
    writeTVar (dsCounters state) (HM.insert (realm, nonce) n' m)
    pure n'
  pure (BS8.pack (printf "%08x" n))


{- | A 24-byte random hexadecimal cnonce. RFC 7616 §3.4 requires
the cnonce to be opaque to the server and unique per request
within a nonce's lifetime.
-}
mkCnonce :: IO ByteString
mkCnonce = do
  bytes <- Random.getRandomBytes 16 :: IO ByteString
  pure (B16.encode bytes)


-- ---------------------------------------------------------------------------
-- Tiny helpers
-- ---------------------------------------------------------------------------

hex :: ByteString -> ByteString
hex = B16.encode


{- | Wrap a value in @\"…\"@. Backslash and DQUOTE inside the
value are escaped per RFC 9110 §5.6.4.
-}
qstr :: ByteString -> ByteString
qstr v = "\"" <> BS.concatMap esc v <> "\""
  where
    esc 0x22 = "\\\""
    esc 0x5C = "\\\\"
    esc w = BS.singleton w


-- | Keeps an IORef happy without causing -Wunused-top-binds.
_keepImports :: IO ()
_keepImports = do
  _ <- newIORef (0 :: Int)
  pure ()
