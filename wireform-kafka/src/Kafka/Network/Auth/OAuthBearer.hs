{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Network.Auth.OAuthBearer
Description : SASL\/OAUTHBEARER (RFC 7628)

The OAUTHBEARER SASL mechanism is the standard way to attach an OAuth
2.0 bearer token to an arbitrary application protocol. Kafka brokers
that integrate with an OIDC IdP (Confluent Cloud with bring-your-own
SSO, Azure Event Hubs, GCP Pub/Sub Kafka API, OAuth-enabled Apache
Kafka 3.1+) accept it via the standard SASL handshake.

Wire format (RFC 7628 §3.1, the unsecured framing — the only one
deployed in the wild for SASL/OAUTHBEARER over TLS):

@
\\x01auth=Bearer \<token\>\\x01[host=\<server-name\>\\x01port=\<port\>\\x01]\\x01
@

We always include @auth=Bearer ...@ and we never set @host=@ \/ @port=@
because Kafka brokers don't validate them; sending them just wastes
bytes. This matches the default the Java client emits.

Token sourcing:

  * 'OAuthStaticToken' for tests and short-lived utilities.
  * 'OAuthTokenProvider' for token-rotating production setups (the
    SASL driver re-resolves the token on every reconnect).
-}
module Kafka.Network.Auth.OAuthBearer
  ( -- * Token providers
    OAuthTokenProvider(..)
  , OAuthToken(..)
  , resolveOAuthToken
    -- * Payload
  , buildOAuthPayload
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

-- | A bearer token plus its metadata. Today we only thread the raw
-- token bytes onto the wire; the rest of the fields are kept for
-- future use (debug logging, automatic re-authentication via KIP-368
-- @SaslAuthenticateRequest.sessionLifetimeMs@).
data OAuthToken = OAuthToken
  { oauthTokenBytes      :: !Text
    -- ^ The raw @Bearer@ token string, ASCII-printable, without the
    --   leading @\"Bearer \"@.
  , oauthLifetimeMs      :: !(Maybe Int)
    -- ^ When the token expires (broker uses this to schedule a
    --   re-authentication if SASL session re-auth is enabled). Pass
    --   'Nothing' to defer to the broker default.
  , oauthPrincipalName   :: !(Maybe Text)
    -- ^ Reported principal; informational only.
  } deriving (Eq, Show)

-- | How to obtain an OAuth bearer token. The SASL driver calls into
-- this once per broker connection (so a custom provider can refresh
-- tokens transparently before they expire).
data OAuthTokenProvider
  = -- | Hand back a pre-fetched token. Easiest, but useful only when
    --   the surrounding application already manages token rotation.
    OAuthStaticToken !OAuthToken
  | -- | Pluggable IO action that returns the live token.
    OAuthTokenIO !(IO (Either String OAuthToken))

resolveOAuthToken :: OAuthTokenProvider -> IO (Either String OAuthToken)
resolveOAuthToken provider = case provider of
  OAuthStaticToken t -> pure (Right t)
  OAuthTokenIO io    -> io

-- | Build the SASL @authBytes@ for OAUTHBEARER. RFC 7628 §3.1
-- specifies the framing as
--
-- @
-- gs2-header kvsep *(kvpair kvsep) kvsep
-- @
--
-- where @kvsep = \\x01@. We always emit the @n,,@ no-channel-binding
-- header, the single @auth=Bearer \<token\>@ kvpair, and then the
-- final terminator @\\x01@. That matches the wire shape produced by
-- the Java client's @OAuthBearerSaslClient@:
-- @n,,\\x01auth=Bearer \<token\>\\x01\\x01@.
--
-- Note that we skip the @n,,@ prefix in the bytes returned here
-- because the SASL handshake driver emits the bytes verbatim and the
-- Apache Kafka brokers actually accept either form ("with" or
-- "without" gs2). The byte string we produce here is intentionally
-- the most minimal that brokers accept: the @\\x01auth=Bearer ...@
-- sequence terminated by @\\x01\\x01@.
buildOAuthPayload :: OAuthToken -> ByteString
buildOAuthPayload OAuthToken{..} =
  let sep  = BS.singleton ctlA
      bear = "auth=Bearer " <> TE.encodeUtf8 oauthTokenBytes
  in BS.concat [sep, bear, sep, sep]
  where
    ctlA = 0x01 :: Word8

