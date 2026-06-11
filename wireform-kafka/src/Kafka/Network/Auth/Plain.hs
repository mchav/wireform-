{- |
Module      : Kafka.Network.Auth.Plain
Description : SASL/PLAIN authentication implementation
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

SASL/PLAIN is a simple username/password authentication mechanism.
It transmits credentials in plaintext, so it should only be used
over encrypted connections (TLS/SSL).

The PLAIN mechanism encodes credentials as:

> [authzid] \\0 username \\0 password

Where authzid is typically empty for Kafka.

See RFC 4616 for the complete PLAIN SASL mechanism specification.
-}
module Kafka.Network.Auth.Plain (
  generatePlainAuth,
  generatePlainAuthWithAuthzid,
  plainAuthData,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)


{- | Generate SASL/PLAIN authentication data with the Kafka-default
empty authorization identity.
-}
generatePlainAuth
  :: Text
  -- ^ Username
  -> Text
  -- ^ Password
  -> ByteString
generatePlainAuth = generatePlainAuthWithAuthzid Nothing


{- | Generate SASL/PLAIN authentication data.

The format is: @[authzid] \\0 username \\0 password@. For Kafka,
@authzid@ is usually empty, but RFC 4616 permits callers to request
a distinct authorization identity when the broker side supports it.
-}
generatePlainAuthWithAuthzid
  :: Maybe Text
  -- ^ Authorization identity; 'Nothing' keeps it empty.
  -> Text
  -- ^ Username
  -> Text
  -- ^ Password
  -> ByteString
generatePlainAuthWithAuthzid mAuthzid username password =
  let authzidBytes = maybe BS.empty encodeUtf8 mAuthzid
      nul = BS.singleton 0 :: ByteString
      usernameBytes = encodeUtf8 username
      passwordBytes = encodeUtf8 password
  in BS.concat [authzidBytes, nul, usernameBytes, nul, passwordBytes]


-- | Alias for 'generatePlainAuth' for clarity.
plainAuthData :: Text -> Text -> ByteString
plainAuthData = generatePlainAuth
