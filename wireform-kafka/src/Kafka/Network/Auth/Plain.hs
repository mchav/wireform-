{-|
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
module Kafka.Network.Auth.Plain
  ( generatePlainAuth
  , plainAuthData
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)

-- | Generate SASL/PLAIN authentication data.
--
-- The format is: [authzid] \\0 username \\0 password
-- For Kafka, authzid is typically empty.
--
-- Example:
--
-- > let authData = generatePlainAuth "alice" "secret123"
-- > -- Sends: \\0alice\\0secret123
generatePlainAuth
  :: Text      -- ^ Username
  -> Text      -- ^ Password
  -> ByteString
generatePlainAuth username password =
  let authzid = BS.empty  -- Empty authorization identity
      nul = BS.singleton 0 :: ByteString
      usernameBytes = encodeUtf8 username
      passwordBytes = encodeUtf8 password
  in BS.concat [authzid, nul, usernameBytes, nul, passwordBytes]

-- | Alias for 'generatePlainAuth' for clarity.
plainAuthData :: Text -> Text -> ByteString
plainAuthData = generatePlainAuth

