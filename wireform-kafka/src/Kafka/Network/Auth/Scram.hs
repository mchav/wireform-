{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Network.Auth.Scram
Description : SASL/SCRAM authentication implementation
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

SASL/SCRAM (Salted Challenge Response Authentication Mechanism) provides
secure password-based authentication using challenge-response.

SCRAM protects against:
* Eavesdropping (password never sent over wire)
* Replay attacks (nonces prevent reuse)
* Dictionary attacks (salted and iterated hashing)

SCRAM authentication involves multiple round trips:

1. Client-first message: username + nonce
2. Server-first message: salt + iteration count + server nonce
3. Client-final message: proof
4. Server-final message: verification

Kafka supports SCRAM-SHA-256 and SCRAM-SHA-512.

See RFC 5802 for the complete SCRAM specification.
-}
module Kafka.Network.Auth.Scram
  ( -- * SCRAM Types
    ScramHashAlgorithm(..)
  , ScramState(..)
    -- * SCRAM Message Generation
  , generateClientFirstMessage
  , generateClientFinalMessage
  , parseServerFirstMessage
  , parseServerFinalMessage
    -- * SCRAM Utilities
  , generateNonce
  , scramSha256
  , scramSha512
  ) where

import Crypto.Hash (SHA256, SHA512, Digest, hash)
import Crypto.MAC.HMAC (HMAC, hmac)
import Crypto.Random (getRandomBytes)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word (Word8)

-- | SCRAM hash algorithm variant.
data ScramHashAlgorithm
  = ScramSHA256  -- ^ SCRAM-SHA-256 (recommended)
  | ScramSHA512  -- ^ SCRAM-SHA-512 (stronger, slower)
  deriving (Eq, Show)

-- | SCRAM authentication state tracking.
data ScramState = ScramState
  { scramUsername      :: !Text
    -- ^ Username being authenticated
  , scramPassword      :: !Text
    -- ^ Password (kept for proof generation)
  , scramClientNonce   :: !ByteString
    -- ^ Client-generated nonce
  , scramServerNonce   :: !(Maybe ByteString)
    -- ^ Server nonce (from server-first message)
  , scramSalt          :: !(Maybe ByteString)
    -- ^ Salt from server
  , scramIterations    :: !(Maybe Int)
    -- ^ Iteration count from server
  , scramHashAlgorithm :: !ScramHashAlgorithm
    -- ^ Hash algorithm being used
  } deriving (Eq, Show)

-- | Generate a cryptographically random nonce for SCRAM.
-- Nonces should be at least 16 bytes.
generateNonce :: IO ByteString
generateNonce = getRandomBytes 24

-- | Generate the client-first message in SCRAM authentication.
--
-- Format: n,,n=username,r=nonce
--
-- Where:
--   - n,, indicates no channel binding
--   - n=username is the username
--   - r=nonce is the client nonce (base64-encoded random bytes)
generateClientFirstMessage
  :: Text        -- ^ Username
  -> ByteString  -- ^ Client nonce
  -> ByteString
generateClientFirstMessage username clientNonce =
  let header = "n,,"  -- No channel binding
      usernamePart = "n=" <> encodeUtf8 username
      noncePart = "r=" <> B64.encode clientNonce
      message = BS.intercalate "," [usernamePart, noncePart]
  in encodeUtf8 (T.pack header) <> message

-- | Parse server-first message in SCRAM authentication.
--
-- Format: r=nonce,s=salt,i=iterations
--
-- Returns: (combined nonce, salt, iteration count)
--
-- TODO: Implement proper parsing of server-first message
-- Should extract:
--   - r=nonce (server nonce appended to client nonce)
--   - s=salt (base64-encoded salt)
--   - i=iterations (iteration count for PBKDF2)
parseServerFirstMessage
  :: ByteString
  -> Either String (ByteString, ByteString, Int)
parseServerFirstMessage serverFirst = do
  -- TODO: Parse server-first message format
  -- Expected format: r=<combined-nonce>,s=<base64-salt>,i=<iterations>
  Left "Server-first message parsing not yet implemented"
    <> Left "\nTODO: Parse attributes:"
    <> Left "\n  - Extract r= (combined nonce)"
    <> Left "\n  - Extract s= (base64 decode salt)"
    <> Left "\n  - Extract i= (parse iteration count)"

-- | Generate the client-final message in SCRAM authentication.
--
-- This is the most complex part of SCRAM, involving:
-- 1. PBKDF2 key derivation
-- 2. HMAC computation for proof
-- 3. Encoding the final message
--
-- TODO: Implement client-final message generation
-- Requires:
--   - PBKDF2 implementation for key derivation
--   - HMAC calculations for client proof
--   - Proper message formatting
generateClientFinalMessage
  :: ScramState
  -> ByteString  -- ^ Server nonce
  -> ByteString  -- ^ Salt
  -> Int         -- ^ Iterations
  -> Either String ByteString
generateClientFinalMessage state serverNonce salt iterations = do
  -- TODO: Implement client-final message
  -- Steps:
  --   1. Derive SaltedPassword using PBKDF2
  --   2. Compute ClientKey = HMAC(SaltedPassword, "Client Key")
  --   3. Compute StoredKey = H(ClientKey)
  --   4. Compute AuthMessage = client-first-bare + "," + server-first + "," + client-final-without-proof
  --   5. Compute ClientSignature = HMAC(StoredKey, AuthMessage)
  --   6. Compute ClientProof = ClientKey XOR ClientSignature
  --   7. Format message: c=<channel-binding>,r=<nonce>,p=<base64-proof>
  Left "Client-final message generation not yet implemented"

-- | Parse server-final message in SCRAM authentication.
--
-- Format: v=signature (on success) or e=error (on failure)
--
-- TODO: Implement server-final message parsing
parseServerFinalMessage
  :: ByteString
  -> Either String ByteString
parseServerFinalMessage serverFinal = do
  -- TODO: Parse server-final message
  -- Check for v= (server signature, success) or e= (error message)
  Left "Server-final message parsing not yet implemented"

-- | SCRAM-SHA-256 authentication.
--
-- TODO: Implement complete SCRAM-SHA-256 flow
scramSha256
  :: Text  -- ^ Username
  -> Text  -- ^ Password
  -> IO (Either String ScramState)
scramSha256 username password = do
  clientNonce <- generateNonce
  return $ Right $ ScramState
    { scramUsername = username
    , scramPassword = password
    , scramClientNonce = clientNonce
    , scramServerNonce = Nothing
    , scramSalt = Nothing
    , scramIterations = Nothing
    , scramHashAlgorithm = ScramSHA256
    }

-- | SCRAM-SHA-512 authentication.
--
-- TODO: Implement complete SCRAM-SHA-512 flow
scramSha512
  :: Text  -- ^ Username
  -> Text  -- ^ Password
  -> IO (Either String ScramState)
scramSha512 username password = do
  clientNonce <- generateNonce
  return $ Right $ ScramState
    { scramUsername = username
    , scramPassword = password
    , scramClientNonce = clientNonce
    , scramServerNonce = Nothing
    , scramSalt = Nothing
    , scramIterations = Nothing
    , scramHashAlgorithm = ScramSHA512
    }

