{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Kafka.Network.Auth.Scram
Description : SASL\/SCRAM-SHA-256 \/ SCRAM-SHA-512 (RFC 5802)

Concrete implementation of the Salted Challenge Response
Authentication Mechanism, the SHA-256 \/ SHA-512 variants that
Kafka brokers expose via the standard SASL handshake.

Wire format (RFC 5802 §5):

@
client-first-bare = \"n=\" username \",r=\" client-nonce
client-first      = gs2-header client-first-bare         -- gs2-header is \"n,,\"
server-first      = \"r=\" full-nonce \",s=\" salt-b64 \",i=\" iterations
client-final-no-proof
                  = \"c=\" channel-binding-b64 \",r=\" full-nonce
client-final      = client-final-no-proof \",p=\" client-proof-b64
server-final      = \"v=\" server-signature-b64    -- success
                  | \"e=\" sasl-error              -- failure
@

Cryptography (RFC 5802 §3 / §5.1, applied verbatim):

@
SaltedPassword  = PBKDF2-HMAC-H(password, salt, iterations, hLen)
ClientKey       = HMAC(SaltedPassword, \"Client Key\")
StoredKey       = H(ClientKey)
AuthMessage     = client-first-bare \",\" server-first \",\" client-final-no-proof
ClientSignature = HMAC(StoredKey, AuthMessage)
ClientProof     = ClientKey XOR ClientSignature
ServerKey       = HMAC(SaltedPassword, \"Server Key\")
ServerSignature = HMAC(ServerKey, AuthMessage)
@

Round-trips with the broker:

  1. Send 'firstClientMessage'.
  2. Receive a server-first message; feed it to 'finalClientMessage'
     to get the proof bytes plus a ScramVerifier closure.
  3. Send the proof.
  4. Receive a server-final message; feed it to ScramVerifier; the
     server proves it knows the password by returning the same
     ServerSignature we computed locally.

The whole thing is pure aside from 'newScramSession', which generates
the client nonce.
-}
module Kafka.Network.Auth.Scram
  ( -- * Algorithm selection
    ScramAlgo(..)
  , algoMechanismName
    -- * Session
  , ScramSession(..)
  , ScramVerifier
  , newScramSession
  , firstClientMessage
  , finalClientMessage
  , verifyServerFinal
    -- * Internal helpers (exported for tests)
  , saltedPassword
  , parseServerFirst
  , ServerFirst(..)
  , scramHmac
  , scramHash
  ) where

import qualified Crypto.Hash as H
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Crypto.MAC.HMAC as HMAC
import qualified Crypto.Random as R
import Data.Bits (xor)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

------------------------------------------------------------------------
-- Algorithm selection
------------------------------------------------------------------------

-- | Which SCRAM hash variant to use. Kafka brokers commonly expose
-- both; SHA-512 is preferred where available.
data ScramAlgo = ScramSHA256 | ScramSHA512
  deriving (Eq, Show)

-- | Wire-level mechanism name the broker advertises in its
-- @SaslHandshakeResponse@.
algoMechanismName :: ScramAlgo -> Text
algoMechanismName ScramSHA256 = "SCRAM-SHA-256"
algoMechanismName ScramSHA512 = "SCRAM-SHA-512"

------------------------------------------------------------------------
-- Hash / HMAC helpers polymorphic in the SCRAM algorithm
------------------------------------------------------------------------

scramHash :: ScramAlgo -> ByteString -> ByteString
scramHash ScramSHA256 bs = convert (H.hash bs :: H.Digest H.SHA256)
scramHash ScramSHA512 bs = convert (H.hash bs :: H.Digest H.SHA512)

scramHmac :: ScramAlgo -> ByteString -> ByteString -> ByteString
scramHmac ScramSHA256 key msg =
  convert (HMAC.hmac key msg :: HMAC.HMAC H.SHA256)
scramHmac ScramSHA512 key msg =
  convert (HMAC.hmac key msg :: HMAC.HMAC H.SHA512)

-- | PBKDF2 with the right hash for the chosen SCRAM algorithm. Output
-- length is the algorithm's native digest length (32 bytes for SHA-256,
-- 64 bytes for SHA-512), which is what RFC 5802 calls @hLen@.
saltedPassword :: ScramAlgo -> ByteString -> ByteString -> Int -> ByteString
saltedPassword algo password salt iters =
  let prfParams = PBKDF2.Parameters
        { PBKDF2.iterCounts = iters
        , PBKDF2.outputLength = case algo of
            ScramSHA256 -> 32
            ScramSHA512 -> 64
        }
      prf = case algo of
        ScramSHA256 -> PBKDF2.prfHMAC H.SHA256
        ScramSHA512 -> PBKDF2.prfHMAC H.SHA512
  in PBKDF2.generate prf prfParams password salt

------------------------------------------------------------------------
-- Session
------------------------------------------------------------------------

-- | In-flight SCRAM session. Created by 'newScramSession', consumed
-- by 'firstClientMessage' / 'finalClientMessage' / 'verifyServerFinal'.
data ScramSession = ScramSession
  { ssAlgo        :: !ScramAlgo
  , ssUsername    :: !Text
  , ssPassword    :: !Text
  , ssClientNonce :: !ByteString
    -- ^ Random base64-printable client nonce. RFC 5802 only requires
    --   "printable" — we generate 24 random bytes and base64-encode
    --   them so the result is ASCII-safe and 32 chars long.
  } deriving (Eq, Show)

-- | Server's first reply, parsed.
data ServerFirst = ServerFirst
  { sfFullNonce  :: !ByteString  -- ^ client-nonce ++ server-nonce
  , sfSalt       :: !ByteString  -- ^ raw bytes (base64-decoded)
  , sfIterations :: !Int
  } deriving (Eq, Show)

-- | A closure produced by 'finalClientMessage' that the SASL driver
-- must feed the server's final message into. Returns @Right ()@ iff
-- the server's signature matches what we expect, otherwise an error
-- describing the mismatch.
type ScramVerifier = ByteString -> Either String ()

-- | Generate a fresh SCRAM session with a 32-character random
-- client nonce.
newScramSession :: ScramAlgo -> Text -> Text -> IO ScramSession
newScramSession algo user pwd = do
  nonceBytes :: ByteString <- R.getRandomBytes 24
  pure ScramSession
    { ssAlgo        = algo
    , ssUsername    = user
    , ssPassword    = pwd
    , ssClientNonce = B64.encode nonceBytes
    }

------------------------------------------------------------------------
-- Wire message construction / parsing
------------------------------------------------------------------------

-- | Build the @client-first@ SASL token (RFC 5802 §5.1).
firstClientMessage :: ScramSession -> ByteString
firstClientMessage ScramSession{..} =
  "n,," <> clientFirstBare ssUsername ssClientNonce

-- | The bare body of the client-first message, used as part of the
-- @AuthMessage@ that the proof is computed over.
clientFirstBare :: Text -> ByteString -> ByteString
clientFirstBare user nonce =
  "n=" <> saslEscape (TE.encodeUtf8 user) <> ",r=" <> nonce

-- | Build the @client-final@ SASL token, returning the bytes to send
-- and a closure for verifying the server's eventual final message.
--
-- Pre-condition: the server's @r=@ MUST start with the bytes we sent
-- in the client-first nonce (RFC 5802 §5.1). We enforce this here.
finalClientMessage
  :: ScramSession
  -> ByteString  -- ^ Raw server-first message (the bytes as received)
  -> Either String (ByteString, ScramVerifier)
finalClientMessage session@ScramSession{..} serverFirstRaw = do
  sf <- parseServerFirst serverFirstRaw
  let fullNonce = sfFullNonce sf
  -- Defence in depth — never send our proof if the server has rotated
  -- our nonce on us; that would let a MITM hijack the session.
  if not (BS.isPrefixOf ssClientNonce fullNonce)
    then Left "SCRAM: server nonce did not extend the client nonce"
    else do
      let algo            = ssAlgo
          channelBinding  = B64.encode "n,,"  -- gs2 header, base64'd
          clientFinalNoP  =
                "c=" <> channelBinding <> ",r=" <> fullNonce
          authMessage     =
                clientFirstBare ssUsername ssClientNonce
            <> ","
            <> serverFirstRaw
            <> ","
            <> clientFinalNoP

          saltedPwd       = saltedPassword algo
                              (TE.encodeUtf8 ssPassword)
                              (sfSalt sf)
                              (sfIterations sf)
          clientKey       = scramHmac algo saltedPwd "Client Key"
          storedKey       = scramHash algo clientKey
          clientSig       = scramHmac algo storedKey authMessage
          clientProof     = bytesXor clientKey clientSig
          serverKey       = scramHmac algo saltedPwd "Server Key"
          serverSig       = scramHmac algo serverKey authMessage

          clientFinal     = clientFinalNoP <> ",p=" <> B64.encode clientProof

          verifier raw = verifyServerFinal serverSig raw
      Right (clientFinal, verifier)

-- | Independent server-final verification. Exposed in case the SASL
-- driver wants to keep the verifier separate from the message it just
-- sent — the closure returned by 'finalClientMessage' calls into here.
verifyServerFinal
  :: ByteString  -- ^ Expected ServerSignature (raw bytes)
  -> ByteString  -- ^ Raw server-final message ("v=...\" or "e=...\")
  -> Either String ()
verifyServerFinal expected raw =
  case BS.splitWith (== c ',') raw of
    (pair:_) -> case BS.splitAt 2 pair of
      ("v=", b64) -> case B64.decode b64 of
        Left err  -> Left ("SCRAM: server signature not valid base64: " <> err)
        Right got
          | got == expected -> Right ()
          | otherwise -> Left "SCRAM: server signature did not verify"
      ("e=", err) -> Left ("SCRAM: server reported error: " <> BS8.unpack err)
      _ -> Left ("SCRAM: malformed server-final message: "
                  <> BS8.unpack raw)
    _ -> Left "SCRAM: empty server-final message"
  where
    c x = fromIntegral (fromEnum x)

-- | Parse a @server-first@ message. We accept extension attributes in
-- any order even though the standard puts them as @r,s,i[,...]@.
parseServerFirst :: ByteString -> Either String ServerFirst
parseServerFirst bs = do
  let attrs = BS.splitWith (== c ',') bs
      kvs   = [(BS.take 1 a, BS.drop 2 a) | a <- attrs, BS.length a >= 2]
  nonce <- requiredAttr "r" kvs
  saltB <- requiredAttr "s" kvs
  itersB <- requiredAttr "i" kvs
  salt  <- case B64.decode saltB of
    Right s -> Right s
    Left err -> Left ("SCRAM: invalid base64 salt: " <> err)
  iters <- parseDecimal itersB
  Right ServerFirst
    { sfFullNonce  = nonce
    , sfSalt       = salt
    , sfIterations = iters
    }
  where
    c x = fromIntegral (fromEnum x)
    requiredAttr k kvs =
      case lookup k kvs of
        Just v  -> Right v
        Nothing -> Left ("SCRAM: server-first missing required attribute "
                         <> BS8.unpack k)
    parseDecimal s
      | BS.null s = Left "SCRAM: empty iteration count"
      | not (BS.all (isDigit . toChar) s) =
          Left ("SCRAM: non-numeric iteration count: " <> BS8.unpack s)
      | otherwise = Right (read (BS8.unpack s))
    toChar w = toEnum (fromIntegral w)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | RFC 5802 §5.1: SASLprep is a "MUST" but in practice SCRAM-SHA-*
-- implementations only need to escape "," and "=" inside the username
-- attribute value, since those are the field separators.
saslEscape :: ByteString -> ByteString
saslEscape =
  let escapeByte w
        | w == c '=' = "=3D"
        | w == c ',' = "=2C"
        | otherwise  = BS.singleton w
      c x = fromIntegral (fromEnum x)
  in BS.concatMap escapeByte

-- | Byte-wise XOR; both inputs must be the same length (RFC 5802 §3
-- guarantees they are: both are hLen bytes).
bytesXor :: ByteString -> ByteString -> ByteString
bytesXor a b = BS.pack (BS.zipWith xor a b)
