{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Kafka.Network.Auth.AwsMskIam
Description : AWS MSK IAM SASL mechanism (\@AWS_MSK_IAM\@)

AWS MSK (Managed Streaming for Apache Kafka) supports IAM-based
authentication via a custom SASL mechanism named @AWS_MSK_IAM@. The
mechanism is documented in the @aws-msk-iam-auth@ Java library
(<https://github.com/aws/aws-msk-iam-auth>) and works as follows:

  1. Client and broker negotiate the @AWS_MSK_IAM@ mechanism via the
     usual Kafka SASL handshake.
  2. The client computes a /presigned URL/ (SigV4) for the action
     @kafka-cluster:Connect@ against the broker host, packages the
     resulting query parameters as a flat JSON object, and sends that
     JSON as the SASL authentication payload.
  3. The broker validates the SigV4 signature against IAM and responds
     with an empty payload on success.

This module is the implementation of step 2 — given an
'AwsCredentials' value, the broker host, the AWS region and the
current time it produces the JSON payload bytes that go on the wire.

The full SASL handshake driver in "Kafka.Network.Auth.SASL" calls
'buildIamPayloadIO' once on first authenticate; refreshed credentials
require a reconnect today (matches the official Java client's
behaviour).
-}
module Kafka.Network.Auth.AwsMskIam (
  -- * Credentials
  AwsCredentials (..),
  AwsCredentialsProvider (..),
  resolveAwsCredentials,
  awsCredentialsFromEnv,

  -- * Payload
  IamPayloadInput (..),
  buildIamPayload,
  buildIamPayloadIO,

  -- * Internal helpers (exported for tests)
  canonicalQueryString,
  stringToSign,
  signingKey,
  sigV4Signature,
  urlEncode,
) where

import Crypto.Hash qualified as H
import Crypto.MAC.HMAC qualified as HMAC
import Data.Aeson (object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteArray (convert)
import Data.ByteArray.Encoding qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.List (sortOn)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format qualified as Time
import Data.Word qualified as W
import System.Environment qualified as Env


------------------------------------------------------------------------
-- Credentials
------------------------------------------------------------------------

{- | A resolved AWS credentials triple. @awsSessionToken@ is non-Nothing
when the credentials came from STS (assumed roles, instance metadata,
@AWS_SESSION_TOKEN@ env var); when present it must also be included
in the canonical request as @X-Amz-Security-Token@.
-}
data AwsCredentials = AwsCredentials
  { awsAccessKeyId :: !Text
  , awsSecretAccessKey :: !Text
  , awsSessionToken :: !(Maybe Text)
  }
  deriving (Eq, Show)


{- | Pluggable credential resolver. The IAM SASL mechanism calls
'resolveAwsCredentials' once per connection; provide a custom
'AwsCustomProvider' if you want to integrate with an SDK chain
(instance metadata, EKS pod-identity, role assumption, ...).
-}
data AwsCredentialsProvider
  = {- | Use a fixed credentials triple. Easiest, but rotates on
    nothing — best only for short-lived utilities or local
    testing.
    -}
    AwsStaticCredentials !AwsCredentials
  | {- | Read @AWS_ACCESS_KEY_ID@ \/ @AWS_SECRET_ACCESS_KEY@ \/
    @AWS_SESSION_TOKEN@ from the process environment. Returns
    an error string if the required vars aren't set.
    -}
    AwsEnvCredentials
  | {- | User-supplied IO action that returns the live credentials.
    The value is re-resolved each time the SASL handshake runs
    (i.e. once per broker connection).
    -}
    AwsCustomProvider !(IO (Either String AwsCredentials))


-- | Run a credential provider.
resolveAwsCredentials :: AwsCredentialsProvider -> IO (Either String AwsCredentials)
resolveAwsCredentials provider = case provider of
  AwsStaticCredentials c -> pure (Right c)
  AwsEnvCredentials -> awsCredentialsFromEnv
  AwsCustomProvider io -> io


{- | Read @AWS_ACCESS_KEY_ID@, @AWS_SECRET_ACCESS_KEY@ and (optionally)
@AWS_SESSION_TOKEN@ from the environment.
-}
awsCredentialsFromEnv :: IO (Either String AwsCredentials)
awsCredentialsFromEnv = do
  k <- Env.lookupEnv "AWS_ACCESS_KEY_ID"
  s <- Env.lookupEnv "AWS_SECRET_ACCESS_KEY"
  t <- Env.lookupEnv "AWS_SESSION_TOKEN"
  case (k, s) of
    (Just kk, Just ss) ->
      pure $
        Right
          AwsCredentials
            { awsAccessKeyId = T.pack kk
            , awsSecretAccessKey = T.pack ss
            , awsSessionToken = T.pack <$> t
            }
    _ ->
      pure $
        Left
          "AWS credentials: AWS_ACCESS_KEY_ID and \
          \AWS_SECRET_ACCESS_KEY env vars must be set \
          \(or use a different AwsCredentialsProvider)."


------------------------------------------------------------------------
-- Payload
------------------------------------------------------------------------

{- | Bag of inputs that 'buildIamPayload' needs. The split lets us
inject a fixed 'iiNow' from tests without an IO call.
-}
data IamPayloadInput = IamPayloadInput
  { iiCredentials :: !AwsCredentials
  , iiHost :: !Text
  -- ^ Broker hostname (no port)
  , iiRegion :: !Text
  -- ^ e.g. \"us-east-1\"
  , iiNow :: !UTCTime
  -- ^ "Now" in UTC; the signature is valid for 'iiExpires' seconds from this point
  , iiUserAgent :: !Text
  -- ^ Sent verbatim in the JSON; defaults are fine
  , iiExpires :: !Int
  -- ^ X-Amz-Expires; max 900 per AWS limits, default 900
  }
  deriving (Eq, Show)


{- | The wire bytes that the SASL driver should send as the initial
(and only) @SaslAuthenticateRequest.authBytes@ for AWS_MSK_IAM.
-}
buildIamPayload :: IamPayloadInput -> ByteString
buildIamPayload IamPayloadInput {..} =
  let datestamp = formatDate iiNow
      amzDate = formatAmzDate iiNow
      credScope =
        T.intercalate
          "/"
          [datestamp, iiRegion, "kafka-cluster", "aws4_request"]
      credential = awsAccessKeyId iiCredentials <> "/" <> credScope
      cqs =
        canonicalQueryString
          credential
          amzDate
          iiExpires
          (awsSessionToken iiCredentials)
      sts = stringToSign amzDate credScope (canonicalRequest iiHost cqs)
      signKey =
        signingKey
          (awsSecretAccessKey iiCredentials)
          datestamp
          iiRegion
          "kafka-cluster"
      sig = sigV4Signature signKey sts

      pairs =
        [ "version" .= ("2020_10_22" :: Text)
        , "host" .= iiHost
        , "user-agent" .= iiUserAgent
        , "action" .= ("kafka-cluster:Connect" :: Text)
        , "x-amz-algorithm" .= ("AWS4-HMAC-SHA256" :: Text)
        , "x-amz-credential" .= credential
        , "x-amz-date" .= amzDate
        , "x-amz-signedheaders" .= ("host" :: Text)
        , "x-amz-expires" .= T.pack (show iiExpires)
        , "x-amz-signature" .= TE.decodeUtf8 sig
        ]
          <> [ "x-amz-security-token" .= tok
             | tok <- maybeToList (awsSessionToken iiCredentials)
             ]
  in BL.toStrict $ Aeson.encode (object pairs)


{- | Convenience wrapper that resolves credentials and reads the wall
clock for you. The host/region/user-agent/expiry come from the
caller because they're per-broker / per-config concerns.
-}
buildIamPayloadIO
  :: AwsCredentialsProvider
  -> Text
  -- ^ Broker host
  -> Text
  -- ^ AWS region
  -> Text
  -- ^ User agent
  -> Int
  -- ^ Expires (seconds)
  -> IO (Either String ByteString)
buildIamPayloadIO provider host region ua expires = do
  credR <- resolveAwsCredentials provider
  case credR of
    Left err -> pure (Left err)
    Right cred -> do
      now <- getCurrentTime
      let input =
            IamPayloadInput
              { iiCredentials = cred
              , iiHost = host
              , iiRegion = region
              , iiNow = now
              , iiUserAgent = ua
              , iiExpires = expires
              }
      pure (Right (buildIamPayload input))


------------------------------------------------------------------------
-- SigV4 internals (extracted for testability)
------------------------------------------------------------------------

{- | The fixed canonical request the AWS_MSK_IAM mechanism signs:
always GET, always path "/", always the X-Amz-* presigned query
string, always a single Host header.
-}
canonicalRequest :: Text -> ByteString -> ByteString
canonicalRequest host cqs =
  BS.intercalate
    "\n"
    [ "GET"
    , "/"
    , cqs
    , "host:" <> TE.encodeUtf8 (T.toLower host)
    , "" -- empty line after canonical headers
    , "host" -- signed headers
    , hexHash "" -- empty body
    ]


{- | Build the canonical query string for a presigned URL. AWS requires
the parameters be sorted lexicographically by key after URL-encoding.
-}
canonicalQueryString
  :: Text
  -- ^ Credential value: access-key/credential-scope
  -> Text
  -- ^ X-Amz-Date in YYYYMMDDTHHMMSSZ
  -> Int
  -- ^ X-Amz-Expires
  -> Maybe Text
  -- ^ Optional X-Amz-Security-Token
  -> ByteString
canonicalQueryString credential amzDate expires sessionToken =
  let pairs0 :: [(ByteString, ByteString)]
      pairs0 =
        [ ("Action", urlEncode (TE.encodeUtf8 "kafka-cluster:Connect"))
        , ("X-Amz-Algorithm", urlEncode (TE.encodeUtf8 "AWS4-HMAC-SHA256"))
        , ("X-Amz-Credential", urlEncode (TE.encodeUtf8 credential))
        , ("X-Amz-Date", urlEncode (TE.encodeUtf8 amzDate))
        , ("X-Amz-Expires", urlEncode (TE.encodeUtf8 (T.pack (show expires))))
        , ("X-Amz-SignedHeaders", urlEncode (TE.encodeUtf8 "host"))
        ]
      pairs =
        pairs0 <> case sessionToken of
          Nothing -> []
          Just tok -> [("X-Amz-Security-Token", urlEncode (TE.encodeUtf8 tok))]
      sorted = sortOn fst pairs
  in BS.intercalate
       "&"
       [k <> "=" <> v | (k, v) <- sorted]


{- | The "string to sign" (AWS SigV4 §3.2.4): the algorithm, the
request timestamp, the credential scope and a hex-encoded SHA-256
of the canonical request.
-}
stringToSign
  :: Text
  -- ^ amzDate
  -> Text
  -- ^ Credential scope
  -> ByteString
  -- ^ Canonical request bytes
  -> ByteString
stringToSign amzDate credScope canonReq =
  BS.intercalate
    "\n"
    [ "AWS4-HMAC-SHA256"
    , TE.encodeUtf8 amzDate
    , TE.encodeUtf8 credScope
    , hexHash canonReq
    ]


{- | The four-step HMAC chain that turns a secret key into the daily
signing key (AWS SigV4 §3.3).
-}
signingKey
  :: Text
  -- ^ Secret access key
  -> Text
  -- ^ Datestamp YYYYMMDD
  -> Text
  -- ^ Region
  -> Text
  -- ^ Service (always \"kafka-cluster\" for AWS_MSK_IAM)
  -> ByteString
signingKey secret datestamp region service =
  let kSecret = TE.encodeUtf8 ("AWS4" <> secret)
      kDate = hmacSha256Bytes kSecret (TE.encodeUtf8 datestamp)
      kRegion = hmacSha256Bytes kDate (TE.encodeUtf8 region)
      kService = hmacSha256Bytes kRegion (TE.encodeUtf8 service)
      kSigning = hmacSha256Bytes kService "aws4_request"
  in kSigning


{- | Final SigV4 signature: hex-encoded HMAC-SHA256 of the
string-to-sign under the daily signing key.
-}
sigV4Signature :: ByteString -> ByteString -> ByteString
sigV4Signature key msg = hexLower (hmacSha256Bytes key msg)


------------------------------------------------------------------------
-- Crypto / encoding shims
------------------------------------------------------------------------

hmacSha256Bytes :: ByteString -> ByteString -> ByteString
hmacSha256Bytes key msg =
  convert (HMAC.hmac key msg :: HMAC.HMAC H.SHA256)


hexHash :: ByteString -> ByteString
hexHash = hexLower . sha256


sha256 :: ByteString -> ByteString
sha256 bs = convert (H.hash bs :: H.Digest H.SHA256)


hexLower :: ByteString -> ByteString
hexLower = BA.convertToBase BA.Base16


formatDate :: UTCTime -> Text
formatDate = T.pack . Time.formatTime Time.defaultTimeLocale "%Y%m%d"


formatAmzDate :: UTCTime -> Text
formatAmzDate = T.pack . Time.formatTime Time.defaultTimeLocale "%Y%m%dT%H%M%SZ"


{- | RFC 3986 percent-encoding tuned for AWS canonical query strings:
everything except @A-Z@, @a-z@, @0-9@ and @-_.~@ is encoded.
-}
urlEncode :: ByteString -> ByteString
urlEncode = BS.concatMap encodeByte
  where
    encodeByte :: W.Word8 -> ByteString
    encodeByte w
      | unreserved w = BS.singleton w
      | otherwise = BS8.pack ('%' : hex w)

    unreserved :: W.Word8 -> Bool
    unreserved w =
      (w >= c '0' && w <= c '9')
        || (w >= c 'A' && w <= c 'Z')
        || (w >= c 'a' && w <= c 'z')
        || w == c '-'
        || w == c '_'
        || w == c '.'
        || w == c '~'

    c x = fromIntegral (fromEnum x) :: W.Word8

    hex :: W.Word8 -> String
    hex w =
      let lo = fromIntegral (w `mod` 16) :: Int
          hi = fromIntegral (w `div` 16) :: Int
      in [hexChar hi, hexChar lo]

    hexChar :: Int -> Char
    hexChar n
      | n < 10 = toEnum (fromEnum '0' + n)
      | otherwise = toEnum (fromEnum 'A' + n - 10)
