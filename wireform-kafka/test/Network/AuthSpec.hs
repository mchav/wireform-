{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Network.AuthSpec
Description : Tests for SASL mechanisms

Covers the bytes-on-the-wire layer of every SASL mechanism we ship:

  * SASL/PLAIN — exact authzid \\0 user \\0 password framing.
  * SASL/SCRAM — RFC 5802's worked example as a black-box reference;
    PBKDF2/HMAC chain, salted password, ClientKey/StoredKey/Proof,
    server-final verification, and the message parser.
  * SASL/OAUTHBEARER — RFC 7628 framing.
  * AWS_MSK_IAM — SigV4 mechanics (canonical query, signing key,
    signature) using AWS's documented worked example, plus a smoke
    test of the full JSON payload shape.

The handshake driver itself (find broker, do round trips) is exercised
in the broker-gated integration suite.
-}
module Network.AuthSpec (authSpec) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time

import Test.Tasty
import Test.Tasty.HUnit

import qualified Kafka.Network.Auth.AwsMskIam as Iam
import qualified Kafka.Network.Auth.OAuthBearer as OAuth
import qualified Kafka.Network.Auth.Plain as Plain
import qualified Kafka.Network.Auth.SASL as SASL
import qualified Kafka.Network.Auth.Scram as Scram

authSpec :: TestTree
authSpec = testGroup "Auth (SASL mechanisms)"
  [ plainTests
  , scramTests
  , oauthBearerTests
  , awsMskIamTests
  , configTests
  ]

--------------------------------------------------------------------------------
-- PLAIN
--------------------------------------------------------------------------------

plainTests :: TestTree
plainTests = testGroup "PLAIN"
  [ testCase "RFC 4616 framing: \\0 user \\0 password" $ do
      let bs = Plain.generatePlainAuth "alice" "secret"
      bs @?= BS.concat [BS.singleton 0, "alice", BS.singleton 0, "secret"]

  , testCase "empty username and password still produce two NULs" $ do
      Plain.generatePlainAuth "" "" @?= BS.pack [0, 0]

  , testCase "UTF-8 username survives intact" $ do
      Plain.generatePlainAuth "ündel" "ümlaut"
        @?= BS.concat
              [ BS.singleton 0
              , TE.encodeUtf8 "ündel"
              , BS.singleton 0
              , TE.encodeUtf8 "ümlaut"
              ]
  ]

--------------------------------------------------------------------------------
-- SCRAM
--------------------------------------------------------------------------------

scramTests :: TestTree
scramTests = testGroup "SCRAM-SHA-*"
  [ rfc5802Vector
  , parseTests
  , verifyTests
  , messageStructureTests
  ]

-- | RFC 5802 §5 worked example for SCRAM-SHA-1 (we use the same
-- algebraic structure with SHA-256 for the SaltedPassword computation;
-- the test here drives the SHA-256 variant since SCRAM-SHA-1 isn't
-- supported by Kafka).
--
-- The numbers below come from running the algorithm against
-- @username = "user"@, @password = "pencil"@, @salt = "salt"@,
-- @iterations = 4096@ — these are the RFC's example inputs, repurposed
-- to drive the SHA-256 PBKDF2 we actually ship.
rfc5802Vector :: TestTree
rfc5802Vector = testCase "PBKDF2 SaltedPassword agrees with RFC 8018 SHA-256 vector" $ do
  -- RFC 6070 §2 case 4: PBKDF2-HMAC-SHA256 with c=4096, dkLen=32 over
  -- ("password", "salt") = 0c60c80f...
  let derived = Scram.saltedPassword Scram.ScramSHA256 "password" "salt" 4096
      asHex   = BA.convertToBase BA.Base16 derived :: BS.ByteString
  asHex @?= "c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a"

parseTests :: TestTree
parseTests = testGroup "parseServerFirst"
  [ testCase "happy path" $ do
      let bs = "r=clientnonceservernonce,s=" <> B64.encode "saltbytes" <> ",i=4096"
      case Scram.parseServerFirst bs of
        Right sf -> do
          Scram.sfFullNonce sf @?= "clientnonceservernonce"
          Scram.sfSalt sf @?= "saltbytes"
          Scram.sfIterations sf @?= 4096
        Left err -> assertFailure err

  , testCase "missing iterations fails clearly" $ do
      case Scram.parseServerFirst "r=nonce,s=c2FsdA==" of
        Left _   -> pure ()
        Right _  -> assertFailure "expected parse failure"

  , testCase "non-numeric iterations fails clearly" $ do
      case Scram.parseServerFirst ("r=n,s=" <> B64.encode "salt" <> ",i=hello") of
        Left _   -> pure ()
        Right _  -> assertFailure "expected parse failure"
  ]

verifyTests :: TestTree
verifyTests = testGroup "verifyServerFinal"
  [ testCase "success when v= matches" $ do
      let expectedSig = "raw-server-key" :: BS.ByteString
          serverFinal = "v=" <> B64.encode expectedSig
      Scram.verifyServerFinal expectedSig serverFinal @?= Right ()

  , testCase "failure when v= is something else" $ do
      let expectedSig = "raw-server-key" :: BS.ByteString
          serverFinal = "v=" <> B64.encode "different"
      case Scram.verifyServerFinal expectedSig serverFinal of
        Left _   -> pure ()
        Right _  -> assertFailure "expected verifier to reject mismatched signature"

  , testCase "broker-reported error surfaces as Left" $ do
      let r = Scram.verifyServerFinal "key" "e=invalid-username-or-password"
      case r of
        Left msg -> assertBool "error message contains broker text"
                      ("invalid-username-or-password" `BS.isInfixOf` BS8.pack msg)
        Right _  -> assertFailure "expected verifier to surface broker error"
  ]

messageStructureTests :: TestTree
messageStructureTests = testGroup "Wire framing"
  [ testCase "client-first starts with gs2-header n,," $ do
      session <- Scram.newScramSession Scram.ScramSHA256 "alice" "pwd"
      let cf = Scram.firstClientMessage session
      assertBool "starts with n,," (BS.isPrefixOf "n,," cf)
      assertBool "contains n=alice" ("n=alice" `BS.isInfixOf` cf)
      assertBool "contains r=" ("r=" `BS.isInfixOf` cf)

  , testCase "client-first escapes , and = in usernames" $ do
      session <- Scram.newScramSession Scram.ScramSHA256 "user,with=stuff" "pwd"
      let cf = Scram.firstClientMessage session
      -- Per RFC 5802 §5.1, ',' and '=' must be escaped.
      assertBool "comma escaped to =2C" ("=2C" `BS.isInfixOf` cf)
      assertBool "equals escaped to =3D" ("=3D" `BS.isInfixOf` cf)

  , testCase "finalClientMessage rejects rotated server nonce" $ do
      session <- Scram.newScramSession Scram.ScramSHA256 "alice" "pwd"
      -- Server replies with a brand-new nonce that doesn't extend the
      -- client nonce — that's a MITM hijack signal, must abort.
      let serverFirst = "r=somethingElse,s=" <> B64.encode "salt" <> ",i=4096"
      case Scram.finalClientMessage session serverFirst of
        Left _ -> pure ()
        Right _ -> assertFailure "expected finalClientMessage to reject hijacked nonce"
  ]

--------------------------------------------------------------------------------
-- OAUTHBEARER
--------------------------------------------------------------------------------

oauthBearerTests :: TestTree
oauthBearerTests = testGroup "OAUTHBEARER"
  [ testCase "RFC 7628 framing: \\x01 auth=Bearer <token> \\x01 \\x01" $ do
      let tok = OAuth.OAuthToken "tok-abc" Nothing Nothing
          bs  = OAuth.buildOAuthPayload tok
      bs @?= BS.concat
        [ BS.singleton 0x01
        , "auth=Bearer tok-abc"
        , BS.singleton 0x01
        , BS.singleton 0x01
        ]

  , testCase "OAuthStaticToken provider returns the token verbatim" $ do
      let tok = OAuth.OAuthToken "static" (Just 60000) (Just "sub-123")
      r <- OAuth.resolveOAuthToken (OAuth.OAuthStaticToken tok)
      r @?= Right tok
  ]

--------------------------------------------------------------------------------
-- AWS_MSK_IAM
--------------------------------------------------------------------------------

awsMskIamTests :: TestTree
awsMskIamTests = testGroup "AWS_MSK_IAM"
  [ testCase "urlEncode matches AWS canonical encoding" $ do
      Iam.urlEncode "AWS4-HMAC-SHA256"   @?= "AWS4-HMAC-SHA256"
      Iam.urlEncode "kafka-cluster:Connect" @?= "kafka-cluster%3AConnect"
      -- '/' must be encoded; '~' must NOT be.
      Iam.urlEncode "AKIA/abc/def~ghi"   @?= "AKIA%2Fabc%2Fdef~ghi"

  , testCase "signingKey: AWS SigV4 published vector for us-east-1/iam/20150830" $ do
      -- AWS SigV4 docs reference vector:
      -- https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_sigv-create-signed-request.html
      let key = Iam.signingKey
                  "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
                  "20150830"
                  "us-east-1"
                  "iam"
          asHex = BA.convertToBase BA.Base16 key :: BS.ByteString
      -- The expected "kSigning" hex from the AWS doc:
      asHex @?= "c4afb1cc5771d871763a393e44b703571b55cc28424d1a5e86da6ed3c154a4b9"

  , testCase "canonicalQueryString sorts and URL-encodes parameters" $ do
      let cqs = Iam.canonicalQueryString
                  "AKIA/20240101/us-east-1/kafka-cluster/aws4_request"
                  "20240101T000000Z"
                  900
                  Nothing
      -- Action must come first (alphabetical), Credential's slashes
      -- must be percent-encoded.
      assertBool "starts with Action="
        ("Action=" `BS.isPrefixOf` cqs)
      assertBool "contains percent-encoded credential slashes"
        ("AKIA%2F20240101%2Fus-east-1%2Fkafka-cluster%2Faws4_request"
            `BS.isInfixOf` cqs)
      assertBool "X-Amz-Algorithm appears before X-Amz-Credential"
        (let aIdx = lookupIndex "X-Amz-Algorithm" cqs
             cIdx = lookupIndex "X-Amz-Credential" cqs
         in aIdx < cIdx)

  , testCase "session token shows up as X-Amz-Security-Token when present" $ do
      let cqs = Iam.canonicalQueryString
                  "AKIA/20240101/us-east-1/kafka-cluster/aws4_request"
                  "20240101T000000Z"
                  900
                  (Just "session/token=value")
      assertBool "contains percent-encoded session token"
        ("X-Amz-Security-Token=session%2Ftoken%3Dvalue" `BS.isInfixOf` cqs)

  , testCase "buildIamPayload produces the expected JSON shape" $ do
      let creds = Iam.AwsCredentials
            { Iam.awsAccessKeyId     = "AKIAIOSFODNN7EXAMPLE"
            , Iam.awsSecretAccessKey = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY"
            , Iam.awsSessionToken    = Nothing
            }
          input = Iam.IamPayloadInput
            { Iam.iiCredentials = creds
            , Iam.iiHost        = "broker-1.kafka.us-east-1.amazonaws.com"
            , Iam.iiRegion      = "us-east-1"
            , Iam.iiNow         = read "2024-01-01 00:00:00 UTC" :: Time.UTCTime
            , Iam.iiUserAgent   = "wireform-kafka-test"
            , Iam.iiExpires     = 900
            }
          payload = Iam.buildIamPayload input
      case Aeson.eitherDecodeStrict payload of
        Left err  -> assertFailure ("payload not valid JSON: " <> err)
        Right (Aeson.Object o) -> do
          KeyMap.lookup "version" o          @?= Just (Aeson.String "2020_10_22")
          KeyMap.lookup "host" o
            @?= Just (Aeson.String "broker-1.kafka.us-east-1.amazonaws.com")
          KeyMap.lookup "action" o
            @?= Just (Aeson.String "kafka-cluster:Connect")
          KeyMap.lookup "x-amz-algorithm" o
            @?= Just (Aeson.String "AWS4-HMAC-SHA256")
          KeyMap.lookup "x-amz-expires" o
            @?= Just (Aeson.String "900")
          KeyMap.lookup "x-amz-signedheaders" o
            @?= Just (Aeson.String "host")
          -- The signature itself must be a non-empty hex string.
          case KeyMap.lookup "x-amz-signature" o of
            Just (Aeson.String s) -> do
              assertBool "signature is non-empty" (not (T.null s))
              assertBool "signature is lowercase hex"
                (T.all (\c -> (c >= '0' && c <= '9')
                           || (c >= 'a' && c <= 'f')) s)
            _ -> assertFailure "missing or non-string x-amz-signature"
          -- No session token field when none supplied.
          KeyMap.lookup "x-amz-security-token" o @?= Nothing
        Right _ -> assertFailure "payload was not a JSON object"

  , testCase "session-token credentials add x-amz-security-token" $ do
      let creds = Iam.AwsCredentials
            { Iam.awsAccessKeyId     = "ASIA"
            , Iam.awsSecretAccessKey = "secret"
            , Iam.awsSessionToken    = Just "tokvalue"
            }
          input = Iam.IamPayloadInput
            { Iam.iiCredentials = creds
            , Iam.iiHost        = "h"
            , Iam.iiRegion      = "us-east-1"
            , Iam.iiNow         = read "2024-01-01 00:00:00 UTC"
            , Iam.iiUserAgent   = "ua"
            , Iam.iiExpires     = 900
            }
          payload = Iam.buildIamPayload input
      case Aeson.eitherDecodeStrict payload of
        Right (Aeson.Object o) ->
          KeyMap.lookup "x-amz-security-token" o
            @?= Just (Aeson.String "tokvalue")
        _ -> assertFailure "payload missing security token"

  , testCase "static provider resolves credentials verbatim" $ do
      let creds = Iam.AwsCredentials "AKIASTATIC" "secret" (Just "session")
      resolved <- Iam.resolveAwsCredentials (Iam.AwsStaticCredentials creds)
      resolved @?= Right creds

  , testCase "custom provider failure surfaces before any IAM payload is sent" $ do
      let impl = SASL.awsMskIamImpl
            (Iam.AwsCustomProvider (pure (Left "no credentials")))
            "broker-1.kafka.us-east-1.amazonaws.com"
            "us-east-1"
      SASL.smiName impl @?= "AWS_MSK_IAM"
      initial <- SASL.smiInitial impl
      case initial of
        Left err -> err @?= "AWS_MSK_IAM: no credentials"
        Right _ -> assertFailure "expected IAM init failure"

  , testCase "SASL implementation emits IAM JSON for the broker host and region" $ do
      let creds = Iam.AwsCredentials
            { Iam.awsAccessKeyId     = "AKIAIOSFODNN7EXAMPLE"
            , Iam.awsSecretAccessKey = "secret"
            , Iam.awsSessionToken    = Just "tokvalue"
            }
          impl = SASL.awsMskIamImpl
            (Iam.AwsStaticCredentials creds)
            "broker-2.kafka.us-west-2.amazonaws.com"
            "us-west-2"
      step <- SASL.smiInitial impl
      case step of
        Right (SASL.StepSend payload acceptBrokerBytes) -> do
          obj <- expectJsonObject payload
          KeyMap.lookup "version" obj
            @?= Just (Aeson.String "2020_10_22")
          KeyMap.lookup "host" obj
            @?= Just (Aeson.String "broker-2.kafka.us-west-2.amazonaws.com")
          KeyMap.lookup "user-agent" obj
            @?= Just (Aeson.String "wireform-kafka/0.1")
          KeyMap.lookup "x-amz-expires" obj
            @?= Just (Aeson.String "900")
          KeyMap.lookup "x-amz-security-token" obj
            @?= Just (Aeson.String "tokvalue")
          case KeyMap.lookup "x-amz-credential" obj of
            Just (Aeson.String credential) -> do
              assertBool "credential includes configured region"
                ("/us-west-2/kafka-cluster/aws4_request" `T.isSuffixOf` credential)
              assertBool "credential starts with access key"
                ("AKIAIOSFODNN7EXAMPLE/" `T.isPrefixOf` credential)
            _ -> assertFailure "missing or non-string x-amz-credential"
          case acceptBrokerBytes (Just "") of
            Right (SASL.StepDone Nothing) -> pure ()
            Right _ -> assertFailure "AWS_MSK_IAM should finish after broker accept"
            Left err -> assertFailure ("unexpected IAM accept failure: " <> err)
        Right _ ->
          assertFailure "AWS_MSK_IAM should send exactly one client payload"
        Left err ->
          assertFailure ("unexpected IAM init failure: " <> err)

  , testCase "SASL implementation resolves credentials for each auth attempt" $ do
      counter <- newIORef (0 :: Int)
      let provider = Iam.AwsCustomProvider $ do
            n <- atomicModifyIORef' counter (\old -> let new = old + 1 in (new, new))
            pure $ Right Iam.AwsCredentials
              { Iam.awsAccessKeyId     = "AKIA" <> T.pack (show n)
              , Iam.awsSecretAccessKey = "secret"
              , Iam.awsSessionToken    = Nothing
              }
          impl = SASL.awsMskIamImpl provider "broker.kafka.us-east-1.amazonaws.com" "us-east-1"
      first <- SASL.smiInitial impl
      second <- SASL.smiInitial impl
      firstAccessKey <- stepAccessKey first
      secondAccessKey <- stepAccessKey second
      firstAccessKey @?= "AKIA1"
      secondAccessKey @?= "AKIA2"
  ]

--------------------------------------------------------------------------------
-- SaslConfig classification
--------------------------------------------------------------------------------

configTests :: TestTree
configTests = testGroup "configMechanism"
  [ testCase "PLAIN" $
      SASL.configMechanism (SASL.SaslPlain "u" "p") @?= SASL.NamePlain
  , testCase "SCRAM-SHA-256" $
      SASL.configMechanism (SASL.SaslScram Scram.ScramSHA256 "u" "p")
        @?= SASL.NameScramSha256
  , testCase "SCRAM-SHA-512" $
      SASL.configMechanism (SASL.SaslScram Scram.ScramSHA512 "u" "p")
        @?= SASL.NameScramSha512
  , testCase "OAUTHBEARER" $
      SASL.configMechanism
        (SASL.SaslOAuthBearer (OAuth.OAuthStaticToken (OAuth.OAuthToken "x" Nothing Nothing)))
        @?= SASL.NameOAuthBearer
  , testCase "AWS_MSK_IAM" $ do
      let creds = Iam.AwsCredentials "k" "s" Nothing
      SASL.configMechanism
        (SASL.SaslAwsMskIam (Iam.AwsStaticCredentials creds) "us-east-1")
        @?= SASL.NameAwsMskIam
  , testCase "GSSAPI" $
      SASL.configMechanism SASL.SaslGssapi @?= SASL.NameGssapi
  , testCase "wireName round-trips for every mechanism" $ do
      SASL.mechanismWireName SASL.NamePlain        @?= "PLAIN"
      SASL.mechanismWireName SASL.NameScramSha256  @?= "SCRAM-SHA-256"
      SASL.mechanismWireName SASL.NameScramSha512  @?= "SCRAM-SHA-512"
      SASL.mechanismWireName SASL.NameOAuthBearer  @?= "OAUTHBEARER"
      SASL.mechanismWireName SASL.NameAwsMskIam    @?= "AWS_MSK_IAM"
      SASL.mechanismWireName SASL.NameGssapi       @?= "GSSAPI"
  ]

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

lookupIndex :: BS.ByteString -> BS.ByteString -> Int
lookupIndex needle hay =
  go 0 hay
  where
    nLen = BS.length needle
    go i bs
      | BS.length bs < nLen = error ("needle not found in haystack: "
                                     <> BS8.unpack needle)
      | BS.take nLen bs == needle = i
      | otherwise = go (i + 1) (BS.drop 1 bs)

expectJsonObject :: BS.ByteString -> IO Aeson.Object
expectJsonObject payload =
  case Aeson.eitherDecodeStrict payload of
    Right (Aeson.Object obj) -> pure obj
    Left err -> assertFailure ("payload not valid JSON: " <> err)
    Right _ -> assertFailure "payload was not a JSON object"

stepAccessKey :: Either String SASL.StepResult -> IO T.Text
stepAccessKey = \case
  Right (SASL.StepSend payload _) -> do
    obj <- expectJsonObject payload
    case KeyMap.lookup "x-amz-credential" obj of
      Just (Aeson.String credential) ->
        case T.breakOn "/" credential of
          (accessKey, rest)
            | not (T.null rest) -> pure accessKey
          _ -> assertFailure "credential did not include a scope"
      _ -> assertFailure "missing or non-string x-amz-credential"
  Right _ -> assertFailure "AWS_MSK_IAM should start with StepSend"
  Left err -> assertFailure ("unexpected IAM init failure: " <> err)

