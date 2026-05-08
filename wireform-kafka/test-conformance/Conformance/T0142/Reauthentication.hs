{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Conformance.T0142.Reauthentication
Description : librdkafka @tests\/0142-reauthentication.c@

librdkafka's @0142-reauthentication@ exercises KIP-368 SASL session
re-authentication: the broker advertises a session lifetime in the
@SaslAuthenticateResponse@; the client must re-run the SASL
handshake before the session expires.

Our analogue: 'Kafka.Network.Auth.SASL' currently runs the
handshake once per connection; KIP-368 session re-auth is in
@docs\/PERFORMANCE.md@'s follow-up list. The conformance test here
is a *negative* test for now — we assert the bytes-level layer
(every mechanism builds its initial SASL payload) and document the
gap. When session-re-auth lands these become positive tests.
-}
module Conformance.T0142.Reauthentication (tests) where

import qualified Data.ByteString as BS

import Test.Tasty
import Test.Tasty.HUnit

import qualified Kafka.Network.Auth.AwsMskIam as Iam
import qualified Kafka.Network.Auth.OAuthBearer as OAuth
import qualified Kafka.Network.Auth.Plain as Plain
import qualified Kafka.Network.Auth.SASL as SASL
import qualified Kafka.Network.Auth.Scram as Scram

tests :: TestTree
tests = testGroup "0142-reauthentication"
  [ testCase "PLAIN payload bytes are deterministic across re-auth attempts" $ do
      Plain.generatePlainAuth "alice" "secret"
        @?= Plain.generatePlainAuth "alice" "secret"

  , testCase "SCRAM client-first nonces differ across sessions" $ do
      s1 <- Scram.newScramSession Scram.ScramSHA256 "alice" "secret"
      s2 <- Scram.newScramSession Scram.ScramSHA256 "alice" "secret"
      -- Two independent sessions must roll fresh nonces; otherwise
      -- a re-authentication could be replayed.
      assertBool "client nonces differ"
        (Scram.ssClientNonce s1 /= Scram.ssClientNonce s2)

  , testCase "OAUTHBEARER token can be rotated between sessions" $ do
      -- OAuth tokens rotate; the framing layer must accept any
      -- token bytes without caching across calls.
      let p1 = OAuth.buildOAuthPayload (OAuth.OAuthToken "tok-1" Nothing Nothing)
          p2 = OAuth.buildOAuthPayload (OAuth.OAuthToken "tok-2" Nothing Nothing)
      assertBool "rotated token produces different payload" (p1 /= p2)
      assertBool "old token still encodable later"
        (p1 == OAuth.buildOAuthPayload (OAuth.OAuthToken "tok-1" Nothing Nothing))

  , testCase "AWS MSK IAM payload re-resolves credentials each session" $ do
      let creds1 = Iam.AwsCredentials "AKIA-1" "s1" Nothing
          creds2 = Iam.AwsCredentials "AKIA-2" "s2" Nothing
          mk c = Iam.buildIamPayload Iam.IamPayloadInput
            { Iam.iiCredentials = c
            , Iam.iiHost        = "broker-1.example.com"
            , Iam.iiRegion      = "us-east-1"
            , Iam.iiNow         = read "2026-01-01 00:00:00 UTC"
            , Iam.iiUserAgent   = "wireform-conformance"
            , Iam.iiExpires     = 900
            }
      assertBool "rotated credentials produce different SigV4 signature"
        (mk creds1 /= mk creds2)

  , testCase "SaslConfig values are pure (no I/O at construction)" $ do
      let _ = SASL.SaslPlain "u" "p"
          _ = SASL.SaslScram Scram.ScramSHA512 "u" "p"
          _ = SASL.SaslOAuthBearer (OAuth.OAuthStaticToken
                  (OAuth.OAuthToken "t" Nothing Nothing))
          _ = SASL.SaslAwsMskIam
                  (Iam.AwsStaticCredentials (Iam.AwsCredentials "k" "s" Nothing))
                  "us-east-1"
          _ = BS.empty   -- keep BS import alive
      pure ()
  ]
