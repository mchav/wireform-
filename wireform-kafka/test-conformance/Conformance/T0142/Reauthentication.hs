{-# LANGUAGE OverloadedStrings #-}

{- |
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

import Data.ByteString qualified as BS
import Kafka.Network.Auth.AwsMskIam qualified as Iam
import Kafka.Network.Auth.OAuthBearer qualified as OAuth
import Kafka.Network.Auth.Plain qualified as Plain
import Kafka.Network.Auth.SASL qualified as SASL
import Kafka.Network.Auth.Scram qualified as Scram
import Test.Syd


tests :: Spec
tests =
  describe "0142-reauthentication" $
    sequence_
      [ it "PLAIN payload bytes are deterministic across re-auth attempts" $ do
          Plain.generatePlainAuth "alice" "secret"
            `shouldBe` Plain.generatePlainAuth "alice" "secret"
      , it "SCRAM client-first nonces differ across sessions" $ do
          s1 <- Scram.newScramSession Scram.ScramSHA256 "alice" "secret"
          s2 <- Scram.newScramSession Scram.ScramSHA256 "alice" "secret"
          -- Two independent sessions must roll fresh nonces; otherwise
          -- a re-authentication could be replayed.
          (Scram.ssClientNonce s1 /= Scram.ssClientNonce s2) `shouldBe` True
      , it "OAUTHBEARER token can be rotated between sessions" $ do
          -- OAuth tokens rotate; the framing layer must accept any
          -- token bytes without caching across calls.
          let p1 = OAuth.buildOAuthPayload (OAuth.OAuthToken "tok-1" Nothing Nothing)
              p2 = OAuth.buildOAuthPayload (OAuth.OAuthToken "tok-2" Nothing Nothing)
          (p1 /= p2) `shouldBe` True
          (p1 == OAuth.buildOAuthPayload (OAuth.OAuthToken "tok-1" Nothing Nothing)) `shouldBe` True
      , it "AWS MSK IAM payload re-resolves credentials each session" $ do
          let creds1 = Iam.AwsCredentials "AKIA-1" "s1" Nothing
              creds2 = Iam.AwsCredentials "AKIA-2" "s2" Nothing
              mk c =
                Iam.buildIamPayload
                  Iam.IamPayloadInput
                    { Iam.iiCredentials = c
                    , Iam.iiHost = "broker-1.example.com"
                    , Iam.iiRegion = "us-east-1"
                    , Iam.iiNow = read "2026-01-01 00:00:00 UTC"
                    , Iam.iiUserAgent = "wireform-conformance"
                    , Iam.iiExpires = 900
                    }
          (mk creds1 /= mk creds2) `shouldBe` True
      , it "SaslConfig values are pure (no I/O at construction)" $ do
          let _ = SASL.SaslPlain "u" "p"
              _ = SASL.SaslScram Scram.ScramSHA512 "u" "p"
              _ =
                SASL.SaslOAuthBearer
                  ( OAuth.OAuthStaticToken
                      (OAuth.OAuthToken "t" Nothing Nothing)
                  )
              _ =
                SASL.SaslAwsMskIam
                  (Iam.AwsStaticCredentials (Iam.AwsCredentials "k" "s" Nothing))
                  "us-east-1"
              _ = BS.empty -- keep BS import alive
          pure () :: IO ()
      ]
