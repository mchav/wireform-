{-# LANGUAGE OverloadedStrings #-}

-- | Tests for "Network.HTTP.Client.AuthChallenge".
--
-- The actual RFC 9110 \u00a711.4 challenge grammar is parsed by
-- hermes's 'credentialsParser'; these tests lock down the bridge
-- between hermes's 'Credentials' shape and the wireform
-- 'AuthChallenge' record.
module Test.AuthChallenge (tests) where

import qualified Data.CaseInsensitive as CI

import Test.Tasty
import Test.Tasty.HUnit

import Network.HTTP.Client.AuthChallenge

tests :: TestTree
tests = testGroup "Network.HTTP.Client.AuthChallenge"
  [ testGroup "parseAuthChallenges"
      [ testCase "Basic with quoted-string realm" $
          parseAuthChallenges "Basic realm=\"example\""
            @?= [AuthChallenge
                  { acScheme   = CI.mk "Basic"
                  , acParams   = [(CI.mk "realm", "example")]
                  , acToken68  = Nothing
                  }]
      , testCase "Bearer with multiple params" $
          parseAuthChallenges
            "Bearer realm=\"api\", error=\"invalid_token\""
            @?= [AuthChallenge
                  { acScheme  = CI.mk "Bearer"
                  , acParams  =
                      [ (CI.mk "realm", "api")
                      , (CI.mk "error", "invalid_token")
                      ]
                  , acToken68 = Nothing
                  }]
      , testCase "scheme with token68 (Negotiate)" $
          parseAuthChallenges "Negotiate ABCD1234+/="
            @?= [AuthChallenge
                  { acScheme  = CI.mk "Negotiate"
                  , acParams  = []
                  , acToken68 = Just "ABCD1234+/="
                  }]
      , testCase "scheme comparison is case-insensitive" $
          let [c] = parseAuthChallenges "basic realm=\"x\""
          in acScheme c @?= CI.mk "BASIC"
      , testCase "garbage input returns []" $
          parseAuthChallenges "" @?= []
      ]
  , testGroup "basicChallengeResponder"
      [ testCase "satisfies a Basic challenge with matching realm" $ do
          let resp = basicChallengeResponder
                       (\r -> if r == "example" then Just ("u","p") else Nothing)
              chs  = parseAuthChallenges "Basic realm=\"example\""
          out <- resp chs
          out @?= Just "Basic dTpw"  -- base64("u:p")
      , testCase "refuses when realm doesn't match" $ do
          let resp = basicChallengeResponder (const Nothing)
              chs  = parseAuthChallenges "Basic realm=\"example\""
          out <- resp chs
          out @?= Nothing
      , testCase "ignores non-Basic challenges" $ do
          let resp = basicChallengeResponder (\_ -> Just ("u","p"))
              chs  = parseAuthChallenges "Bearer realm=\"api\""
          out <- resp chs
          out @?= Nothing
      ]
  ]
