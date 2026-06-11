{-# LANGUAGE OverloadedStrings #-}

{- | Tests for "Network.HTTP.Client.AuthChallenge".

The actual RFC 9110 \u00a711.4 challenge grammar is parsed by
hermes's 'credentialsParser'; these tests lock down the bridge
between hermes's 'Credentials' shape and the wireform
'AuthChallenge' record.
-}
module Test.AuthChallenge (tests) where

import Data.CaseInsensitive qualified as CI
import Network.HTTP.Client.AuthChallenge
import Test.Syd


tests :: Spec
tests =
  describe "Network.HTTP.Client.AuthChallenge" $
    sequence_
      [ describe "parseAuthChallenges" $
          sequence_
            [ it "Basic with quoted-string realm" $
                parseAuthChallenges "Basic realm=\"example\""
                  `shouldBe` [ AuthChallenge
                                 { acScheme = CI.mk "Basic"
                                 , acParams = [(CI.mk "realm", "example")]
                                 , acToken68 = Nothing
                                 }
                             ]
            , it "Bearer with multiple params" $
                parseAuthChallenges
                  "Bearer realm=\"api\", error=\"invalid_token\""
                  `shouldBe` [ AuthChallenge
                                 { acScheme = CI.mk "Bearer"
                                 , acParams =
                                     [ (CI.mk "realm", "api")
                                     , (CI.mk "error", "invalid_token")
                                     ]
                                 , acToken68 = Nothing
                                 }
                             ]
            , it "scheme with token68 (Negotiate)" $
                parseAuthChallenges "Negotiate ABCD1234+/="
                  `shouldBe` [ AuthChallenge
                                 { acScheme = CI.mk "Negotiate"
                                 , acParams = []
                                 , acToken68 = Just "ABCD1234+/="
                                 }
                             ]
            , it "scheme comparison is case-insensitive" $
                let [c] = parseAuthChallenges "basic realm=\"x\""
                in acScheme c `shouldBe` CI.mk "BASIC"
            , it "garbage input returns []" $
                parseAuthChallenges "" `shouldBe` []
            ]
      , describe "basicChallengeResponder" $
          sequence_
            [ it "satisfies a Basic challenge with matching realm" $ do
                let resp =
                      basicChallengeResponder
                        (\r -> if r == "example" then Just ("u", "p") else Nothing)
                    chs = parseAuthChallenges "Basic realm=\"example\""
                out <- resp chs
                out `shouldBe` Just "Basic dTpw" -- base64("u:p")
            , it "refuses when realm doesn't match" $ do
                let resp = basicChallengeResponder (const Nothing)
                    chs = parseAuthChallenges "Basic realm=\"example\""
                out <- resp chs
                out `shouldBe` Nothing
            , it "ignores non-Basic challenges" $ do
                let resp = basicChallengeResponder (\_ -> Just ("u", "p"))
                    chs = parseAuthChallenges "Bearer realm=\"api\""
                out <- resp chs
                out `shouldBe` Nothing
            ]
      ]
