{-# LANGUAGE OverloadedStrings #-}

{- |
Tests for the RFC 6265bis ingest hardening added to
"Network.HTTP.Client.Cookies": Domain acceptance, @SameSite=None@
requires @Secure@, the @__Host-@ / @__Secure-@ name-prefix rules,
and the per-cookie size limit.
-}
module Test.Cookies (tests) where

import Data.ByteString qualified as BS
import Network.HTTP.Client.Cookies
import Test.Syd


baseCookie :: Cookie
baseCookie =
  Cookie
    { cookieName = "session"
    , cookieValue = "abc"
    , cookieDomain = "example.com"
    , cookieDomainExplicit = False
    , cookiePath = "/"
    , cookieExpires = Nothing
    , cookieSecure = True
    , cookieHttpOnly = False
    , cookieSameSite = SameSiteLax
    }


withJarThat :: (CookieJar -> IO a) -> IO a
withJarThat k = do
  jar <- newCookieJar
  k jar


tests :: Spec
tests =
  describe "Network.HTTP.Client.Cookies" $
    sequence_
      [ describe "validateCookieName / validateCookieValue" $
          sequence_
            [ it "valid token name accepted" $
                validateCookieName "session" `shouldBe` Right ()
            , it "empty name rejected" $
                validateCookieName "" `shouldBe` Left (CookieNameInvalid "")
            , it "name with whitespace rejected" $
                validateCookieName "bad name"
                  `shouldBe` Left (CookieNameInvalid "bad name")
            , it "valid cookie-octet value accepted" $
                validateCookieValue "opaque-token.123" `shouldBe` Right ()
            , it "value with comma rejected" $
                validateCookieValue "has,comma"
                  `shouldBe` Left (CookieValueInvalid "has,comma")
            ]
      , describe "insertCookieChecked" $
          sequence_
            [ it "SameSite=None without Secure is rejected" $ withJarThat $ \jar -> do
                let c =
                      baseCookie
                        { cookieSecure = False
                        , cookieSameSite = SameSiteNone
                        }
                r <- insertCookieChecked jar c
                r `shouldBe` Left (CookieSameSiteNoneRequiresSecure "session")
            , it "SameSite=None with Secure is accepted" $ withJarThat $ \jar -> do
                let c =
                      baseCookie
                        { cookieSecure = True
                        , cookieSameSite = SameSiteNone
                        }
                r <- insertCookieChecked jar c
                (r == Right ()) `shouldBe` True
            , it "__Secure- prefix without Secure flag is rejected" $ withJarThat $ \jar -> do
                let c =
                      baseCookie
                        { cookieName = "__Secure-token"
                        , cookieSecure = False
                        }
                r <- insertCookieChecked jar c
                r `shouldBe` Left (CookieSecurePrefixWithoutSecure "__Secure-token")
            , it "__Secure- prefix with Secure flag is accepted" $ withJarThat $ \jar -> do
                let c =
                      baseCookie
                        { cookieName = "__Secure-token"
                        , cookieSecure = True
                        }
                r <- insertCookieChecked jar c
                (r == Right ()) `shouldBe` True
            , it "__Host- prefix requires Secure + Path=/ + no Domain" $ withJarThat $ \jar -> do
                let c =
                      baseCookie
                        { cookieName = "__Host-id"
                        , cookieDomainExplicit = True
                        , cookieSecure = True
                        , cookiePath = "/"
                        }
                r <- insertCookieChecked jar c
                r `shouldBe` Left (CookieHostPrefixViolation "__Host-id")
            , it "__Host- with no Domain, Path=/, Secure is accepted" $ withJarThat $ \jar -> do
                let c =
                      baseCookie
                        { cookieName = "__Host-id"
                        , cookieDomainExplicit = False
                        , cookieSecure = True
                        , cookiePath = "/"
                        }
                r <- insertCookieChecked jar c
                (r == Right ()) `shouldBe` True
            , it "size limit rejects oversize cookies" $ withJarThat $ \jar -> do
                let c = baseCookie {cookieValue = BS.replicate 5000 0x78}
                r <- insertCookieChecked jar c
                case r of
                  Left CookieTooLarge {} -> pure ()
                  other -> error (show other)
            ]
      ]
